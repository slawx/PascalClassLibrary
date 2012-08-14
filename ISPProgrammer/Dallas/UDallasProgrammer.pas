unit UDallasProgrammer;

{$mode delphi}

interface

uses
  Classes, SysUtils, USerialPort, UCommSerialPort, UCommPin,
  UJobProgressView, SyncObjs, DateUtils, Dialogs, URegistry,
  Forms, UISPProgrammer, Registry, UBinarySerializer, SpecializedList;

const
  NewLine = #$0D#$0A;

type
  ETimeout = class(Exception);

  { TDallasProgrammer }

  TDallasProgrammer = class(TISPProgrammer)
  private
    FOnLogData: TOnLogDataEvent;
    Pin: TCommPin;
    Response: TBinarySerializer; // should be thread safe
    ResponseLock: TCriticalSection;
    ResponseTemp: TBinarySerializer;
    ReceiveEvent: TEvent;
    SerialPortBackup: TCommSerialPort;
    SerialPortBackupPin: TCommPin;
    HexData: TStringList;
    Request: TBinarySerializer;
    StartTime: TDateTime;
    WaitResult: TWaitResult;
    procedure ReceiveData(Sender: TCommPin; Stream: TListByte);
    function WaitForString(EndString: string; Timeout: TDateTime): TWaitResult;
    procedure ResponseClear;
    procedure CheckErrorCode(Value: string);
  protected
    procedure SetActive(AValue: Boolean); override;
  public
    Timeout: TDateTime;
    Identification: string;
    BaudRate: TBaudRate;
    procedure LoadFromRegistry(Root: HKEY; Key: string); override;
    procedure SaveToRegistry(Root: HKEY; Key: string); override;
    procedure Read(Job: TJob); override;
    procedure Write(Job: TJob); override;
    procedure Erase; override;
    procedure Verify(Job: TJob); override;
    procedure Reset; override;
    function ReadIdentification: string; override;
    constructor Create; override;
    destructor Destroy; override;
    property OnLogData: TOnLogDataEvent read FOnLogData write FOnLogData;
  end;

implementation

resourcestring
  STimeout = 'Timeout';
  SEmptyBuffer = 'Empty buffer';
  SInvalidHexFormat = 'Invalid Intel Hex record format';
  SFlashControllerError = 'Flash controller error';
  SInvalidRecordAddress = 'Invalid address in Intel Hex record';
  SInvalidRecordLength = 'Invalid Intel Hex record length';
  SWriteFailure = 'Failure to write 1s to 0s during programming';
  SInvalidRecordType = 'Invalid Intel Hex record type';
  SInvalidRecordChecksum = 'Invalid checksum in Intel Hex record';
  SVerifyError = 'Verify Error';
  SInvalidResponse = 'Invalid response';
  SUnknownProgrammerResponse = 'Unknown flash programmer response "%s"';
  SIdentification = 'Identification';

{ TDallasProgrammer }

procedure TDallasProgrammer.ReceiveData(Sender: TCommPin; Stream: TListByte);
var
  OldPosition: Integer;
begin
  try
    ResponseLock.Acquire;
    OldPosition := Response.Position;
    Response.Position := Response.List.Count;
    Response.WriteList(Stream, 0, Stream.Count);
    Response.Position := OldPosition;
  finally
    ResponseLock.Release;
  end;
  ReceiveEvent.SetEvent;
end;

function TDallasProgrammer.WaitForString(EndString: string; Timeout: TDateTime): TWaitResult;
var
  StartTime: TDateTime;
  TimeoutPart: TDateTime;
  OldPosition: Integer;
  ElapsedTime: TDateTime;
begin
  StartTime := Now;
  repeat
    ElapsedTime := Now - StartTime;
    TimeoutPart := Timeout - ElapsedTime;
    if TimeoutPart < 0 then TimeoutPart := 0;
    Result := ReceiveEvent.WaitFor(Round(TimeOutPart / OneMillisecond));
    try
      ResponseLock.Acquire;
      OldPosition := Response.Position;
      Response.Position := 0;
      Response.ReadStringTerminated(EndString);
      if (Response.Position = 0) then begin
        Result := wrTimeout;
      end;
      Response.Position := OldPosition;
    finally
      ResponseLock.Release;
    end;
  until (Result = wrSignaled) or (ElapsedTime > Timeout);
end;

procedure TDallasProgrammer.ResponseClear;
begin
  try
    ResponseLock.Acquire;
    Response.Clear;
    ReceiveEvent.ResetEvent;
  finally
    ResponseLock.Release;
  end;
end;

procedure TDallasProgrammer.SetActive(AValue: Boolean);
begin
  if Active = AValue then Exit;
  inherited;
  if AValue then begin
    Request := TBinarySerializer.Create;
    Request.List := TListByte.Create;
    Request.OwnsList := True;
    HexData := TStringList.Create;

    SerialPort.Active := False;
    SerialPortBackup.Assign(SerialPort);
    SerialPortBackupPin := SerialPort.Pin.RemotePin;
    SerialPort.Pin.Disconnect;

    //SerialPort.Name := SerialPort.Name;
    SerialPort.FlowControl := fcNone;
    SerialPort.BaudRate := BaudRate;
    SerialPort.DTR := True;
    SerialPort.Pin.Connect(Pin);
    SerialPort.Flush;
    SerialPort.Purge;
    ResponseClear;
    SerialPort.Active := True;
    if Assigned(FOnLogData) then
      Pin.OnLogData := FOnLogData;

    ReadIdentification;
  end else begin
    SerialPort.Active := False;
    SerialPort.Assign(SerialPortBackup);
    SerialPort.Pin.Connect(SerialPortBackupPin);
    SerialPort.Active := True;
    HexData.Free;
    Request.Free;
  end;
end;

procedure TDallasProgrammer.CheckErrorCode(Value: string);
begin
  if Value = 'H' then raise Exception.Create(SInvalidHexFormat)
  else if Value = 'F' then raise Exception.Create(SFlashControllerError)
  else if Value = 'A' then raise Exception.Create(SInvalidRecordAddress)
  else if Value = 'L' then raise Exception.Create(SInvalidRecordLength)
  else if Value = 'P' then raise Exception.Create(SWriteFailure)
  else if Value = 'R' then raise Exception.Create(SInvalidRecordType)
  else if Value = 'S' then raise Exception.Create(SInvalidRecordChecksum)
  else if Value = 'V' then raise Exception.Create(SVerifyError)
  else if Value = '' then raise Exception.Create(SInvalidResponse)
  else if Value <> 'G' then raise Exception.Create(Format(SUnknownProgrammerResponse, [Value]));
end;

procedure TDallasProgrammer.LoadFromRegistry(Root: HKEY; Key: string);
begin
  with TRegistryEx.Create do
    try
      RootKey := Root;
      OpenKey(Key + '\ISPProgrammer\Dallas', True);
      BaudRate := TBaudRate(ReadIntegerWithDefault('FirmwareBaudRate', Integer(br57600)));
    finally
      Free;
    end;
end;

procedure TDallasProgrammer.SaveToRegistry(Root: HKEY; Key: string);
begin
  with TRegistryEx.Create do
    try
      RootKey := Root;
      OpenKey(Key + '\ISPProgrammer\Dallas', True);
      WriteInteger('FirmwareBaudRate', Integer(BaudRate));
    finally
      Free;
    end;
end;

procedure TDallasProgrammer.Read(Job: TJob);
var
  Value: string;
  I: Integer;
begin
  Active := True;

  Request.List.Count := 0;
  ResponseClear;
  Request.WriteByte(Ord('D'));
  Request.WriteByte($0D);
  Pin.Send(Request.List);
  if WaitForString(NewLine, Timeout) <> wrSignaled then begin
    raise Exception.Create(STimeout);
  end;
  Response.ReadStringTerminated(NewLine); // D
  try
    ResponseLock.Acquire;
    Response.List.DeleteItems(0, Response.Position);
    Response.Position := 0;
  finally
    ResponseLock.Release;
  end;

  //HexFile.SaveToStringList(HexData);
  Job.Progress.Max := 65535 div 32;
  //Request.Size := 0;
  //ResponseClear;
  I := 0;
  repeat
    //Request.WriteString(HexData[I]);
    //Request.WriteByte($0D);
    //Pin.Send(Request);
    if WaitForString(NewLine, Timeout) <> wrSignaled then
      raise Exception.Create(STimeout);

    //if ReceiveEvent.WaitFor(Round(Timeout / OneMillisecond)) <> wrSignaled then
    //  raise Exception.Create(STimeout);
    try
      ResponseLock.Acquire;
      //Response.Position := 0;
      //if Response.Size = 0 then
      //  raise Exception.Create(SEmptyBuffer);
      Value := Response.ReadStringTerminated(NewLine);
      if Value <> '' then begin
        Response.List.DeleteItems(0, Response.Position);
        Response.Position := 0;
        //Log(Value);
        HexData.Add(Value);
        //Response.Size := 0;
        if Value = ':00000001FF' then Break;
        Inc(I);
      end;
    finally
      ResponseLock.Release;
    end;
    Job.Progress.Value := I;
    if Job.Terminate then Break;
  until False;
  //for I := 0 to 10 do //HexData.Count - 1 do
  //  Log(HexData[I]);
  if not Job.Terminate then
    HexFile.LoadFromStringList(HexData);
end;

procedure TDallasProgrammer.Verify(Job: TJob);
var
  Value: string;
  I: Integer;
begin
  Active := True;

  Request.List.Count := 0;
  ResponseClear;
  Request.WriteByte(Ord('V'));
  Request.WriteByte($0D);
  Pin.Send(Request.List);
  if WaitForString(NewLine, Timeout) <> wrSignaled then begin
    Pin.Send(Request.List);
    raise Exception.Create(STimeout);
  end;
  HexFile.SaveToStringList(HexData);
  Job.Progress.Max := HexData.Count;
  for I := 0 to HexData.Count - 1 do begin
    Request.List.Count := 0;
    ResponseClear;
    Request.WriteString(HexData[I]);
    Request.WriteByte($0D);
    Pin.Send(Request.List);
    if ReceiveEvent.WaitFor(Round(Timeout / OneMillisecond)) <> wrSignaled then
      raise Exception.Create(STimeout);
    try
      ResponseLock.Acquire;
      Response.Position := 0;
      if Response.List.Count = 0 then
        raise Exception.Create(SEmptyBuffer);
      Value := Chr(Response.ReadByte);
    finally
      ResponseLock.Release;
    end;
    CheckErrorCode(Value);
    Job.Progress.Value := I;
    if Job.Terminate then Break;
  end;
end;

procedure TDallasProgrammer.Write(Job: TJob);
var
  Value: string;
  I: Integer;
begin
  Active := True;
  Request.Clear;
  ResponseClear;
  Request.WriteByte(Ord('L'));
  Request.WriteByte($0D);
  Pin.Send(Request.List);
  if WaitForString(NewLine, Timeout) <> wrSignaled then begin
    Pin.Send(Request.List);
    raise Exception.Create(STimeout);
  end;
  HexFile.SaveToStringList(HexData);
  Job.Progress.Max := HexData.Count;
  for I := 0 to HexData.Count - 1 do begin
    Request.List.Count := 0;
    ResponseClear;
    Request.WriteString(HexData[I]);
    Request.WriteByte($0D);
    Pin.Send(Request.List);
    if ReceiveEvent.WaitFor(Round(Timeout / OneMillisecond)) <> wrSignaled then
      raise Exception.Create(STimeout);
    try
      ResponseLock.Acquire;
      Response.Position := 0;
      if Response.List.Count = 0 then
        raise Exception.Create(SEmptyBuffer);
      Value := Chr(Response.ReadByte);
    finally
      ResponseLock.Release;
    end;
    CheckErrorCode(Value);
    Job.Progress.Value := I;
    if Job.Terminate then Break;
  end;
end;

procedure TDallasProgrammer.Erase;
begin
  Active := True;
  Request.Clear;
  ResponseClear;
  Request.WriteByte(Ord('K'));
  Request.WriteByte($0D);
  Pin.Send(Request.List);
  if WaitForString('>', Timeout) <> wrSignaled then
    raise Exception.Create(STimeout);
end;

procedure TDallasProgrammer.Reset;
begin
end;

function TDallasProgrammer.ReadIdentification: string;
var
  InitTimeout: TDateTime;
begin
  Result := '';
  InitTimeout := 6000 * OneMillisecond;

  Active := True;

  // Init and read identification
  StartTime := Now;
  repeat
    ResponseClear;
    Request.List.Count := 0;
    Request.WriteByte($0D);
    Pin.Send(Request.List);
    WaitResult := WaitForString('>', Timeout);
  until (WaitResult = wrSignaled) or ((Now - StartTime) > InitTimeout);
  if WaitResult <> wrSignaled then
    raise Exception.Create(STimeout);
  try
    ResponseLock.Acquire;
    Response.Position := 0;
    Response.ReadStringTerminated(NewLine);
    Identification := Response.ReadStringTerminated(NewLine);
    Result := Identification;
    Log(SIdentification + ': ' + Identification);
  finally
    ResponseLock.Release;
  end;
end;

constructor TDallasProgrammer.Create;
begin
  inherited;
  Capabilities := [ipcErase, ipcRead, ipcWrite, ipcReset];
  Timeout := 3000 * OneMillisecond;
  ReceiveEvent := TSimpleEvent.Create;
  Response := TBinarySerializer.Create;
  Response.List := TListByte.Create;
  Response.OwnsList := True;
  ResponseLock := TCriticalSection.Create;
  ResponseTemp := TBinarySerializer.Create;
  ResponseTemp.List := TListByte.Create;
  ResponseTemp.OwnsList := True;
  Pin := TCommPin.Create;
  Pin.OnReceive := ReceiveData;
  BaudRate := br9600;
  SerialPortBackup := TCommSerialPort.Create;
end;

destructor TDallasProgrammer.Destroy;
begin
  Active := False;
  SerialPortBackup.Free;
  Pin.Free;
  Response.Free;
  ResponseLock.Free;
  ResponseTemp.Free;
  ReceiveEvent.Free;
  inherited Destroy;
end;

end.

