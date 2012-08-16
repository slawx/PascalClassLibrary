unit UDallasProgrammer;

{$mode delphi}

interface

uses
  Classes, SysUtils, USerialPort, UCommSerialPort, UCommPin, UCommMark,
  UJobProgressView, SyncObjs, DateUtils, Dialogs, URegistry,
  Forms, UISPProgrammer, Registry, UBinarySerializer, SpecializedList,
  UCommTelnet;

const
  Mark = #13#10;

type
  ETimeout = class(Exception);

  { TDallasProgrammer }

  TDallasProgrammer = class(TISPProgrammer)
  private
    FOnLogData: TOnLogDataEvent;
    Pin: TCommPin;
    CommMark: TCommMark;
    ResponseQueue: TListObject;
    ResponseLock: TCriticalSection;
    ResponseTemp: TBinarySerializer;
    SerialPortBackup: TCommSerialPort;
    SerialPortBackupPin: TCommPin;
    HexData: TStringList;
    Request: TBinarySerializer;
    Mark: TListByte;
    procedure ReceiveData(Sender: TCommPin; Stream: TListByte);
    function ReadResponse: string;
    function ResponseCount: Integer;
    procedure ResponseClear;
    procedure CheckErrorCode(Value: string);
  protected
    procedure SetActive(AValue: Boolean); override;
  public
    Timeout: TDateTime;
    Identification: string;
    BaudRate: Integer;
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
  SIdentification = 'Device identification';

{ TDallasProgrammer }

procedure TDallasProgrammer.ReceiveData(Sender: TCommPin; Stream: TListByte);
var
  OldPosition: Integer;
  NewList: TListByte;
begin
  try
    ResponseLock.Acquire;
    NewList := TListByte.Create;
    NewList.Assign(Stream);
    ResponseQueue.Add(NewList);
  finally
    ResponseLock.Release;
  end;
end;

function TDallasProgrammer.ReadResponse: string;
var
  Serializer: TBinarySerializer;
  StartTime: TDateTime;
  ElapsedTime: TDateTime;
begin
  StartTime := Now;
  repeat
    if ResponseCount > 0 then Break;
    ElapsedTime := Now - StartTime;
  until (ElapsedTime > Timeout);
  if ElapsedTime > Timeout then
    raise Exception.Create(STimeout);
  try
    ResponseLock.Acquire;
    Serializer := TBinarySerializer.Create;
    Serializer.List := TListByte(ResponseQueue.First);
    Result := Serializer.ReadString(Serializer.List.Count);
    ResponseQueue.Delete(0);
  finally
    Serializer.Free;
    ResponseLock.Release;
  end;
end;

function TDallasProgrammer.ResponseCount: Integer;
begin
  try
    ResponseLock.Acquire;
    Result := ResponseQueue.Count;
  finally
    ResponseLock.Release;
  end;
end;

procedure TDallasProgrammer.ResponseClear;
begin
  try
    ResponseLock.Acquire;
    ResponseQueue.Clear;
  finally
    ResponseLock.Release;
  end;
end;

procedure TDallasProgrammer.SetActive(AValue: Boolean);
var
  SerialPort: TCommSerialPort;
  Telnet: TCommTelnet;
begin
  if Active = AValue then Exit;
  inherited;
  if AValue then begin
    Request := TBinarySerializer.Create;
    Request.List := TListByte.Create;
    Request.OwnsList := True;
    HexData := TStringList.Create;

    if ExtPin.Connected and (ExtPin.RemotePin.Node is TCommSerialPort) then begin
      SerialPort := TCommSerialPort(ExtPin.RemotePin.Node);
      SerialPort.Active := False;
      SerialPortBackup.Assign(SerialPort);
      SerialPortBackupPin := SerialPort.Pin.RemotePin;
      SerialPort.Pin.Disconnect;

      //SerialPort.Name := SerialPort.Name;
      SerialPort.SerialPort.FlowControl := fcNone;
      SerialPort.SerialPort.BaudRate := BaudRate;
      SerialPort.SerialPort.DTR := True;
      SerialPort.Pin.Connect(CommMark.PinRaw);
      SerialPort.SerialPort.Flush;
      SerialPort.SerialPort.Purge;
      SerialPort.Active := True;
      if Assigned(FOnLogData) then
        Pin.OnLogData := FOnLogData;
    end;
    if ExtPin.Connected and (ExtPin.RemotePin.Node is TCommTelnet) then begin
      Telnet := TCommTelnet(ExtPin.RemotePin.Node);
    end;
    ResponseClear;
    CommMark.Active := True;
    ReadIdentification;
  end else begin
    CommMark.Active := False;
    if ExtPin.Connected and (ExtPin.RemotePin.Node is TCommSerialPort) then begin
      SerialPort := TCommSerialPort(ExtPin.Node);
      SerialPort.Active := False;
      SerialPort.Assign(SerialPortBackup);
      SerialPort.Pin.Connect(SerialPortBackupPin);
      SerialPort.Active := True;
    end;
    if ExtPin.Connected and (ExtPin.RemotePin.Node is TCommTelnet) then begin
      Telnet := TCommTelnet(ExtPin.RemotePin.Node);
    end;
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
      BaudRate := ReadIntegerWithDefault('FirmwareBaudRate', 57600);
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
  Pin.Send(Request.List);
  Value := ReadResponse;
  ReadResponse; // Empty line

  //HexFile.SaveToStringList(HexData);
  Job.Progress.Max := 65535 div 32;
  //Request.Size := 0;
  //ResponseClear;
  I := 0;
  repeat
    Value := ReadResponse;
    if Value <> '' then begin
      //Log(Value);
      HexData.Add(Value);
      if Value = ':00000001FF' then Break;
      Inc(I);
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
  Pin.Send(Request.List);
  ReadResponse;

  try
    CommMark.Mark.Clear;
    HexFile.SaveToStringList(HexData);
    Job.Progress.Max := HexData.Count;
    for I := 0 to HexData.Count - 1 do begin
      Request.Clear;
      ResponseClear;
      Request.WriteString(HexData[I]);
      Request.WriteList(Mark, 0, Mark.Count);
      Pin.Send(Request.List);
      Value := ReadResponse;
      CheckErrorCode(Value);
      Job.Progress.Value := I;
      if Job.Terminate then Break;
    end;
  finally
    CommMark.Mark.Assign(Mark);
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
  Pin.Send(Request.List);
  Value := ReadResponse;

  try
    CommMark.Mark.Clear;
    HexFile.SaveToStringList(HexData);
    Job.Progress.Max := HexData.Count;
    for I := 0 to HexData.Count - 1 do begin
      Request.Clear;
      ResponseClear;
      Request.WriteString(HexData[I]);
      Request.WriteList(Mark, 0, Mark.Count);
      Pin.Send(Request.List);
      Value := ReadResponse;
      CheckErrorCode(Value);
      Job.Progress.Value := I;
      if Job.Terminate then Break;
    end;
  finally
    CommMark.Mark.Assign(Mark);
  end;
end;

procedure TDallasProgrammer.Erase;
begin
  Active := True;
  Request.Clear;
  ResponseClear;
  Request.WriteByte(Ord('K'));
  Pin.Send(Request.List);
  ReadResponse;
end;

procedure TDallasProgrammer.Reset;
begin
end;

function TDallasProgrammer.ReadIdentification: string;
var
  InitTimeout: TDateTime;
  Value: string;
begin
  Result := '';
  InitTimeout := 6000 * OneMillisecond;
  Active := True;

  ResponseClear;
  Request.Clear;
  Pin.Send(Request.List);
  ReadResponse; // Empty line
  Identification := ReadResponse;
  Result := Identification;
  Log(SIdentification + ': ' + Identification);
end;

constructor TDallasProgrammer.Create;
begin
  inherited;
  Capabilities := [ipcErase, ipcRead, ipcWrite, ipcReset];
  Timeout := 3000 * OneMillisecond;
  ResponseQueue := TListObject.Create;
  ResponseLock := TCriticalSection.Create;
  ResponseTemp := TBinarySerializer.Create;
  ResponseTemp.List := TListByte.Create;
  ResponseTemp.OwnsList := True;
  Pin := TCommPin.Create;
  Pin.OnReceive := ReceiveData;
  Mark := TListByte.Create;
  Mark.SetArray([13, 10]);
  CommMark := TCommMark.Create(nil);
  CommMark.Mark.Assign(Mark);
  CommMark.PinFrame.Connect(Pin);
  BaudRate := 9600;
  SerialPortBackup := TCommSerialPort.Create(nil);
end;

destructor TDallasProgrammer.Destroy;
begin
  Active := False;
  Mark.Free;
  CommMark.Free;
  SerialPortBackup.Free;
  Pin.Free;
  ResponseQueue.Free;
  ResponseLock.Free;
  ResponseTemp.Free;
  inherited Destroy;
end;

end.

