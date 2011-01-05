unit UCommProtocol;

{$mode delphi}

interface

uses
  Classes, SysUtils, UVarBlockSerializer, syncobjs, UCommPin,
  UDebugLog, UStreamHelper, StopWatch, SpecializedList;

type
  ECommResponseCodeError = class(Exception);
  ECommTimeout = class(Exception);
  ECommError = class(Exception);

  TResponseError = (rcNone, rcCommandNotSupported, rcSequenceOutOfRange,
    rcEWrongParameters, rcVarIntDecode, rcDropped);
  TMessageType = (mtNone, mtRequest, mtResponse);

  TCommProtocol = class;

  { TDeviceProtocolSession }

  TDeviceProtocolSession = class
  private
    RepeatCounter: integer;
    ReceiveEvent: TEvent;
    Request: TStreamHelper;
    ResponseParameters: TVarBlockIndexed;
    TransmitTime: TDateTime;
    StopWatch: TStopWatch;
  public
    Lock: TCriticalSection;
    SequenceNumber: Integer;
    ResponseCode: Integer;
    CommandError: Integer;
    RaiseError: Boolean;
    Timeouted: Boolean;
    CommandIndex: TListInteger;
    Latency: Double;
    constructor Create;
    destructor Destroy; override;
  end;

  { TDeviceProtocolSessionList }

  TDeviceProtocolSessionList = class(TListObject)
    SequenceNumber: integer;
    Parent: TCommProtocol;
    Lock: TCriticalSection;
    function GetFree: TDeviceProtocolSession;
    function GetBySequence(Sequence: integer): TDeviceProtocolSession;
    procedure Remove(Session: TDeviceProtocolSession);
    constructor Create;
    destructor Destroy; override;
  end;

  TAfterRequest = procedure(Command: TListInteger; Parameters: TVarBlockIndexed;
    Result: TVarBlockIndexed; var ResponseError: TResponseError;
    var CommandError: integer) of object;

  { TRetransmitCheckThread }

  TRetransmitCheckThread = class(TThread)
  private
  public
    Parent: TCommProtocol;
    CheckPeriod: Integer;
    procedure Execute; override;
  end;

  { TCommProtocol }

  TCommProtocol = class
  private
    FActive: Boolean;
    FOnAfterRequest: TAfterRequest;
    FOnCommand: TAfterRequest;
    FOnDebugLog: TDebugLogAddEvent;
    OnAfterRequest: TAfterRequest;
    RetransmitThread: TRetransmitCheckThread;
    procedure DataReceive(Sender: TCommPin; Stream: TStream);
    procedure HandleRequest(Stream: TStream);
    procedure SetActive(const AValue: Boolean);
  public
    RetransmitTimeout: TDateTime;
    RetransmitRepeatCount: integer;
    RetransmitTotalCount: integer;
    MaxSequenceNumber: integer;
    MaxSessionCount: integer;
    RemoteBufferSize: Integer;
    RemoteBufferUsed: Integer;
    WrongSequenceCount: integer;
    Sessions: TDeviceProtocolSessionList;
    Pin: TCommPin;
    LastCommandResponseTime: TDateTime;
    LastLatency: Double;
    procedure SendCommand(Command: array of integer;
      ResponseParameters: TVarBlockIndexed = nil;
      RequestParameters: TVarBlockIndexed = nil; ARaiseError: boolean = True);
    constructor Create;
    destructor Destroy; override;
    property OnAfterRequest: TAfterRequest read FOnAfterRequest write FOnAfterRequest;
    property OnCommand: TAfterRequest read FOnCommand write FOnCommand;
    property OnDebugLog: TDebugLogAddEvent read FOnDebugLog write FOnDebugLog;
    property Active: Boolean read FActive write SetActive;
  end;

implementation

resourcestring
  SResponseError = 'Command %s response error %s';
  SResponseTimeout = 'Response timeout';
  SWrongSequenceCount = 'Receive wrong sequence number %d';
  SDeviceProtocol = 'Device protocol';
  SProtocolDecodeError = 'Data decode error';

procedure TCommProtocol.DataReceive(Sender: TCommPin; Stream: TStream);
var
  ResponseSequenceNumber: Integer;
  Session: TDeviceProtocolSession;
  MessageType: Integer;
  Request: TVarBlockIndexed;
  TempStream: TMemoryStream;
begin
  TempStream := TMemoryStream.Create;
  Request := TVarBlockIndexed.Create;
  Request.Enclose := False;
  with Request do try
    try
      ReadFromStream(Stream);
      if TestIndex(0) then
        MessageType := ReadVarUInt(0);
      if MessageType = Integer(mtResponse) then begin
        if TestIndex(1) then begin
          ResponseSequenceNumber := ReadVarUInt(1);
          try
            Sessions.Lock.Acquire;
            Session := Sessions.GetBySequence(ResponseSequenceNumber);
            if Assigned(Session) then begin
              with Session do try
                Session.Lock.Acquire;
                if TestIndex(2) and Assigned(ResponseParameters) then begin
                  ReadVarStream(2, TempStream);
                  ResponseParameters.Enclose := False;
                  ResponseParameters.ReadFromStream(TempStream);
                end;
                if TestIndex(3) then ResponseCode := ReadVarUInt(3)
                  else ResponseCode := 0;
                if TestIndex(4) then CommandError := ReadVarUInt(4)
                  else CommandError := 0;
                StopWatch.Stop;
                Latency := StopWatch.ElapsedMiliseconds;
                ReceiveEvent.SetEvent;
              finally
                Session.Lock.Release;
              end;
            end else begin
              Inc(WrongSequenceCount);
              if Assigned(FOnDebugLog) then
                FOnDebugLog(SDeviceProtocol, Format(SWrongSequenceCount, [ResponseSequenceNumber]));
            end;
          finally
            Sessions.Lock.Release;
          end;
        end;
      end else
      if MessageType = Integer(mtRequest) then HandleRequest(Stream);
    except
      on EReadError do begin
        if Assigned(FOnDebugLog) then
          FOnDebugLog(SDeviceProtocol, SProtocolDecodeError);
      end;
    end;
  finally
    TempStream.Free;
    Free;
  end;
end;

procedure TCommProtocol.HandleRequest(Stream: TStream);
var
  ResponseCode: TResponseError;
  Response: TVarBlockIndexed;
  ResponseData: TVarBlockIndexed;
  RequestData: TVarBlockIndexed;
  CommandIndex: TListInteger;
  CommandStream: TVarBlockIndexed;
  SequenceNumber: integer;
  CommandError: integer;
  MessageType: integer;
  Command: TVarBlockIndexed;
  TempStream: TMemoryStream;
begin
  try
    TempStream := TMemoryStream.Create;
    Command := TVarBlockIndexed.Create;
    CommandIndex := TListInteger.Create;
    Response := TVarBlockIndexed.Create;
    ResponseData := TVarBlockIndexed.Create;
    CommandStream := TVarBlockIndexed.Create;
    RequestData := TVarBlockIndexed.Create;
    ResponseCode := rcNone;
    Command.Enclose := False;
    Command.ReadFromStream(Stream);
    with Command do begin
      if TestIndex(0) then
        MessageType := ReadVarUInt(0);
      if TestIndex(1) then
        SequenceNumber := ReadVarUInt(1);
      if TestIndex(2) then
        ReadVarIntegerArray(2, CommandIndex);
      if TestIndex(3) then
        ReadVarIndexedBlock(3, RequestData);
      if Assigned(FOnCommand) then
        FOnCommand(CommandIndex, RequestData, ResponseData, ResponseCode, CommandError)
      else
        ResponseCode := rcCommandNotSupported;
    end;
    with Response do begin
      Enclose := False;
      WriteVarUInt(0, Integer(mtResponse));
      WriteVarUInt(1, Integer(SequenceNumber));
      if ResponseData.Items.Count > 0 then
        WriteVarIndexedBlock(2, ResponseData);
      WriteVarUInt(3, Integer(ResponseCode));
      WriteVarUInt(4, Integer(CommandError));
      WriteToStream(TempStream);
      if ResponseCode <> rcDropped then
        Pin.Send(TempStream);
    end;

    if Assigned(FOnAfterRequest) then
      FOnAfterRequest(CommandIndex, RequestData, ResponseData,
        ResponseCode, CommandError);
  finally
    TempStream.Free;
    RequestData.Free;
    CommandStream.Free;
    ResponseData.Free;
    Response.Free;
    Command.Free;
    CommandIndex.Free;
  end;
end;

procedure TCommProtocol.SetActive(const AValue: Boolean);
begin
  if FActive = AValue then Exit;
  FActive := AValue;
  if AValue then begin
    RetransmitThread := TRetransmitCheckThread.Create(True);
    with RetransmitThread do begin
      CheckPeriod := 100;
      Parent := Self;
      FreeOnTerminate := False;
      Start;
    end;
  end else begin
    FreeAndNil(RetransmitThread);
  end;
end;

procedure TCommProtocol.SendCommand(Command: array of integer;
  ResponseParameters: TVarBlockIndexed = nil; RequestParameters: TVarBlockIndexed = nil;
  ARaiseError: boolean = True);
var
  Session: TDeviceProtocolSession;
  NewRequest: TVarBlockIndexed;
begin
  Session := Sessions.GetFree;
  try
    NewRequest := TVarBlockIndexed.Create;

    Session.ResponseParameters := ResponseParameters;
    with Session do begin
      try
        Session.Lock.Acquire;
        CommandIndex.Clear;
        CommandIndex.AddArray(Command);
        with NewRequest do begin
          Enclose := False;
          WriteVarUInt(0, Integer(mtRequest));
          WriteVarUInt(1, SequenceNumber);
          WriteVarIntegerArray(2, CommandIndex);
          if Assigned(RequestParameters) then
            WriteVarIndexedBlock(3, RequestParameters);
        end;
        RaiseError := ARaiseError;
        NewRequest.WriteToStream(Request);

        // Wait for free remote buffer
        repeat
          Sleep(1);
        until (RemoteBufferUsed + Request.Size) <= RemoteBufferSize;

        StopWatch.Start;
        TransmitTime := Now;
        Pin.Send(Request);
      finally
        Lock.Release;
      end;
      try
        RemoteBufferUsed := RemoteBufferUsed + Request.Size;
        while ReceiveEvent.WaitFor(10) = wrTimeout do begin
          if Timeouted then
            raise ECommTimeout.Create(SResponseTimeout);
        end;
        if ResponseCode <> Integer(rcNone) then begin
          if Assigned(FOnDebugLog) then
            FOnDebugLog(SDeviceProtocol, Format(SResponseError, [CommandIndex.Implode('.', IntToStr), IntToStr(ResponseCode)]));
          raise ECommResponseCodeError.Create(Format(SResponseError, [CommandIndex.Implode('.', IntToStr), IntToStr(ResponseCode)]));
        end;
        LastCommandResponseTime := Now;
        LastLatency := Latency;
      finally
        RemoteBufferUsed := RemoteBufferUsed - Session.Request.Size;
        if RemoteBufferUsed < 0 then RemoteBufferUsed := 0;
        Sessions.Remove(Session);
      end;
    end;
  finally
    NewRequest.Free;
  end;
end;

constructor TCommProtocol.Create;
begin
  Pin := TCommPin.Create;
  Pin.OnReceive := DataReceive;
  Sessions := TDeviceProtocolSessionList.Create;
  Sessions.Parent := Self;
  MaxSessionCount := 10;
  MaxSequenceNumber := 127;
  RetransmitTimeout := 1 / 24 / 3600 * 1;
  RetransmitRepeatCount := 3;
  RetransmitTotalCount := 0;
  RemoteBufferSize := 127;
end;

destructor TCommProtocol.Destroy;
begin
  Active := False;
  Sessions.Free;
  Pin.Free;
  inherited Destroy;
end;

{ TDeviceProtocolSession }

constructor TDeviceProtocolSession.Create;
begin
  StopWatch := TStopWatch.Create;
  ResponseCode := 0;
  Lock := TCriticalSection.Create;
  ReceiveEvent := TEvent.Create(nil, False, False, '');
  Request := TStreamHelper.Create;
  ResponseParameters := nil;
  CommandIndex := TListInteger.Create;
  Latency := 0;
end;

destructor TDeviceProtocolSession.Destroy;
begin
  CommandIndex.Free;
  Request.Free;
  ReceiveEvent.Free;
  Lock.Free;
  StopWatch.Free;
  inherited Destroy;
end;

{ TDeviceProtocolSessionList }

function TDeviceProtocolSessionList.GetFree: TDeviceProtocolSession;
begin
  // Block if no free session available
  Lock.Acquire;
  try
    while Count >= Parent.MaxSessionCount do
    begin
      Lock.Release;
      Sleep(1);
      Lock.Acquire;
    end;
    Result := TDeviceProtocolSession.Create;
    Result.SequenceNumber := SequenceNumber;
    Inc(SequenceNumber);
    if SequenceNumber > Parent.MaxSequenceNumber then
      SequenceNumber := 0;
    Add(Result);
  finally
    Lock.Release;
  end;
end;

function TDeviceProtocolSessionList.GetBySequence(Sequence: integer):
TDeviceProtocolSession;
var
  I: integer;
begin
  I := 0;
  while (I < Count) and (TDeviceProtocolSession(Items[I]).SequenceNumber <> Sequence) do
    Inc(I);
  if I < Count then
    Result := TDeviceProtocolSession(Items[I])
  else
    Result := nil;
end;

procedure TDeviceProtocolSessionList.Remove(Session: TDeviceProtocolSession);
begin
  try
    Lock.Acquire;
    inherited Remove(TObject(Session));
  finally
    Lock.Release;
  end;
end;

constructor TDeviceProtocolSessionList.Create;
begin
  inherited Create;
  Lock := TCriticalSection.Create;
end;

destructor TDeviceProtocolSessionList.Destroy;
begin
  // Free session list before freeing Lock
  // instead of freeing in inherited Destroy in TListObject
  Lock.Acquire;
  Clear;
  Lock.Release;

  Lock.Free;
  inherited Destroy;
end;

{ TRetransmitCheckThread }

procedure TRetransmitCheckThread.Execute;
var
  I: integer;
begin
  with Parent do repeat
      try
        try
          Parent.Sessions.Lock.Acquire;
          for I := 0 to Sessions.Count - 1 do begin
          with TDeviceProtocolSession(Sessions[I]) do begin
              try
                Lock.Acquire;
                if Now > (TransmitTime + RetransmitTimeout) then begin
                  if RepeatCounter < RetransmitRepeatCount then begin
                    Pin.Send(Request);
                    StopWatch.Start;
                    TransmitTime := Now;
                    Inc(RepeatCounter);
                    Inc(RetransmitTotalCount);
                  end else
                    Timeouted := True;
                end;
              finally
                Lock.Release;
              end;
            end;
          end;
        finally
          Parent.Sessions.Lock.Release;
        end;

      if not Terminated then
        Sleep(CheckPeriod);
      except
        on E: Exception do begin
          //MainForm.ExceptionLogger1.ThreadExceptionHandler(Self, E);
        end;
      end;
    until Terminated;
end;

end.

