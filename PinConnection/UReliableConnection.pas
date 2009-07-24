unit UReliableConnection;

interface

uses
  CommPort, Classes, UCommon, UMemoryStreamEx, Dialogs, SysUtils,
  Windows, SyncObjs, Forms, Contnrs, UByteQueue, UPin;

type
  TPacketType = (ptInit, ptData, ptAcknowledge, ptNotAcknowledge, ptBufferSizeRequest, ptBufferSizeResponse);

  TReliableConnection = class;
  TPacketBuffer = class;

  TPacketBufferSendThread = class(TThread)
    Parent: TReliableConnection;
    ShowList: Integer;
    procedure Execute; override;
  end;

  TPacketBufferReceiveThread = class(TThread)
    Parent: TReliableConnection;
    procedure Execute; override;
  end;

  TPacketState = (psNew, psTransmited, psAcknowledged, psNotAcknowledged, psTimeout);

  TPacket = class
    State: TPacketState;
    RetransmitCount: Byte;
    Data: TMemoryStreamEx;
    Time: TDateTime;
    constructor Create;
    destructor Destroy; override;
  end;

  TPacketBuffer = class
  private
  public
    PacketList: TThreadList; // Tlist<TPacket>
    BufferSize: Word;
    Latency: Real;
    PacketCountMax: Word;
    PacketSequenceBottom: Word;
    function UsedPacketSize: Word;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
  end;

  TPacketBufferSend = class(TPacketBuffer)
  const
    MaxRetransmitCount = 3;
  var
    EventNewPacket: TEvent;
    PacketRetransmitCount: Integer;
    PacketTimeoutCount: Integer;
    RetransmitTimeout: Double;
    procedure Acknowledge(Sequence: Word);
    procedure NotAcknowledge(Sequence: Word);
    function AddPacket(PacketType: TPacketType; Stream: TStream = nil; OverLimit: Boolean = False): TPacket;
    constructor Create;
    destructor Destroy; override;
  end;

  TPacketBufferReceive = class(TPacketBuffer)
    EventNewPacket: TEvent;
    PacketDroppedCount: Integer;
    PacketDuplicateCount: Integer;
    function StorePacket(PacketType: TPacketType; Sequence: Integer; Stream: TStream = nil): TPacket;
    constructor Create;
    destructor Destroy; override;
  end;

  TReliableConnection = class
  private
    SendBuffer: TPacketBufferSend;
    ReceiveBuffer: TPacketBufferReceive;
    ReceiveBufferThread: TPacketBufferReceiveThread;
    SendBufferThread: TPacketBufferSendThread;
    FOnError: TNotifyEvent;
    function GetPacketDroppedCount: Integer;
    function GetPacketDuplicateCount: Integer;
    function GetPacketTimeoutCount: Integer;
    function GetReceiveBufferPacketCount: Integer;
    function GetSendBufferPacketCount: Integer;
  public
    FramePin: TPin;
    PacketPin: TPin;
    procedure Init;
    procedure FramePinReceive(Stream: TStream);
    procedure PacketPinReceive(Stream: TStream);
    constructor Create;
    destructor Destroy; override;
    property ReceiveBufferPacketCount: Integer read GetReceiveBufferPacketCount;
    property SendBufferPacketCount: Integer read GetSendBufferPacketCount;
    property PacketDroppedCount: Integer read GetPacketDroppedCount;
    property PacketDuplicateCount: Integer read GetPacketDuplicateCount;
    property PacketTimeoutCount: Integer read GetPacketTimeoutCount;
    property OnError: TNotifyEvent read FOnError write FOnError;
  end;

implementation


{ TPacketBuffer }

procedure TPacketBufferSend.Acknowledge(Sequence: Word);
var
  List: TList;
begin
  List := PacketList.LockList;
  if (Sequence >= PacketSequenceBottom) and (Sequence < (PacketSequenceBottom + List.Count)) then begin
    TPacket(List[Sequence - PacketSequenceBottom]).State := psAcknowledged;

    // Bottom acked packet can be removed from buffer
    while (List.Count > 0) and (TPacket(List[0]).State = psAcknowledged) do begin
      TPacket(List[0]).Free;
      List.Delete(0);
      Inc(PacketSequenceBottom);
    end;
  end;
  PacketList.UnlockList;
end;

constructor TPacketBufferReceive.Create;
begin
  inherited;
  EventNewPacket := TEvent.Create(nil, False, False, '');
end;

destructor TPacketBufferReceive.Destroy;
begin
  EventNewPacket.Free;
  inherited;
end;

function TPacketBufferReceive.StorePacket(PacketType: TPacketType; Sequence: Integer; Stream: TStream = nil): TPacket;
var
  List: TList;
  NewPacket: TPacket;
  Index: Integer;
  SignalEvent: Boolean;
begin
  SignalEvent := False;
  Result := nil;
  List := PacketList.LockList;
  Index := Sequence - PacketSequenceBottom;
  if (Sequence >= PacketSequenceBottom) and (Sequence < (PacketSequenceBottom + PacketCountMax)) then begin
    if List.Count <= Index then List.Count := Index + 1;
    if not Assigned(List[Index]) then begin
      NewPacket := TPacket.Create;
      with NewPacket do begin
        //Data.WriteByte(Integer(PacketType));
        //Data.WriteByte(Sequence - PacketSequenceBottom);
        if Assigned(Stream) then Data.WriteStream(Stream);
        Time := Now;
      end;
      List[Index] := NewPacket;
      Result := NewPacket;
      if Index = 0 then SignalEvent := True;
    end else Inc(PacketDuplicateCount);
  end else Inc(PacketDroppedCount);
  PacketList.UnlockList;
  if SignalEvent then EventNewPacket.SetEvent;
end;

procedure TPacketBuffer.Clear;
var
  I: Integer;
  List: TList;
begin
  PacketSequenceBottom := 0;
  List := PacketList.LockList;
  for I := 0 to List.Count - 1 do
    TPacket(List[I]).Free;
  List.Clear;
  PacketList.UnlockList;
end;

constructor TPacketBuffer.Create;
begin
  PacketList := TThreadList.Create;
end;

destructor TPacketBuffer.Destroy;
var
  I: Integer;
  List: TList;
begin
  List := PacketList.LockList;
  for I := 0 to List.Count - 1 do
    TPacket(List[I]).Free;
  PacketList.Destroy;
  inherited;
end;

procedure TPacketBufferSend.NotAcknowledge(Sequence: Word);
var
  List: TList;
begin
  List := PacketList.LockList;
  if (Sequence >= PacketSequenceBottom) and (Sequence < (PacketSequenceBottom + List.Count)) then begin
    TPacket(List[Sequence - PacketSequenceBottom]).State := psNotAcknowledged;

    // Bottom acked packet can be removed from buffer
    while (List.Count > 0) and (TPacket(List[0]).State = psNotAcknowledged) do begin
      TPacket(List[0]).Free;
      List.Delete(0);
      Inc(PacketSequenceBottom);
    end;
  end;
  PacketList.UnlockList;
end;

function TPacketBuffer.UsedPacketSize: Word;
var
  I: Integer;
  List: TList;
begin
  Result := 0;
  List := PacketList.LockList;
  for I := 0 to List.Count - 1 do
    Inc(Result, TPacket(List[I]).Data.Size);
  PacketList.UnlockList;
end;

{ TPacketBufferSendThread }

procedure TPacketBufferSendThread.Execute;
var
  List: TList;
  Stream: TMemoryStreamEx;
  I: Integer;
begin
  Stream := TMemoryStreamEx.Create;
  inherited;
  with Parent, SendBuffer do
  repeat
    if EventNewPacket.WaitFor(10) = wrSignaled then begin
      List := PacketList.LockList;
      //ShowList := Integer(List);
      //Synchronize(DoShow);
      I := 0;
      while I < List.Count do
      with TPacket(List[I]) do begin
        if (State = psNew) or ((State = psTransmited) and ((Now - Time) > RetransmitTimeout)) then begin
          Stream.Clear;
          with TPacket(List[0]) do begin
            Data.Position := 0;
            Stream.WriteStream(Data);
            Time := Now;
          end;
          //Stream.Show;
          PacketList.UnlockList;
          FramePin.Send(Stream);
          List := PacketList.LockList;
          if (State = psTransmited) then begin
            Inc(RetransmitCount);
            if RetransmitCount > MaxRetransmitCount then begin
              PacketList.UnlockList;
              NotAcknowledge(PacketSequenceBottom + I);
              Inc(PacketTimeoutCount);
              Dec(I);
              List := PacketList.LockList;
            end;
          end;
          State := psTransmited;
        end;
        Inc(I);
      end;
      PacketList.UnlockList;
    end;
  until Terminated;
  Stream.Free;
end;

{ TPacket }

constructor TPacket.Create;
begin
  Data := TMemoryStreamEx.Create;
  State := psNew;
  RetransmitCount := 0;
end;

destructor TPacket.Destroy;
begin
  Data.Free;
end;

{ TReliableConnection }

constructor TReliableConnection.Create;
begin
  FramePin := TPin.Create;
  FramePin.OnReceive := FramePinReceive;
  PacketPin := TPin.Create;
  PacketPin.OnReceive := PacketPinReceive;
  ReceiveBuffer := TPacketBufferReceive.Create;
  ReceiveBuffer.PacketCountMax := 40;
  SendBuffer := TPacketBufferSend.Create;
  SendBuffer.PacketCountMax := 40;
  ReceiveBufferThread := TPacketBufferReceiveThread.Create(True);
  ReceiveBufferThread.Parent := Self;
  ReceiveBufferThread.Resume;
  SendBufferThread := TPacketBufferSendThread.Create(True);
  SendBufferThread.Parent := Self;
  SendBufferThread.Resume;
end;

destructor TReliableConnection.Destroy;
begin
  ReceiveBufferThread.Free;
  SendBufferThread.Free;
  ReceiveBuffer.Free;
  SendBuffer.Free;
  FramePin.Free;
  PacketPin.Free;
  inherited;
end;

procedure TReliableConnection.FramePinReceive(Stream: TStream);
var
  PacketType: TPacketType;
  Sequence: Byte;
  Data: TMemoryStreamEx;
begin
  inherited;
  with TMemoryStreamEx(Stream), ReceiveBuffer do begin
    PacketType := TPacketType(ReadByte);
    Sequence := ReadByte;
    case PacketType of
      ptInit: begin
        Clear;
        SendBuffer.AddPacket(ptAcknowledge);
      end;
      ptData: begin
        SendBuffer.AddPacket(ptAcknowledge, nil, True);
        ReceiveBuffer.StorePacket(ptData, Sequence, Stream);
      end;
      ptAcknowledge: begin
        SendBuffer.Acknowledge(Sequence);
      end;
      ptNotAcknowledge: begin
        SendBuffer.NotAcknowledge(Sequence);
      end;
      ptBufferSizeRequest: begin
        Data := TMemoryStreamEx.Create;
        Data.WriteWord(PacketCountMax);
        Data.WriteWord(BufferSize);
        Data.Free;
        Data.Position := 0;
        SendBuffer.AddPacket(ptBufferSizeResponse, Data);
      end;
    end;
  end;
end;

function TReliableConnection.GetPacketDroppedCount: Integer;
begin
  Result := ReceiveBuffer.PacketDroppedCount;
end;

function TReliableConnection.GetPacketDuplicateCount: Integer;
begin
  Result := ReceiveBuffer.PacketDuplicateCount;
end;

function TReliableConnection.GetPacketTimeoutCount: Integer;
begin
  Result := SendBuffer.PacketTimeoutCount;
end;

function TReliableConnection.GetReceiveBufferPacketCount: Integer;
var
  List: TList;
begin
  List := ReceiveBuffer.PacketList.LockList;
  Result := List.Count;
  ReceiveBuffer.PacketList.UnlockList;
end;

function TReliableConnection.GetSendBufferPacketCount: Integer;
var
  List: TList;
begin
  List := SendBuffer.PacketList.LockList;
  Result := List.Count;
  SendBuffer.PacketList.UnlockList;
end;

procedure TReliableConnection.Init;
begin
  SendBuffer.AddPacket(ptInit);
end;

procedure TReliableConnection.PacketPinReceive(Stream: TStream);
begin
  inherited;
  with SendBuffer do begin
     AddPacket(ptData, Stream);
  end;
end;

{ TPacketBufferReceiveThread }

procedure TPacketBufferReceiveThread.Execute;
var
  List: TList;
  Stream: TMemoryStreamEx;
begin
  Stream := TMemoryStreamEx.Create;
  inherited;
  with Parent, ReceiveBuffer do
  repeat
    if EventNewPacket.WaitFor(100) = wrSignaled then begin
      List := PacketList.LockList;
      while (List.Count > 0) and (TPacket(List[0]).State = psNew) do begin
        Stream.Clear;
        TPacket(List[0]).Data.Position := 0;
        Stream.WriteStream(TPacket(List[0]).Data);
        PacketList.UnlockList;
        PacketPin.Send(Stream);
        List := PacketList.LockList;
        TPacket(List[0]).Free;
        List.Delete(0);
        Inc(PacketSequenceBottom);
      end;
      PacketList.UnlockList;
    end;
  until Terminated;
  Stream.Free;
end;

{ TPacketBufferSend }

function TPacketBufferSend.AddPacket(PacketType: TPacketType;
  Stream: TStream; OverLimit: Boolean): TPacket;
var
  List: TList;
  NewPacket: TPacket;
begin
  // Wait if buffer is full
  List := PacketList.LockList;
  if not OverLimit then
  while List.Count >= PacketCountMax do begin
    PacketList.UnlockList;
    Sleep(10);
    List := PacketList.LockList;
  end;

  NewPacket := TPacket.Create;
  with NewPacket do begin
    Data.WriteByte(Integer(PacketType));
    Data.WriteByte(PacketSequenceBottom + List.Count);
    if Assigned(Stream) then Data.WriteStream(Stream);
    Time := Now;
  end;
  List.Add(NewPacket);
  //ShowMessage(IntToStr(Integer(List)));
  Result := NewPacket;
  PacketList.UnlockList;
  //List := PacketList.LockList;
  //PacketList.UnlockList;
  EventNewPacket.SetEvent;
end;

constructor TPacketBufferSend.Create;
begin
  inherited;
  RetransmitTimeout := 1 / 24 / 3600;
  EventNewPacket := TEvent.Create(nil, False, False, '');
end;

destructor TPacketBufferSend.Destroy;
begin
  EventNewPacket.Free;
  inherited;
end;

end.
