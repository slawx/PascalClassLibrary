unit UPacketBurst;

{$mode Delphi}{$H+}

interface

uses
  Classes, UCommPin, SyncObjs, UCommon, SysUtils, SpecializedList,
  DateUtils, UBinarySerializer;

type
  TPacketBurst = class;

  TPacketBurstSendThread = class(TThread)
    PacketBurst: TPacketBurst;
    procedure Execute; override;
  end;

  { TPacketBurst }

  TPacketBurst = class(TCommNode)
  private
    SendThreadEvent: TEvent;
    SendThread: TPacketBurstSendThread;
    SendStreamLock: TCriticalSection;
    SendStream: TBinarySerializer;
    ReceiveStream: TBinarySerializer;
    procedure PacketSingleReceive(Sender: TCommPin; Stream: TListByte);
    procedure PacketBurstReceive(Sender: TCommPin; Stream: TListByte);
  protected
    procedure SetActive(const AValue: Boolean); override;
  public
    SendPeriod: Integer;
    SendBurstSize: Integer;
    PacketSinglePin: TCommPin;
    PacketBurstPin: TCommPin;
    destructor Destroy; override;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TSerialPort }

constructor TPacketBurst.Create(AOwner: TComponent);
begin
  inherited;
  PacketSinglePin := TCommPin.Create;
  PacketSinglePin.OnReceive := PacketSingleReceive;
  PacketSinglePin.Node := Self;
  PacketBurstPin := TCommPin.Create;
  PacketBurstPin.OnReceive := PacketBurstReceive;
  PacketBurstPin.Node := Self;
  SendThreadEvent := TSimpleEvent.Create;
  SendPeriod := 1;
end;

destructor TPacketBurst.Destroy;
begin
  Active := False;
  SendThreadEvent.Free;
  PacketSinglePin.Free;
  PacketBurstPin.Free;
  inherited;
end;

procedure TPacketBurst.PacketBurstReceive(Sender: TCommPin; Stream: TListByte);
var
  PacketStream: TListByte;
  Size: Word;
begin
  try
    PacketStream := TListByte.Create;
    ReceiveStream.Position := ReceiveStream.List.Count;
    ReceiveStream.WriteList(Stream, 0, Stream.Count);
    ReceiveStream.Position := 0;
    Size := ReceiveStream.ReadWord;
    while Size < ReceiveStream.List.Count do begin
      PacketStream.Count := Size;
      ReceiveStream.ReadList(PacketStream, 0, Size);
      PacketSinglePin.Send(PacketStream);
      Size := ReceiveStream.ReadWord;
    end;
  finally
    PacketStream.Free;
  end;
end;

procedure TPacketBurst.SetActive(const AValue: Boolean);
begin
  if FActive = AValue then Exit;
  FActive := AValue;
  if AValue then begin
    SendThread := TPacketBurstSendThread.Create(True);
    SendThread.FreeOnTerminate := False;
    SendThread.PacketBurst := Self;
    //SendThread.Name := 'PacketBurst';
    SendThread.Start;
  end else begin
    FreeAndNil(SendThread);
  end;
  inherited;
end;

procedure TPacketBurst.PacketSingleReceive(Sender: TCommPin; Stream: TListByte);
var
  SignalEvent: Boolean;
begin
  try
    SendStreamLock.Acquire;
    SendStream.WriteWord(Stream.Count);
    SendStream.WriteList(Stream, 0, Stream.Count);
    SignalEvent := SendStream.List.Count > SendBurstSize;
  finally
    SendStreamLock.Release;
  end;
  if SignalEvent then SendThreadEvent.SetEvent;
end;

{ TPacketBurstSendThread }

procedure TPacketBurstSendThread.Execute;
var
  Stream: TListByte;
begin
  try
    Stream := TListByte.Create;
    with PacketBurst do
    repeat
      if SendThreadEvent.WaitFor(SendPeriod) = wrSignaled then
      try
        SendStreamLock.Acquire;
        SendStream.Position := 0;
        if SendStream.List.Count < SendBurstSize then begin
          PacketBurstPin.Send(SendStream.List);
          SendStream.List.Count := 0;
        end else
        while (SendStream.List.Count - SendStream.Position) > SendBurstSize do begin
          Stream.Count := 0;
          SendStream.ReadList(Stream, 0, SendBurstSize);
          PacketBurstPin.Send(Stream);
        end;
      finally
        SendStreamLock.Release;
      end;
    until Terminated;
  finally
    Stream.Free;
  end;
end;

end.
