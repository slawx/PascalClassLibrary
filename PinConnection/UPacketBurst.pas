unit UPacketBurst;

interface

uses
  Classes, CommPort, UPin, UMemoryStreamEx, SyncObjs;

type
  TPacketBurst = class;

  TPacketBurstSendThread = class(TThread)
    PacketBurst: TPacketBurst;
    procedure Execute; override;
  end;

  TPacketBurst = class
  private
    SendThreadEvent: TEvent;
    SendThread: TPacketBurstSendThread;
    SendStreamLock: TCriticalSection;
    SendStream: TMemoryStreamEx;
    ReceiveStream: TMemoryStreamEx;
    procedure PacketSingleReceive(Stream: TStream);
    procedure PacketBurstReceive(Stream: TStream);
  public
    SendPeriod: Integer; // ms
    SendBurstSize: Integer;
    PacketSinglePin: TPin;
    PacketBurstPin: TPin;
    destructor Destroy; override;
    constructor Create;
  end;

implementation

{ TSerialPort }

constructor TPacketBurst.Create;
begin
  PacketSinglePin := TPin.Create;
  PacketSinglePin.OnReceive := PacketSingleReceive;
  PacketBurstPin := TPin.Create;
  PacketBurstPin.OnReceive := PacketBurstReceive;
  SendThread := TPacketBurstSendThread.Create(True);
  SendThread.PacketBurst := Self;
  SendThread.Resume;
end;

destructor TPacketBurst.Destroy;
begin
  SendThread.Free;
  PacketSinglePin.Free;
  PacketBurstPin.Free;
  inherited;
end;

procedure TPacketBurst.PacketBurstReceive(Stream: TStream);
var
  PacketStream: TMemoryStreamEx;
  Size: Word;
begin
  PacketStream := TMemoryStreamEx.Create;
  ReceiveStream.Seek(0, soFromEnd);
  ReceiveStream.WriteStream(Stream);
  ReceiveStream.Position := 0;
  Size := ReceiveStream.ReadWord;
  while Size < ReceiveStream.Size do begin
    PacketStream.Clear;
    PacketStream.ReadStream(TStream(ReceiveStream), Size);
    PacketSinglePin.Send(PacketStream);
    Size := ReceiveStream.ReadWord;
  end;
  PacketStream.Free;
end;

procedure TPacketBurst.PacketSingleReceive(Stream: TStream);
var
  SignalEvent: Boolean;
begin
  SendStreamLock.Acquire;
  SendStream.WriteWord(Stream.Size);
  Stream.Position := 0;
  SendStream.WriteStream(Stream);
  SignalEvent := SendStream.Size > SendBurstSize;
  SendStreamLock.Release;
  if SignalEvent then SendThreadEvent.SetEvent;
end;

{ TPacketBurstSendThread }

procedure TPacketBurstSendThread.Execute;
var
  Stream: TMemoryStreamEx;
begin
  inherited;
  Stream := TMemoryStreamEx.Create;
  with PacketBurst do
  repeat
    if SendThreadEvent.WaitFor(SendPeriod) = wrSignaled then begin
      SendStreamLock.Acquire;
      SendStream.Position := 0;
      if SendStream.Size < SendBurstSize then begin
        PacketBurstPin.Send(SendStream);
        SendStream.Clear;
      end else
      while (SendStream.Size - SendStream.Position) > SendBurstSize do begin
        Stream.Clear;
        SendStream.ReadStream(TStream(Stream), SendBurstSize);
        PacketBurstPin.Send(Stream);
      end;
      SendStreamLock.Release;
    end;
  until Terminated;
  Stream.Free;
end;

end.
