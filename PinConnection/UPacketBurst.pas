unit UPacketBurst;

{$mode Delphi}{$H+}

interface

uses
  Classes, UCommPin, SyncObjs, UStreamHelper, UCommon, SysUtils,
  UMicroThreading, DateUtils;

type
  TPacketBurst = class;

  TPacketBurstSendThread = class(TMicroThread)
    PacketBurst: TPacketBurst;
    procedure Execute; override;
  end;

  { TPacketBurst }

  TPacketBurst = class
  private
    FActive: Boolean;
    SendThreadEvent: TMicroThreadEvent;
    SendThread: TPacketBurstSendThread;
    SendStreamLock: TCriticalSection;
    SendStream: TStreamHelper;
    ReceiveStream: TStreamHelper;
    procedure PacketSingleReceive(Sender: TCommPin; Stream: TStream);
    procedure PacketBurstReceive(Sender: TCommPin; Stream: TStream);
    procedure SetActive(const AValue: Boolean);
  public
    SendPeriod: TDateTime;
    SendBurstSize: Integer;
    PacketSinglePin: TCommPin;
    PacketBurstPin: TCommPin;
    destructor Destroy; override;
    constructor Create;
    property Active: Boolean read FActive write SetActive;
  end;

implementation

{ TSerialPort }

constructor TPacketBurst.Create;
begin
  PacketSinglePin := TCommPin.Create;
  PacketSinglePin.OnReceive := PacketSingleReceive;
  PacketBurstPin := TCommPin.Create;
  PacketBurstPin.OnReceive := PacketBurstReceive;
  SendThreadEvent := TMicroThreadEvent.Create;
  SendPeriod := OneMillisecond;
end;

destructor TPacketBurst.Destroy;
begin
  Active := False;
  SendThreadEvent.Free;
  PacketSinglePin.Free;
  PacketBurstPin.Free;
  inherited;
end;

procedure TPacketBurst.PacketBurstReceive(Sender: TCommPin; Stream: TStream);
var
  PacketStream: TStreamHelper;
  Size: Word;
begin
  try
    PacketStream := TStreamHelper.Create;
    ReceiveStream.Seek(0, soFromEnd);
    ReceiveStream.WriteStream(Stream, Stream.Size);
    ReceiveStream.Position := 0;
    Size := ReceiveStream.ReadWord;
    while Size < ReceiveStream.Size do begin
      PacketStream.Stream.Size := 0;
      PacketStream.ReadStream(TStream(ReceiveStream), Size);
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
    SendThread.Name := 'PacketBurst';
    SendThread.Start;
  end else begin
    FreeAndNil(SendThread);
  end;
end;

procedure TPacketBurst.PacketSingleReceive(Sender: TCommPin; Stream: TStream);
var
  SignalEvent: Boolean;
begin
  try
    SendStreamLock.Acquire;
    SendStream.WriteWord(Stream.Size);
    Stream.Position := 0;
    SendStream.WriteStream(Stream, Stream.Size);
    SignalEvent := SendStream.Size > SendBurstSize;
  finally
    SendStreamLock.Release;
  end;
  if SignalEvent then SendThreadEvent.SetEvent;
end;

{ TPacketBurstSendThread }

procedure TPacketBurstSendThread.Execute;
var
  Stream: TStreamHelper;
begin
  try
    Stream := TStreamHelper.Create;
    with PacketBurst do
    repeat
      if SendThreadEvent.WaitFor(SendPeriod) = wrSignaled then
      try
        SendStreamLock.Acquire;
        SendStream.Position := 0;
        if SendStream.Size < SendBurstSize then begin
          PacketBurstPin.Send(SendStream);
          SendStream.Stream.Size := 0;
        end else
        while (SendStream.Size - SendStream.Position) > SendBurstSize do begin
          Stream.Stream.Size := 0;
          SendStream.ReadStream(TStream(Stream), SendBurstSize);
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
