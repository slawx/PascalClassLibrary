unit UCommDelay;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, UCommPin, UThreading, SyncObjs, SpecializedList, UStreamHelper;

type
  TCommDelay = class;

  { TDelayedPacket }

  TDelayedPacket = class
    ReceiveTime: TDateTime;
    Data: TStreamHelper;
    constructor Create;
    destructor Destroy; override;
  end;

  { TCommDelayThread }

  TCommDelayThread = class(TTermThread)
    Parent: TCommDelay;
    PacketQueue: TListObject;
    Pin: TCommPin;
    Lock: TCriticalSection;
    procedure Execute; override;
  end;

  { TCommDelay }

  TCommDelay = class
  private
    FActive: Boolean;
    FDelay: TDateTime;
    PacketQueue1: TListObject; // TListObject<TDelayedPacket>
    PacketQueue2: TListObject; // TListObject<TDelayedPacket>
    Thread1: TCommDelayThread;
    Thread2: TCommDelayThread;
    procedure ReceiveData1(Sender: TCommPin; AStream: TStream);
    procedure ReceiveData2(Sender: TCommPin; AStream: TStream);
    procedure SetActive(AValue: Boolean);
  public
    Lock1: TCriticalSection;
    Lock2: TCriticalSection;
    Pin1: TCommPin;
    Pin2: TCommPin;
    constructor Create;
    destructor Destroy; override;
    property Delay: TDateTime read FDelay write FDelay;
    property Active: Boolean read FActive write SetActive;
  end;

implementation

{ TCommDelayThread }

procedure TCommDelayThread.Execute;
var
  I: Integer;
  CurrentTime: TDateTime;
  SendData: TStreamHelper;
  DoSleep: Boolean;
begin
  try
    SendData := TStreamHelper.Create;
    repeat
      DoSleep := True;
        try
          Lock.Acquire;
          CurrentTime := Now;
          I := 0;
          while (I < PacketQueue.Count) do
            if TDelayedPacket(PacketQueue[I]).ReceiveTime < (CurrentTime - Parent.Delay) then begin
              DoSleep := False;
              SendData.Clear;
              SendData.WriteStream(TDelayedPacket(PacketQueue[I]).Data, TDelayedPacket(PacketQueue[I]).Data.Size);
              PacketQueue.Delete(I);
              try
                Lock.Release;
                Pin.Send(SendData.Stream);
              finally
                Lock.Acquire;
              end;
            end else Inc(I);
        finally
          Lock.Release;
        end;
      if not Terminated and DoSleep then Sleep(1);
    until Terminated;
  finally
    SendData.Free;
  end;
end;

{ TDelayedPacket }

constructor TDelayedPacket.Create;
begin
  Data := TStreamHelper.Create;
end;

destructor TDelayedPacket.Destroy;
begin
  Data.Free;
  inherited Destroy;
end;

{ TCommDelay }

procedure TCommDelay.ReceiveData1(Sender: TCommPin; AStream: TStream);
begin
  try
    Lock2.Acquire;
    if Delay = 0 then Pin2.Send(AStream)
    else
    with TDelayedPacket(PacketQueue2.AddNew(TDelayedPacket.Create)) do begin
      ReceiveTime := Now;
      Data.WriteStream(AStream, AStream.Size);
    end;
  finally
    Lock2.Release;
  end;
end;

procedure TCommDelay.ReceiveData2(Sender: TCommPin; AStream: TStream);
begin
  try
    Lock1.Acquire;
    if Delay = 0 then Pin1.Send(AStream)
    else
    with TDelayedPacket(PacketQueue1.AddNew(TDelayedPacket.Create)) do begin
      ReceiveTime := Now;
      Data.WriteStream(AStream, AStream.Size);
    end;
  finally
    Lock1.Release;
  end;
end;

procedure TCommDelay.SetActive(AValue: Boolean);
begin
  if FActive = AValue then Exit;
  FActive := AValue;
  if AValue then begin
    Thread1 := TCommDelayThread.Create(True);
    Thread1.FreeOnTerminate := False;
    Thread1.Parent := Self;
    Thread1.Name := 'CommDelay1';
    Thread1.PacketQueue := PacketQueue1;
    Thread1.Pin := Pin1;
    Thread1.Lock := Lock1;
    Thread1.Start;

    Thread2 := TCommDelayThread.Create(True);
    Thread2.FreeOnTerminate := False;
    Thread2.Parent := Self;
    Thread2.Name := 'CommDelay2';
    Thread2.PacketQueue := PacketQueue2;
    Thread2.Pin := Pin2;
    Thread2.Lock := Lock2;
    Thread2.Start;
  end else begin
    FreeAndNil(Thread1);
    FreeAndNil(Thread2);
  end;
end;

constructor TCommDelay.Create;
begin
  Lock1 := TCriticalSection.Create;
  Lock2 := TCriticalSection.Create;
  PacketQueue1 := TListObject.Create;
  PacketQueue2 := TListObject.Create;
  Pin1 := TCommPin.Create;
  Pin1.OnReceive := ReceiveData1;
  Pin2 := TCommPin.Create;
  Pin2.OnReceive := ReceiveData2;
end;

destructor TCommDelay.Destroy;
begin
  Active := False;
  Pin2.Free;
  Pin1.Free;
  PacketQueue1.Free;
  PacketQueue2.Free;
  Lock1.Free;
  Lock2.Free;
  inherited Destroy;
end;

end.

