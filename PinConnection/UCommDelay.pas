unit UCommDelay;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, UCommPin, UThreading, SyncObjs, SpecializedList;

type
  TCommDelay = class;

  { TDelayedPacket }

  TDelayedPacket = class
    ReceiveTime: TDateTime;
    Data: TListByte;
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

  TCommDelay = class(TCommNode)
  private
    FDelay: TDateTime;
    PacketQueue1: TListObject; // TListObject<TDelayedPacket>
    PacketQueue2: TListObject; // TListObject<TDelayedPacket>
    Thread1: TCommDelayThread;
    Thread2: TCommDelayThread;
    procedure ReceiveData1(Sender: TCommPin; AStream: TListByte);
    procedure ReceiveData2(Sender: TCommPin; AStream: TListByte);
  protected
    procedure SetActive(const AValue: Boolean); override;
  public
    Lock1: TCriticalSection;
    Lock2: TCriticalSection;
    Pin1: TCommPin;
    Pin2: TCommPin;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Delay: TDateTime read FDelay write FDelay;
  end;

implementation

{ TCommDelayThread }

procedure TCommDelayThread.Execute;
var
  I: Integer;
  CurrentTime: TDateTime;
  SendData: TListByte;
  DoSleep: Boolean;
begin
  try
    SendData := TListByte.Create;
    repeat
      DoSleep := True;
        try
          Lock.Acquire;
          CurrentTime := Now;
          I := 0;
          while (I < PacketQueue.Count) do
            if TDelayedPacket(PacketQueue[I]).ReceiveTime < (CurrentTime - Parent.Delay) then begin
              DoSleep := False;
              SendData.Assign(TDelayedPacket(PacketQueue[I]).Data);
              PacketQueue.Delete(I);
              try
                Lock.Release;
                Pin.Send(SendData);
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
  Data := TListByte.Create;
end;

destructor TDelayedPacket.Destroy;
begin
  Data.Free;
  inherited Destroy;
end;

{ TCommDelay }

procedure TCommDelay.ReceiveData1(Sender: TCommPin; AStream: TListByte);
begin
  try
    Lock2.Acquire;
    if Delay = 0 then Pin2.Send(AStream)
    else
    with TDelayedPacket(PacketQueue2.AddNew(TDelayedPacket.Create)) do begin
      ReceiveTime := Now;
      Data.Assign(AStream);
    end;
  finally
    Lock2.Release;
  end;
end;

procedure TCommDelay.ReceiveData2(Sender: TCommPin; AStream: TListByte);
begin
  try
    Lock1.Acquire;
    if Delay = 0 then Pin1.Send(AStream)
    else
    with TDelayedPacket(PacketQueue1.AddNew(TDelayedPacket.Create)) do begin
      ReceiveTime := Now;
      Data.Assign(AStream);
    end;
  finally
    Lock1.Release;
  end;
end;

procedure TCommDelay.SetActive(const AValue: Boolean);
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
  inherited;
end;

constructor TCommDelay.Create(AOwner: TComponent);
begin
  inherited;
  Lock1 := TCriticalSection.Create;
  Lock2 := TCriticalSection.Create;
  PacketQueue1 := TListObject.Create;
  PacketQueue2 := TListObject.Create;
  Pin1 := TCommPin.Create;
  Pin1.OnReceive := ReceiveData1;
  Pin1.Node := Self;
  Pin2 := TCommPin.Create;
  Pin2.OnReceive := ReceiveData2;
  Pin2.Node := Self;
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

