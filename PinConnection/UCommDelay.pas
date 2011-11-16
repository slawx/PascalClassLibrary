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
    procedure Execute; override;
  end;

  { TCommDelay }

  TCommDelay = class
  private
    FActive: Boolean;
    FDelay: TDateTime;
    PacketQueue1: TListObject; // TListObject<TDelayedPacket>
    PacketQueue2: TListObject; // TListObject<TDelayedPacket>
    Thread: TCommDelayThread;
    procedure ReceiveData1(Sender: TCommPin; AStream: TStream);
    procedure ReceiveData2(Sender: TCommPin; AStream: TStream);
    procedure SetActive(AValue: Boolean);
  public
    Lock: TCriticalSection;
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
begin
  try
    SendData := TStreamHelper.Create;
    repeat
      with Parent do begin
        try
          Lock.Acquire;
          CurrentTime := Now;
          for I := PacketQueue1.Count - 1 downto 0 do
            if TDelayedPacket(PacketQueue1[I]).ReceiveTime < (CurrentTime - Delay) then begin
              SendData.Clear;
              SendData.WriteStream(TDelayedPacket(PacketQueue1[I]).Data, TDelayedPacket(PacketQueue1[I]).Data.Size);
              PacketQueue1.Delete(I);
              try
                Lock.Release;
                Pin1.Send(SendData.Stream);
              finally
                Lock.Acquire;
              end;
            end;

          for I := PacketQueue2.Count - 1 downto 0 do
            if TDelayedPacket(PacketQueue2[I]).ReceiveTime < (CurrentTime - Delay) then begin
              SendData.Clear;
              SendData.WriteStream(TDelayedPacket(PacketQueue2[I]).Data, TDelayedPacket(PacketQueue2[I]).Data.Size);
              PacketQueue2.Delete(I);
              try
                Lock.Release;
                Pin2.Send(SendData.Stream);
              finally
                Lock.Acquire;
              end;
            end;
        finally
          Lock.Release;
        end;
      end;
      if not Terminated then Sleep(1);
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
    Lock.Acquire;
    if Delay = 0 then Pin2.Send(AStream)
    else
    with TDelayedPacket(PacketQueue2.AddNew(TDelayedPacket.Create)) do begin
      ReceiveTime := Now;
      Data.WriteStream(AStream, AStream.Size);
    end;
  finally
    Lock.Release;
  end;
end;

procedure TCommDelay.ReceiveData2(Sender: TCommPin; AStream: TStream);
begin
  try
    Lock.Acquire;
    if Delay = 0 then Pin1.Send(AStream)
    else
    with TDelayedPacket(PacketQueue1.AddNew(TDelayedPacket.Create)) do begin
      ReceiveTime := Now;
      Data.WriteStream(AStream, AStream.Size);
    end;
  finally
    Lock.Release;
  end;
end;

procedure TCommDelay.SetActive(AValue: Boolean);
begin
  if FActive = AValue then Exit;
  FActive := AValue;
  if AValue then begin
    Thread := TCommDelayThread.Create(True);
    Thread.FreeOnTerminate := False;
    Thread.Parent := Self;
    Thread.Name := 'CommDelay';
    Thread.Start;
  end else begin
    FreeAndNil(Thread);
  end;
end;

constructor TCommDelay.Create;
begin
  Lock := TCriticalSection.Create;
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
  Lock.Free;
  inherited Destroy;
end;

end.

