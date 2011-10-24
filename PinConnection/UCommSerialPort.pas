unit UCommSerialPort;

{$mode Delphi}{$H+}

interface

uses
  Classes, USerialPort, UCommPin, SysUtils, DateUtils,
  SyncObjs;

type

  { TCommSerialPort }

  TCommSerialPort = class(TSerialPort)
  private
    procedure Receive(Sender: TCommPin; Stream: TStream);
    procedure SetStatus(Sender: TCommPin; AValue: Integer);
    procedure ReceiveData(Stream: TMemoryStream);
  public
    Lock: TCriticalSection;
    Pin: TCommPin;
    destructor Destroy; override;
    constructor Create;
  end;


implementation


{ TCommSerialPort }

procedure TCommSerialPort.ReceiveData(Stream: TMemoryStream);
begin
  if Active then Pin.Send(Stream);
end;

procedure TCommSerialPort.SetStatus(Sender: TCommPin; AValue: Integer);
begin
  try
    Lock.Acquire;
    if (AValue and 1) = 1 then Parity := paMark else Parity := paSpace;
  finally
    Lock.Release;
  end;
end;

constructor TCommSerialPort.Create;
begin
  inherited;
  Lock := TCriticalSection.Create;
  Pin := TCommPin.Create;
  Pin.OnReceive := Receive;
  Pin.OnSetSatus := SetStatus;
  OnReceiveData := ReceiveData;
end;

destructor TCommSerialPort.Destroy;
begin
  OnReceiveData := nil;
  Pin.Free;
  Lock.Free;
  inherited;
end;

procedure TCommSerialPort.Receive(Sender: TCommPin; Stream: TStream);
begin
  if Active then begin
    Stream.Position := 0;
    repeat
      try
        Lock.Acquire;
        if CanWrite(0) then
          SendStreamRaw(Stream);
      finally
        Lock.Release;
      end;
      if Stream.Position <> Stream.Size then
        Sleep(1);
    until Stream.Position = Stream.Size;
  end;
end;

end.
