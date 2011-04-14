unit UCommSerialPort;

{$mode Delphi}{$H+}

interface

uses
  Classes, USerialPort, UCommPin, SysUtils, DateUtils,
  SyncObjs;

type
  TCommSerialPort = class(TSerialPort)
  private
    procedure Receive(Sender: TCommPin; Stream: TStream);
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
  Pin.Send(Stream);
end;

constructor TCommSerialPort.Create;
begin
  inherited;
  Lock := TCriticalSection.Create;
  Pin := TCommPin.Create;
  Pin.OnReceive := Receive;
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
  Stream.Position := 0;
  repeat
    try
      Lock.Acquire;
      SendStreamRaw(Stream);
    finally
      Lock.Release;
    end;
    if Stream.Position <> Stream.Size then
      Sleep(1);
  until Stream.Position = Stream.Size;
end;

end.
