unit UCommSerialPort;

{$mode Delphi}{$H+}

interface

uses
  Classes, USerialPort, UCommPin, SysUtils;

type
  TCommSerialPort = class(TSerialPort)
  private
    procedure Receive(Sender: TCommPin; Stream: TStream);
    procedure ReceiveData(Stream: TMemoryStream);
  public
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
  Pin := TCommPin.Create;
  Pin.OnReceive := Receive;
  OnReceiveData := ReceiveData;
end;

destructor TCommSerialPort.Destroy;
begin
  OnReceiveData := nil;
  Pin.Free;
  inherited;
end;

procedure TCommSerialPort.Receive(Sender: TCommPin; Stream: TStream);
begin
  Stream.Position := 0;
  repeat
    SendStreamRaw(Stream);
    Sleep(1);
  until Stream.Position = Stream.Size;
end;

end.
