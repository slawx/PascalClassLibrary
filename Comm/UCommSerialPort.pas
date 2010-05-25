unit UCommSerialPort;

{$mode Delphi}{$H+}

interface

uses
  Classes, USerialPort, UCommPin, SysUtils;

type
  TCommSerialPort = class(TSerialPort)
  private
    FTxCount: Integer;
    FRxCount: Integer;
    procedure Receive(Stream: TStream);
    procedure ReceiveData(Stream: TMemoryStream);
  public
    DataPin: TCommPin;
    destructor Destroy; override;
    constructor Create;
    property TxCount: Integer read FTxCount;
    property RxCount: Integer read FRxCount;
  end;


implementation


{ TCommSerialPort }

procedure TCommSerialPort.ReceiveData(Stream: TMemoryStream);
begin
  Inc(FRxCount, Stream.Size);
  DataPin.Send(Stream);
end;

constructor TCommSerialPort.Create;
begin
  inherited;
  DataPin := TCommPin.Create;
  DataPin.OnReceive := Receive;
  OnReceiveData := ReceiveData;
end;

destructor TCommSerialPort.Destroy;
begin
  FreeAndNil(DataPin);
  inherited;
end;

procedure TCommSerialPort.Receive(Stream: TStream);
begin
  Inc(FTxCount, Stream.Size);
  Stream.Position := 0;
  repeat
    SendStreamRaw(Stream);
    Sleep(1);
  until Stream.Position = Stream.Size;
end;

end.
