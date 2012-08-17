unit UCommSerialPort;

{$mode Delphi}{$H+}

interface

uses
  Classes, USerialPort, UCommPin, SysUtils, DateUtils, SpecializedList,
  SyncObjs;

type

  { TCommSerialPort }

  TCommSerialPort = class(TCommNode)
  private
    procedure Receive(Sender: TCommPin; Stream: TListByte);
    procedure SetStatus(Sender: TCommPin; AValue: Integer);
    procedure ReceiveData(Stream: TListByte);
  protected
    procedure SetActive(const AValue: Boolean); override;
  public
    SerialPort: TSerialPort;
    Lock: TCriticalSection;
    Pin: TCommPin;
    destructor Destroy; override;
    constructor Create(AOwner: TComponent); override;
  end;


implementation


{ TCommSerialPort }

procedure TCommSerialPort.ReceiveData(Stream: TListByte);
begin
  if SerialPort.Active then Pin.Send(Stream);
end;

procedure TCommSerialPort.SetActive(const AValue: Boolean);
begin
  inherited;
  SerialPort.Active := AValue;
end;

procedure TCommSerialPort.SetStatus(Sender: TCommPin; AValue: Integer);
begin
  try
    Lock.Acquire;
    if (AValue and 1) = 1 then SerialPort.Parity := paMark
      else SerialPort.Parity := paSpace;
  finally
    Lock.Release;
  end;
end;

constructor TCommSerialPort.Create(AOwner: TComponent);
begin
  inherited;
  SerialPort := TSerialPort.Create;
  SerialPort.OnReceiveData := ReceiveData;
  Lock := TCriticalSection.Create;
  Pin := TCommPin.Create;
  Pin.OnReceive := Receive;
  Pin.OnSetSatus := SetStatus;
  Pin.Node := Self;
end;

destructor TCommSerialPort.Destroy;
begin
  SerialPort.OnReceiveData := nil;
  Pin.Free;
  Lock.Free;
  SerialPort.Free;
  inherited;
end;

procedure TCommSerialPort.Receive(Sender: TCommPin; Stream: TListByte);
var
  S: TMemoryStream;
begin
  try
    S := TMemoryStream.Create;
    Stream.WriteToStream(S);
    if SerialPort.Active then begin
      S.Position := 0;
      repeat
        try
          Lock.Acquire;
          if SerialPort.CanWrite(0) then
            SerialPort.SendStreamRaw(S);
        finally
          Lock.Release;
        end;
        if S.Position <> S.Size then
          Sleep(1);
      until S.Position = S.Size;
    end;
  finally
    S.Free;
  end;
end;

end.
