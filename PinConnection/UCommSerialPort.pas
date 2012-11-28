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
    procedure AssignTo(Dest: TPersistent); override;
  public
    SerialPort: TSerialPort;
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
  FActive := SerialPort.Active;
end;

procedure TCommSerialPort.AssignTo(Dest: TPersistent);
begin
  if Dest is TCommSerialPort then begin
    TCommSerialPort(Dest).SerialPort.Assign(SerialPort);
  end else inherited;
end;

procedure TCommSerialPort.SetStatus(Sender: TCommPin; AValue: Integer);
begin
  try
    SerialPort.Lock.Acquire;
    if (AValue and 1) = 1 then SerialPort.Parity := paMark
      else SerialPort.Parity := paSpace;
  finally
    SerialPort.Lock.Release;
  end;
end;

constructor TCommSerialPort.Create(AOwner: TComponent);
begin
  inherited;
  SerialPort := TSerialPort.Create;
  SerialPort.OnReceiveData := ReceiveData;
  Pin := TCommPin.Create;
  Pin.OnReceive := Receive;
  Pin.OnSetSatus := SetStatus;
  Pin.Node := Self;
end;

destructor TCommSerialPort.Destroy;
begin
  SerialPort.OnReceiveData := nil;
  FreeAndNil(Pin);
  FreeAndNil(SerialPort);
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
          SerialPort.Lock.Acquire;
          if SerialPort.CanWrite(0) then
            SerialPort.SendStreamRaw(S);
        finally
          SerialPort.Lock.Release;
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
