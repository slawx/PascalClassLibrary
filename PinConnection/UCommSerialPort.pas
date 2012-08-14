unit UCommSerialPort;

{$mode Delphi}{$H+}

interface

uses
  Classes, USerialPort, UCommPin, SysUtils, DateUtils, SpecializedList,
  SyncObjs;

type

  { TCommSerialPort }

  TCommSerialPort = class(TSerialPort)
  private
    procedure Receive(Sender: TCommPin; Stream: TListByte);
    procedure SetStatus(Sender: TCommPin; AValue: Integer);
    procedure ReceiveData(Stream: TListByte);
  public
    Lock: TCriticalSection;
    Pin: TCommPin;
    destructor Destroy; override;
    constructor Create;
  end;


implementation


{ TCommSerialPort }

procedure TCommSerialPort.ReceiveData(Stream: TListByte);
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

procedure TCommSerialPort.Receive(Sender: TCommPin; Stream: TListByte);
var
  S: TMemoryStream;
begin
  try
    S := TMemoryStream.Create;
    Stream.WriteToStream(S);
    if Active then begin
      S.Position := 0;
      repeat
        try
          Lock.Acquire;
          if CanWrite(0) then
            SendStreamRaw(S);
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
