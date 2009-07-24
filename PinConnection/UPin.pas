unit UPin;

interface

uses
  Classes;

type
  TOnStreamEvent = procedure (Stream: TStream) of object;

  TPin = class
  private
    FOnReceive: TOnStreamEvent;
    function GetConnected: Boolean;
  published
    Pin: TPin;
    destructor Destroy; override;
    procedure Connect(Pin: TPin);
    procedure Disconnect;
    procedure Send(Stream: TStream);
    procedure Receive(Stream: TStream);
    property OnReceive: TOnStreamEvent read FOnReceive write FOnReceive;
    property Connected: Boolean read GetConnected;
  end;


implementation

{ TPin }

procedure TPin.Connect(Pin: TPin);
begin
  if Assigned(Pin) then Disconnect;
  Self.Pin := Pin;
  Pin.Pin := Self;
end;

destructor TPin.Destroy;
begin
  Disconnect;
  inherited;
end;

procedure TPin.Disconnect;
begin
  if Assigned(Pin) then begin
    Pin.Pin := nil;
    Pin := nil;
  end;
end;

function TPin.GetConnected: Boolean;
begin
  Result := Assigned(Pin);
end;

procedure TPin.Receive(Stream: TStream);
begin
  Stream.Position := 0;
  if Assigned(FOnReceive) then FOnReceive(Stream);
end;

procedure TPin.Send(Stream: TStream);
begin
  if Assigned(Pin) then Pin.Receive(Stream);  
end;


end.
