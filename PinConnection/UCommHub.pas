unit UCommHub;

{$mode delphi}

interface

uses
  Classes, SysUtils, Contnrs, UCommPin;

type

  { TCommHub }

  TCommHub = class
  private
    FPins: TObjectList;
    function GetPin(Index: Integer): TCommPin;
    procedure Receive(Sender: TCommPin; Stream: TStream);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Pin: TCommPin): Integer;
    function Extract(Pin: TCommPin): TCommPin;
    function Remove(Pin: TCommPin): Integer;
    function IndexOf(Pin: TCommPin): Integer;
    Procedure Insert(Index: Integer; Pin: TCommPin);
    function First: TCommPin;
    Function Last: TCommPin;
    function AddNew: TCommPin;
    property Pins[Index: Integer]: TCommPin read GetPin;
  end;

implementation

{ TCommHub }

function TCommHub.GetPin(Index: Integer): TCommPin;
begin
  Result := TCommPin(FPins[Index]);
end;

procedure TCommHub.Receive(Sender: TCommPin; Stream: TStream);
var
  I: Integer;
begin
  // Broadcast received packet to all other pins
  for I := 0 to FPins.Count - 1 do
    if Sender <> FPins[I] then
      TCommPin(FPins[I]).Send(Stream);
end;

constructor TCommHub.Create;
begin
  FPins := TObjectList.Create;
end;

destructor TCommHub.Destroy;
begin
  FPins.Free;
  inherited Destroy;
end;

function TCommHub.Add(Pin: TCommPin): Integer;
begin
  Pin.OnReceive := Receive;
  Result := FPins.Add(Pin);
end;

function TCommHub.Extract(Pin: TCommPin): TCommPin;
begin
  Pin.OnReceive := nil;
  Result := TCommPin(FPins.Extract(Pin));
end;

function TCommHub.Remove(Pin: TCommPin): Integer;
begin
  Result := FPins.Remove(Pin);
end;

function TCommHub.IndexOf(Pin: TCommPin): Integer;
begin
  Result := FPins.IndexOf(Pin);
end;

procedure TCommHub.Insert(Index: Integer; Pin: TCommPin);
begin
  Pin.OnReceive := Receive;
  FPins.Insert(Index, Pin);
end;

function TCommHub.First: TCommPin;
begin
  Result := TCommPin(FPins.First);
end;

function TCommHub.Last: TCommPin;
begin
  Result := TCommPin(FPins.Last);
end;

function TCommHub.AddNew: TCommPin;
begin
  Result := TCommPin.Create;
  Result.OnReceive := Receive;
  FPins.Add(Result);
end;

end.

