// All pins received data is written to main pin
// Data received on main pin is sent to all pins

unit UCommConcentrator;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, UCommPin;

type
  TCommConcentrator = class;

  { TPinList }

  TPinList = class(TObjectList)
    Concentrator: TCommConcentrator;
    function Add(AObject: TObject): Integer;
    function AddNew: TCommPin;
    function Extract(Item: TObject): TObject;
    procedure Insert(Index: Integer; AObject: TObject);
  end;

  { TCommConcentrator }

  TCommConcentrator = class
  private
    FActive: Boolean;
    FPins: TPinList;
    FMain: TCommPin;
    procedure MainReceive(Sender: TCommPin; Stream: TStream);
    procedure MainSetStatus(Sender: TCommPin; Status: Integer);
    procedure Receive(Sender: TCommPin; Stream: TStream);
    procedure SetStatus(Sender: TCommPin; Status: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    property Pins: TPinList read FPins write FPins;
    property Main: TCommPin read FMain write FMain;
    property Active: Boolean read FActive write FActive;
  end;

implementation

{ TPinList }

function TPinList.Add(AObject: TObject): Integer;
begin
  Result := inherited Add(AObject);
  TCommPin(AObject).OnReceive := Concentrator.Receive;
  TCommPin(AObject).OnSetSatus := Concentrator.SetStatus;
end;

function TPinList.AddNew: TCommPin;
begin
  Result := TCommPin(Items[Add(TCommPin.Create)]);
end;

function TPinList.Extract(Item: TObject): TObject;
begin
  TCommPin(Item).OnReceive := nil;
  TCommPin(Item).OnSetSatus := nil;
  Result := inherited Extract(Item);
end;

procedure TPinList.Insert(Index: Integer; AObject: TObject);
begin
  inherited Insert(Index, AObject);
  TCommPin(AObject).OnReceive := Concentrator.Receive;
  TCommPin(AObject).OnSetSatus := Concentrator.SetStatus;
end;

{ TCommConcentrator }

procedure TCommConcentrator.MainReceive(Sender: TCommPin; Stream: TStream);
var
  I: Integer;
begin
  if FActive then begin
    // Broadcast received packet to all other pins
    for I := 0 to FPins.Count - 1 do
      if Sender <> FPins[I] then
        TCommPin(FPins[I]).Send(Stream);
  end;
end;

procedure TCommConcentrator.MainSetStatus(Sender: TCommPin; Status: Integer);
var
  I: Integer;
begin
  if FActive then begin
    // Broadcast received packet to all other pins
    for I := 0 to FPins.Count - 1 do
      if Sender <> FPins[I] then
        TCommPin(FPins[I]).Status := Status;
  end;
end;

procedure TCommConcentrator.Receive(Sender: TCommPin; Stream: TStream);
begin
  if FActive then FMain.Send(Stream);
end;

procedure TCommConcentrator.SetStatus(Sender: TCommPin; Status: Integer);
begin
  if FActive then FMain.Status := Status;
end;

constructor TCommConcentrator.Create;
begin
  FPins := TPinList.Create;
  FPins.Concentrator := Self;
  FMain := TCommPin.Create;
  FMain.OnSetSatus := MainSetStatus;
  FMain.OnReceive := MainReceive;
end;

destructor TCommConcentrator.Destroy;
begin
  FActive := False;
  FPins.Free;
  FMain.Free;
  inherited Destroy;
end;

end.

