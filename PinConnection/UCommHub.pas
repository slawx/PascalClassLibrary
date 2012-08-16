unit UCommHub;

{$mode delphi}

interface

uses
  Classes, SysUtils, Contnrs, UCommPin, SpecializedList;

type
  TCommHub = class;

  { TPinList }

  TPinList = class(TObjectList)
    Hub: TCommHub;
    function Add(AObject: TObject): Integer;
    function AddNew: TCommPin;
    function Extract(Item: TObject): TObject;
    procedure Insert(Index: Integer; AObject: TObject);
  end;

  { TCommHub }

  TCommHub = class(TCommNode)
  private
    FActive: Boolean;
    FPins: TPinList;
    procedure Receive(Sender: TCommPin; Stream: TListByte);
    procedure SetStatus(Sender: TCommPin; Status: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Pins: TPinList read FPins write FPins;
    property Active: Boolean read FActive write FActive;
  end;

implementation

{ TPinList }

function TPinList.Add(AObject: TObject): Integer;
begin
  Result := inherited Add(AObject);
  TCommPin(AObject).OnReceive := Hub.Receive;
  TCommPin(AObject).OnSetSatus := Hub.SetStatus;
end;

function TPinList.AddNew: TCommPin;
begin
  Result := TCommPin(Items[Add(TCommPin.Create)]);
  Result.Node := Hub;
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
  TCommPin(AObject).OnReceive := Hub.Receive;
  TCommPin(AObject).OnSetSatus := Hub.SetStatus;
end;

{ TCommHub }

procedure TCommHub.Receive(Sender: TCommPin; Stream: TListByte);
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

procedure TCommHub.SetStatus(Sender: TCommPin; Status: Integer);
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

constructor TCommHub.Create(AOwner: TComponent);
begin
  inherited;
  FPins := TPinList.Create;
  FPins.Hub := Self;
end;

destructor TCommHub.Destroy;
begin
  Active := False;
  FPins.Free;
  inherited Destroy;
end;

end.

