unit UCDMaster;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UCDCommon, UCDManager;

type
  { TCDMaster }

  TCDMaster = class(TCDMasterBase)
  private
    FDefaultHeaderPos: THeaderPos;
    FDefaultMoveSpeed: Integer;
    FDefaultTabsPos: THeaderPos;
    FShowIcons: Boolean;
    FTabsEnabled: Boolean;
    procedure SetShowIcons(const AValue: Boolean);
    procedure SetTabsEnabled(const AValue: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property TabsEnabled: Boolean read FTabsEnabled write SetTabsEnabled;
    property DefaultTabsPos: THeaderPos read FDefaultTabsPos
      write FDefaultTabsPos;
    property DefaultHeaderPos: THeaderPos read FDefaultHeaderPos
      write FDefaultHeaderPos;
    property DefaultMoveSpeed: Integer read FDefaultMoveSpeed
      write FDefaultMoveSpeed;
    property ShowIcons: Boolean read FShowIcons
      write SetShowIcons;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CoolDocking', [TCDMaster]);
end;

{ TCDMaster }

procedure TCDMaster.SetTabsEnabled(const AValue: Boolean);
begin
  if FTabsEnabled = AValue then Exit;
  FTabsEnabled := AValue;
end;

procedure TCDMaster.SetShowIcons(const AValue: Boolean);
begin
  if FShowIcons = AValue then Exit;
  FShowIcons := AValue;
end;

constructor TCDMaster.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCDMaster.Destroy;
var
  I: Integer;
begin
  // Assigning nil to Client Master property cause unregistring client from list
  Customize := nil;
  inherited Destroy;
end;

end.

