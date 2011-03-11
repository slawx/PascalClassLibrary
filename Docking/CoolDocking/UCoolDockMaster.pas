unit UCoolDockMaster;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UCoolDockCommon, UCoolDockStyle, UCoolDockClientPanel;

type
  { TCoolDockMaster }

  TCoolDockMaster = class(TCoolDockMasterBase)
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
  RegisterComponents('CoolDocking', [TCoolDockMaster]);
end;

{ TCoolDockMaster }

procedure TCoolDockMaster.SetTabsEnabled(const AValue: Boolean);
begin
  if FTabsEnabled = AValue then Exit;
  FTabsEnabled := AValue;
end;

procedure TCoolDockMaster.SetShowIcons(const AValue: Boolean);
begin
  if FShowIcons = AValue then Exit;
  FShowIcons := AValue;
end;

constructor TCoolDockMaster.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCoolDockMaster.Destroy;
var
  I: Integer;
begin
  // Assigning nil to Client Master property cause unregistring client from list
  Customize := nil;
  inherited Destroy;
end;

end.

