unit UCoolDocking;

{$mode delphi}{$H+}

// Date: 2010-09-17

interface

uses
  Classes, SysUtils, Controls, LCLType, LMessages, Graphics, StdCtrls,
  Buttons, ExtCtrls, Contnrs, Forms, ComCtrls, Dialogs, Menus, FileUtil,
  UCoolDockCustomize, DOM, XMLWrite, XMLRead, UCoolDockCommon,
  DateUtils, UCoolDockStyleTabs, UCoolDockStyleRegions, UCoolDockStylePopupTabs,
  UCoolDockStylePopupRegions, UCoolDockStyle, UCoolDockClientPanel,
  UCoolDockPopupMenu, UCoolDockManager;

const
  GrabberSize = 22;

type
  TCoolDockClient = class;
  TCoolDockMaster = class;

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

  TCoolDockClient = class(TCoolDockClientBase)
  private
    FDockable: Boolean;
    FFloatable: Boolean;
    procedure SetDockable(const AValue: Boolean);
    procedure SetFloatable(const AValue: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Dockable: Boolean read FDockable
      write SetDockable default True;
    property Floatable: Boolean read FFloatable
      write SetFloatable default True;
  end;


procedure Register;

resourcestring
  SWrongOwner = 'Owner of TCoolDockClient have to be TForm';

implementation

procedure Register;
begin
  RegisterComponents('CoolDocking', [TCoolDockMaster]);
  RegisterComponents('CoolDocking', [TCoolDockClient]);
  RegisterComponents('CoolDocking', [TCoolDockCustomize]);
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


{ TCoolDockClient }

procedure TCoolDockClient.SetDockable(const AValue: Boolean);
begin
  if FDockable = AValue then Exit;
  FDockable := AValue;
  if (Owner is TForm) then
  with (Owner as TForm) do
  if AValue then begin
    DragKind := dkDock;
    DragMode := dmAutomatic;
    DockSite := True;
  end else begin
    DragKind := dkDrag;
    DragMode := dmManual;
    DockSite := False;
  end;
end;

procedure TCoolDockClient.SetFloatable(const AValue: Boolean);
begin
  if FFloatable = AValue then Exit;
  FFloatable := AValue;
end;

constructor TCoolDockClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDockable := True;
  if not (AOwner is TForm) then
    raise Exception.Create(SWrongOwner);
  with (AOwner as TForm) do begin
    if not (csDesigning in ComponentState) then begin
      if Dockable then begin
        DragKind := dkDock;
        DragMode := dmAutomatic;
        DockSite := True;
      end;
      UseDockManager := True;
      DockManager := TCoolDockManager.Create(TWinControl(AOwner));
    end;
  end;
end;

destructor TCoolDockClient.Destroy;
begin
  inherited Destroy;
  Master := nil;
end;


end.

