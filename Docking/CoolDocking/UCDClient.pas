unit UCDClient;

{$mode delphi}{$H+}

// Date: 2010-09-17

interface

uses
  Classes, SysUtils, Controls, LCLType, LMessages, Graphics, StdCtrls,
  Buttons, ExtCtrls, Contnrs, Forms, ComCtrls, Dialogs, Menus, FileUtil,
  UCDCustomize, DOM, XMLWrite, XMLRead, UCDCommon,
  DateUtils, UCDPopupMenu, UCDManager;

const
  GrabberSize = 22;

type

  { TCDClient }

  TCDClient = class(TCDClientBase)
  private
    FDockable: Boolean;
    FFloatable: Boolean;
    procedure SetDockable(const AValue: Boolean);
    procedure SetFloatable(const AValue: Boolean);
    procedure SetPanel(const AValue: TPanel); override;
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

uses
  UCDManagerRegions;

procedure Register;
begin
  RegisterComponents('CoolDocking', [TCDClient]);
  RegisterComponents('CoolDocking', [TCDCustomize]);
end;



{ TCDClient }

procedure TCDClient.SetDockable(const AValue: Boolean);
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

procedure TCDClient.SetFloatable(const AValue: Boolean);
begin
  if FFloatable = AValue then Exit;
  FFloatable := AValue;
end;

procedure TCDClient.SetPanel(const AValue: TPanel);
begin
  inherited SetPanel(AValue);
  if not (csDesigning in ComponentState) then begin
    if Assigned(Panel) then
    with Panel do begin
      DockSite := True;
      UseDockManager := True;
      DockManager := TCDManagerRegions.Create(Panel);
    end;
  end;
end;

constructor TCDClient.Create(AOwner: TComponent);
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
      DockManager := TCDManagerRegions.Create(TWinControl(AOwner));
    end;
  end;
end;

destructor TCDClient.Destroy;
begin
  inherited Destroy;
  Master := nil;
end;


end.

