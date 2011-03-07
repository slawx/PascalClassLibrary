unit UCoolDockPopupMenu;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Menus, Forms, Controls, Dialogs, UCoolDockClientPanel,
  ExtCtrls, ComCtrls;

type

  { TCoolDockPopupMenu }

  TCoolDockPopupMenu = class(TPopupMenu)
  public
    Manager: TObject; // TCoolDockManager
    constructor Create(AManager: TObject);
    procedure PopupMenuListClick(Sender: TObject);
    procedure PopupMenuTabsClick(Sender: TObject);
    procedure PopupMenuPopupListClick(Sender: TObject);
    procedure PopupMenuPopupTabsClick(Sender: TObject);
    procedure PopupMenuCloseClick(Sender: TObject);
    procedure PopupMenuRenameClick(Sender: TObject);
    procedure PopupMenuPositionAutoClick(Sender: TObject);
    procedure PopupMenuPositionLeftClick(Sender: TObject);
    procedure PopupMenuPositionRightClick(Sender: TObject);
    procedure PopupMenuPositionTopClick(Sender: TObject);
    procedure PopupMenuPositionBottomClick(Sender: TObject);
    procedure PopupMenuUndockClick(Sender: TObject);
    procedure PopupMenuCustomizeClick(Sender: TObject);
  end;

implementation

uses
  UCoolDocking, UCoolDockStyleTabs;

resourcestring
  SDockStyle = 'Style';
  SDockList = 'List';
  SDockTabs = 'Tabs';
  SDockPopupList = 'Popup list';
  SDockPopupTabs = 'Popup tabs';
  SCloseForm = 'Close';
  SRenameForm = 'Rename';
  SPosition = 'Position';
  SPositionAuto = 'Auto';
  SPositionTop = 'Top';
  SPositionLeft = 'Left';
  SPositionRight = 'Right';
  SPositionBottom = 'Bottom';
  SUndock = 'Undock';
  SCustomize = 'Customize...';
  SEnterNewWindowName = 'Enter new window name';
  SRenameWindow = 'Rename window';


{ TCoolDockPopupMenu }

constructor TCoolDockPopupMenu.Create(AManager: TObject);
var
  NewMenuItem: TMenuItem;
  NewMenuItem2: TMenuItem;
  I: Integer;
begin
  inherited Create(nil);
  Manager := AManager;

  Name := TCoolDockManager(AManager).DockSite.Name + '_' + 'PopupMenu';

  NewMenuItem := TMenuItem.Create(Self);
  NewMenuItem.Caption := SDockStyle;
  Items.Add(NewMenuItem);

  NewMenuItem2 := TMenuItem.Create(NewMenuItem);
  NewMenuItem2.Caption := SDockList;
  NewMenuItem2.OnClick := PopupMenuListClick;
  NewMenuItem.Add(NewMenuItem2);

  NewMenuItem2 := TMenuItem.Create(NewMenuItem);
  NewMenuItem2.Caption := SDockTabs;
  NewMenuItem2.OnClick := PopupMenuTabsClick;
  NewMenuItem.Add(NewMenuItem2);

  NewMenuItem2 := TMenuItem.Create(NewMenuItem);
  NewMenuItem2.Caption := SDockPopupList;
  NewMenuItem2.OnClick := PopupMenuPopupListClick;
  NewMenuItem.Add(NewMenuItem2);

  NewMenuItem2 := TMenuItem.Create(NewMenuItem);
  NewMenuItem2.Caption := SDockPopupTabs;
  NewMenuItem2.OnClick := PopupMenuPopupTabsClick;
  NewMenuItem.Add(NewMenuItem2);

  NewMenuItem := TMenuItem.Create(Self);
  NewMenuItem.Caption := SPosition;
  Items.Add(NewMenuItem);

  NewMenuItem2 := TMenuItem.Create(NewMenuItem);
  NewMenuItem2.Caption := SPositionAuto;
  NewMenuItem2.OnClick := PopupMenuPositionAutoClick;
  NewMenuItem.Add(NewMenuItem2);

  NewMenuItem2 := TMenuItem.Create(NewMenuItem);
  NewMenuItem2.Caption := SPositionTop;
  NewMenuItem2.OnClick := PopupMenuPositionTopClick;
  NewMenuItem.Add(NewMenuItem2);

  NewMenuItem2 := TMenuItem.Create(NewMenuItem);
  NewMenuItem2.Caption := SPositionLeft;
  NewMenuItem2.OnClick := PopupMenuPositionLeftClick;
  NewMenuItem.Add(NewMenuItem2);

  NewMenuItem2 := TMenuItem.Create(NewMenuItem);
  NewMenuItem2.Caption := SPositionBottom;
  NewMenuItem2.OnClick := PopupMenuPositionBottomClick;
  NewMenuItem.Add(NewMenuItem2);

  NewMenuItem2 := TMenuItem.Create(NewMenuItem);
  NewMenuItem2.Caption := SPositionRight;
  NewMenuItem2.OnClick := PopupMenuPositionRightClick;
  NewMenuItem.Add(NewMenuItem2);

  NewMenuItem := TMenuItem.Create(Self);
  NewMenuItem.Caption := SCloseForm;
  NewMenuItem.OnClick := PopupMenuCloseClick;
  Items.Add(NewMenuItem);

  NewMenuItem := TMenuItem.Create(Self);
  NewMenuItem.Caption := SRenameForm;
  NewMenuItem.OnClick := PopupMenuRenameClick;
  Items.Add(NewMenuItem);

  NewMenuItem := TMenuItem.Create(Self);
  NewMenuItem.Caption := SUndock;
  NewMenuItem.OnClick := PopupMenuUndockClick;
  Items.Add(NewMenuItem);

  NewMenuItem := TMenuItem.Create(Self);
  NewMenuItem.Caption := SCustomize;
  NewMenuItem.OnClick := PopupMenuCustomizeClick;
  Items.Add(NewMenuItem);
end;

procedure TCoolDockPopupMenu.PopupMenuTabsClick(Sender: TObject);
begin
  TCoolDockManager(Manager).DockStyle := dsTabs;
end;

procedure TCoolDockPopupMenu.PopupMenuPopupListClick(Sender: TObject);
begin
  TCoolDockManager(Manager).DockStyle := dsPopupList;
end;

procedure TCoolDockPopupMenu.PopupMenuPopupTabsClick(Sender: TObject);
begin
  TCoolDockManager(Manager).DockStyle := dsPopupTabs;
end;

procedure TCoolDockPopupMenu.PopupMenuCloseClick(Sender: TObject);
var
  Control: TControl;
begin
  if PopupComponent is TTabControl then
  with TTabControl(PopupComponent) do begin
    TForm(TCoolDockClientPanel(TCoolDockManager(Manager).DockPanels[TabIndex]).Control).Close;
  end;
  if PopupComponent is TCoolDockHeader then
  with TCoolDockHeader(PopupComponent) do begin
    TForm(ParentClientPanel.Control).Close;
  end;
end;

procedure TCoolDockPopupMenu.PopupMenuRenameClick(Sender: TObject);
var
  Value: string;
begin
  //ShowMessage(PopupComponent.ClassName);
  if PopupComponent is TTabControl then
  with TTabControl(PopupComponent) do begin
    Value := TCoolDockClientPanel(TCoolDockManager(Manager).DockPanels[TabIndex]).Control.Caption;
    if InputQuery(SRenameWindow, SEnterNewWindowName, False, Value) then begin
      TCoolDockClientPanel(TCoolDockManager(Manager).DockPanels[TabIndex]).Control.Caption := Value;
      Tabs[TabIndex] := Value;
    end;
  end;
  if PopupComponent is TCoolDockHeader then
  with TCoolDockHeader(PopupComponent) do begin
    Value := ParentClientPanel.Control.Caption;
    if InputQuery(SRenameWindow, SEnterNewWindowName, False, Value) then begin
      ParentClientPanel.Control.Caption := Value;
      Title.Caption := Value;
    end;
  end;
end;

procedure TCoolDockPopupMenu.PopupMenuPositionAutoClick(Sender: TObject);
begin
  TCoolDockManager(Manager).HeaderPos := hpAuto;
end;

procedure TCoolDockPopupMenu.PopupMenuPositionLeftClick(Sender: TObject);
var
  Control: TControl;
begin
  Control := FindLCLControl(Mouse.CursorPos);
  TCoolDockManager(Manager).HeaderPos := hpLeft;
end;

procedure TCoolDockPopupMenu.PopupMenuPositionRightClick(Sender: TObject);
begin
  TCoolDockManager(Manager).HeaderPos := hpRight;
end;

procedure TCoolDockPopupMenu.PopupMenuPositionTopClick(Sender: TObject);
begin
  TCoolDockManager(Manager).HeaderPos := hpTop;
end;

procedure TCoolDockPopupMenu.PopupMenuPositionBottomClick(Sender: TObject);
begin
  TCoolDockManager(Manager).HeaderPos := hpBottom;
end;

procedure TCoolDockPopupMenu.PopupMenuUndockClick(Sender: TObject);
var
  Control: TControl;
begin

  //Control.ManualFloat(Control.BoundsRect);
end;

procedure TCoolDockPopupMenu.PopupMenuCustomizeClick(Sender: TObject);
begin
  with TCoolDockManager(Manager) do
  if Assigned(Master) and
    Assigned(Master.Customize) then
    Master.Customize.Execute;
end;

procedure TCoolDockPopupMenu.PopupMenuListClick(Sender: TObject);
begin
  TCoolDockManager(Manager).DockStyle := dsList;
end;


end.

