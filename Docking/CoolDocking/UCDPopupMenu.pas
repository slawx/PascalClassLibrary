unit UCDPopupMenu;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Menus, Forms, Controls, Dialogs, UCDClientPanel,
  ExtCtrls, ComCtrls, UCDCommon;

type

  { TCDPopupMenu }

  TCDPopupMenu = class(TPopupMenu)
  public
    Manager: TCDManagerBase;
    constructor Create(AManager: TCDManagerBase);
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
  UCDClient, UCDStyleTabs, UCDCustomize, UCDManager;

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


{ TCDPopupMenu }

constructor TCDPopupMenu.Create(AManager: TCDManagerBase);
var
  NewMenuItem: TMenuItem;
  NewMenuItem2: TMenuItem;
  I: Integer;
begin
  inherited Create(nil);
  Manager := AManager;

  Name := TCDManager(AManager).DockSite.Name + '_' + 'PopupMenu';

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

procedure TCDPopupMenu.PopupMenuTabsClick(Sender: TObject);
begin
  TCDManager(Manager).DockStyle := dsTabs;
end;

procedure TCDPopupMenu.PopupMenuPopupListClick(Sender: TObject);
begin
  TCDManager(Manager).DockStyle := dsPopupList;
end;

procedure TCDPopupMenu.PopupMenuPopupTabsClick(Sender: TObject);
begin
  TCDManager(Manager).DockStyle := dsPopupTabs;
end;

procedure TCDPopupMenu.PopupMenuCloseClick(Sender: TObject);
var
  Control: TControl;
begin
  if PopupComponent is TPageControl then
  with TPageControl(PopupComponent) do begin
    TForm(TCDClientPanel(TCDManager(Manager).DockPanels[TabIndex]).Control).Close;
  end;
  if PopupComponent is TCDHeader then
  with TCDHeader(PopupComponent) do begin
    TForm(ParentClientPanel.Control).Close;
  end;
end;

procedure TCDPopupMenu.PopupMenuRenameClick(Sender: TObject);
var
  Value: string;
begin
  //ShowMessage(PopupComponent.ClassName);
  if PopupComponent is TPageControl then
  with TPageControl(PopupComponent) do begin
    Value := TCDClientPanel(TCDManager(Manager).DockPanels[TabIndex]).Control.Caption;
    if InputQuery(SRenameWindow, SEnterNewWindowName, False, Value) then begin
      TCDClientPanel(TCDManager(Manager).DockPanels[TabIndex]).Control.Caption := Value;
      Pages[TabIndex].Caption := Value;
    end;
  end;
  if PopupComponent is TCDHeader then
  with TCDHeader(PopupComponent) do begin
    Value := ParentClientPanel.Control.Caption;
    if InputQuery(SRenameWindow, SEnterNewWindowName, False, Value) then begin
      ParentClientPanel.Control.Caption := Value;
      Title.Caption := Value;
    end;
  end;
end;

procedure TCDPopupMenu.PopupMenuPositionAutoClick(Sender: TObject);
begin
  if PopupComponent is TPageControl then
  with TPageControl(PopupComponent) do begin
    TCDStyleTabs(Manager).TabsPos := hpAuto;
  end else
  if PopupComponent is TCDHeader then
  with TCDHeader(PopupComponent) do begin
    TCDManager(Manager).HeaderPos := hpAuto;
  end;
end;

procedure TCDPopupMenu.PopupMenuPositionLeftClick(Sender: TObject);
begin
  if PopupComponent is TPageControl then
  with TPageControl(PopupComponent) do begin
    TCDStyleTabs(Manager).TabsPos := hpLeft;
  end else
  if PopupComponent is TCDHeader then
  with TCDHeader(PopupComponent) do begin
    TCDManager(Manager).HeaderPos := hpLeft;
  end;
end;

procedure TCDPopupMenu.PopupMenuPositionRightClick(Sender: TObject);
begin
  if PopupComponent is TPageControl then
  with TPageControl(PopupComponent) do begin
    TCDStyleTabs(Manager).TabsPos := hpRight;
  end else
  if PopupComponent is TCDHeader then
  with TCDHeader(PopupComponent) do begin
    TCDManager(Manager).HeaderPos := hpRight;
  end;
end;

procedure TCDPopupMenu.PopupMenuPositionTopClick(Sender: TObject);
begin
  if PopupComponent is TPageControl then
  with TPageControl(PopupComponent) do begin
    TCDStyleTabs(Manager).TabsPos := hpTop;
  end else
  if PopupComponent is TCDHeader then
  with TCDHeader(PopupComponent) do begin
    TCDManager(Manager).HeaderPos := hpTop;
  end;
end;

procedure TCDPopupMenu.PopupMenuPositionBottomClick(Sender: TObject);
begin
  if PopupComponent is TPageControl then
  with TPageControl(PopupComponent) do begin
    TCDStyleTabs(Manager).TabsPos := hpBottom;
  end else
  if PopupComponent is TCDHeader then
  with TCDHeader(PopupComponent) do begin
    TCDManager(Manager).HeaderPos := hpBottom;
  end;
end;

procedure TCDPopupMenu.PopupMenuUndockClick(Sender: TObject);
var
  Control: TControl;
begin
  if PopupComponent is TPageControl then
  with TPageControl(PopupComponent) do begin
    Control := TCDClientPanel(TCDManager(Manager).DockPanels[TabIndex]).Control;
  end else
  if PopupComponent is TCDHeader then
  with TCDHeader(PopupComponent) do begin
    Control := ParentClientPanel.Control;
  end else Control := nil;
  if Assigned(Control) then
    Control.ManualFloat(Control.BoundsRect);
end;

procedure TCDPopupMenu.PopupMenuCustomizeClick(Sender: TObject);
begin
  with TCDManager(Manager) do
  if Assigned(Master) and
    Assigned(Master.Customize) then
    TCDCustomize(Master.Customize).Execute;
end;

procedure TCDPopupMenu.PopupMenuListClick(Sender: TObject);
begin
  TCDManager(Manager).DockStyle := dsList;
end;


end.

