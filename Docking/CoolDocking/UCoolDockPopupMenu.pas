unit UCoolDockPopupMenu;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Menus, Controls, Dialogs;

type

  { TCoolDockPopupMenu }

  TCoolDockPopupMenu = class(TPopupMenu)
  public
    Manager: TObject; // TCoolDockManager
    constructor Create(AManager: TObject);
    procedure PopupMenuListClick(Sender: TObject);
    procedure PopupMenuTabsClick(Sender: TObject);
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
  UCoolDocking;

resourcestring
  SDockStyle = 'Style';
  SDockList = 'List';
  SDockTabs = 'Tabs';
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

  (*  // Tabs popup

  PopupMenuTabs := TPopupMenu.Create(FDockSite);
  PopupMenuTabs.Name := ADockSite.Name + '_' + 'PopupMenuTabs';

  NewMenuItem := TMenuItem.Create(PopupMenuTabs);
  NewMenuItem.Caption := SDockStyle;
  PopupMenuTabs.Items.Add(NewMenuItem);

  NewMenuItem2 := TMenuItem.Create(NewMenuItem);
  NewMenuItem2.Caption := SDockList;
  NewMenuItem2.OnClick := PopupMenuListClick;
  NewMenuItem.Add(NewMenuItem2);

  NewMenuItem2 := TMenuItem.Create(NewMenuItem);
  NewMenuItem2.Caption := SDockTabs;
  NewMenuItem2.OnClick := PopupMenuTabsClick;
  NewMenuItem.Add(NewMenuItem2);

  NewMenuItem := TMenuItem.Create(PopupMenuTabs);
  NewMenuItem.Caption := SPosition;
  PopupMenuTabs.Items.Add(NewMenuItem);

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

  NewMenuItem := TMenuItem.Create(PopupMenuTabs);
  NewMenuItem.Caption := SCloseForm;
  NewMenuItem.OnClick := PopupMenuCloseClick;
  PopupMenuTabs.Items.Add(NewMenuItem);

  NewMenuItem := TMenuItem.Create(PopupMenuTabs);
  NewMenuItem.Caption := SRenameForm;
  NewMenuItem.OnClick := PopupMenuRenameClick;
  PopupMenuTabs.Items.Add(NewMenuItem);

  NewMenuItem := TMenuItem.Create(PopupMenuTabs);
  NewMenuItem.Caption := SUndock;
  NewMenuItem.OnClick := PopupMenuUndockClick;
  PopupMenuTabs.Items.Add(NewMenuItem);

  NewMenuItem := TMenuItem.Create(PopupMenuTabs);
  NewMenuItem.Caption := SCustomize;
  NewMenuItem.OnClick := PopupMenuCustomizeClick;
  PopupMenuTabs.Items.Add(NewMenuItem);

  // Header popup

  PopupMenuHeader := TPopupMenu.Create(FDockSite);
  PopupMenuHeader.Name := ADockSite.Name + '_' + 'PopupMenuHeader';

  NewMenuItem := TMenuItem.Create(PopupMenuHeader);
  NewMenuItem.Caption := SDockStyle;
  PopupMenuHeader.Items.Add(NewMenuItem);

  NewMenuItem2 := TMenuItem.Create(NewMenuItem);
  NewMenuItem2.Caption := SDockList;
  NewMenuItem2.OnClick := PopupMenuListClick;
  NewMenuItem.Add(NewMenuItem2);

  NewMenuItem2 := TMenuItem.Create(NewMenuItem);
  NewMenuItem2.Caption := SDockTabs;
  NewMenuItem2.OnClick := PopupMenuTabsClick;
  NewMenuItem.Add(NewMenuItem2);

  NewMenuItem := TMenuItem.Create(PopupMenuHeader);
  NewMenuItem.Caption := SPosition;
  PopupMenuHeader.Items.Add(NewMenuItem);

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

  NewMenuItem := TMenuItem.Create(PopupMenuHeader);
  NewMenuItem.Caption := SCloseForm;
  NewMenuItem.OnClick := PopupMenuCloseClick;
  PopupMenuHeader.Items.Add(NewMenuItem);

  NewMenuItem := TMenuItem.Create(PopupMenuHeader);
  NewMenuItem.Caption := SRenameForm;
  NewMenuItem.OnClick := PopupMenuRenameClick;
  PopupMenuHeader.Items.Add(NewMenuItem);

  NewMenuItem := TMenuItem.Create(PopupMenuHeader);
  NewMenuItem.Caption := SUndock;
  NewMenuItem.OnClick := PopupMenuUndockClick;
  PopupMenuHeader.Items.Add(NewMenuItem);

  NewMenuItem := TMenuItem.Create(PopupMenuHeader);
  NewMenuItem.Caption := SCustomize;
  NewMenuItem.OnClick := PopupMenuCustomizeClick;
  PopupMenuHeader.Items.Add(NewMenuItem);
  *)

end;

procedure TCoolDockPopupMenu.PopupMenuTabsClick(Sender: TObject);
begin
  TCoolDockManager(Manager).DockStyle := dsTabs;
end;

procedure TCoolDockPopupMenu.PopupMenuCloseClick(Sender: TObject);
var
  Control: TControl;
begin
  Control := FindLCLControl(Mouse.CursorPos);
  if Assigned(Control) then
    ShowMessage(Control.ClassName);
//  DockSiteTForm(TCoolDockManager(TControl(Sender).Parent.Parent.Parent.DockManager).FDockSite).Close;
end;

procedure TCoolDockPopupMenu.PopupMenuRenameClick(Sender: TObject);
var
  Value: string;
begin
  Value := TCoolDockManager(Manager).DockSite.Parent.Caption;
  if InputQuery(SRenameWindow, SEnterNewWindowName, False, Value) then
    TCoolDockManager(Manager).DockSite.Parent.Caption := Value;
end;

procedure TCoolDockPopupMenu.PopupMenuPositionAutoClick(Sender: TObject);
begin
  //TabsPos := hpAuto;
end;

procedure TCoolDockPopupMenu.PopupMenuPositionLeftClick(Sender: TObject);
begin
  //TabsPos := hpLeft;
end;

procedure TCoolDockPopupMenu.PopupMenuPositionRightClick(Sender: TObject);
begin
  //TabsPos := hpRight;
end;

procedure TCoolDockPopupMenu.PopupMenuPositionTopClick(Sender: TObject);
begin
  //TabsPos := hpTop;
end;

procedure TCoolDockPopupMenu.PopupMenuPositionBottomClick(Sender: TObject);
begin
  //TabsPos := hpBottom;
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

