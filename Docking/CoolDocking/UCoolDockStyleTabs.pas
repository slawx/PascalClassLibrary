unit UCoolDockStyleTabs;

{$mode Delphi}{$H+}

interface

uses
  Classes, Controls, ExtCtrls, ComCtrls, SysUtils,
  Menus, UCoolDockStyle, Forms, UCoolDockClientPanel;

type

  { TCoolDockStyleTabs }

  TCoolDockStyleTabs = class(TCoolDockStyle)
    MouseDown: Boolean;
    MouseButton: TMouseButton;
    MouseDownSkip: Boolean;
    TabControl: TTabControl;
    TabImageList: TImageList;
    PopupMenuTabs: TPopupMenu;
    procedure TabControlMouseLeave(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
    procedure TabControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TabControlMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    constructor Create(AManager: TObject);
    destructor Destroy; override;
    procedure InsertControl(NewPanel: TCoolDockClientPanel;
      AControl: TControl; InsertAt: TAlign); override;
    procedure UpdateClientSize; override;
  private
    FTabsPos: THeaderPos;
  public
    procedure SetTabsPos(const AValue: THeaderPos);
    procedure PopupMenuTabCloseClick(Sender: TObject);
    property TabsPos: THeaderPos read FTabsPos write SetTabsPos;
  end;

implementation

uses
  UCoolDocking;

{ TCoolDockStyleTabs }

procedure TCoolDockStyleTabs.PopupMenuTabCloseClick(Sender: TObject);
begin
  if TabControl.TabIndex <> -1 then
    TCoolDockClientPanel(TCoolDockManager(Manager).DockPanels[TabControl.TabIndex]).Control.Hide;
end;

procedure TCoolDockStyleTabs.TabControlMouseLeave(Sender: TObject);
begin
(*  if MouseDown then
  if (TabControl.TabIndex <> -1) then begin
    TCoolDockClientPanel(FDockPanels[TabControl.TabIndex]).ClientAreaPanel.DockSite := False;
    DragManager.DragStart(TCoolDockClientPanel(FDockPanels[TabControl.TabIndex]).Control, False, 1);
  end;
  MouseDown := False;
  *)
end;

procedure TCoolDockStyleTabs.TabControlChange(Sender: TObject);
var
  I: Integer;
begin
  // Hide all clients
  with TCoolDockManager(Manager) do
  for I := 0 to DockPanels.Count - 1 do begin
    TCoolDockClientPanel(DockPanels[I]).ClientAreaPanel.Visible := False;
    TCoolDockClientPanel(DockPanels[I]).ClientAreaPanel.Parent := DockSite;
    TCoolDockClientPanel(DockPanels[I]).Control.Align := alClient;
    TCoolDockClientPanel(DockPanels[I]).Control.Visible := False;

    // Workaround for "Cannot focus" error
    TForm(TCoolDockClientPanel(DockPanels[I]).Control).ActiveControl := nil;
  end;
  with TCoolDockManager(Manager) do
  if (TabControl.TabIndex <> -1) and (DockPanels.Count > TabControl.TabIndex) then begin
    with TCoolDockClientPanel(DockPanels[TabControl.TabIndex]), ClientAreaPanel do begin
      Control.Show;
      (*AutoHide.Enable := True;
      if AutoHide.Enable then begin
        //Parent := nil;
        Visible := True;
        if AutoHide.ControlVisible then begin
          AutoHide.Hide;
        end;
        AutoHide.Control := Control;
        AutoHide.Show;
      end else begin
      *)
        //Parent := DockSite;
        Visible := True;
        UpdateClientSize;
//      end;
    end;
  //TCoolDockClientPanel(FDockPanels[TabControl.TabIndex]).Visible := True;
  end;
  MouseDownSkip := True;
end;

procedure TCoolDockStyleTabs.TabControlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not MouseDownSkip then begin
    MouseDown := True;
    MouseButton := Button;
  end;
  MouseDownSkip := False;
end;

procedure TCoolDockStyleTabs.TabControlMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MouseDown := False;
end;

constructor TCoolDockStyleTabs.Create(AManager: TObject);
var
  NewMenuItem: TMenuItem;
  NewMenuItem2: TMenuItem;
  I: Integer;
begin
  inherited;

  (*// Tabs popup

  PopupMenuTabs := TPopupMenu.Create(Manager.DockSite);

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
  PopupMenuTabs.Items.Add(NewMenuItem);     *)

  TabImageList := TImageList.Create(TCoolDockManager(AManager).DockSite); //FDockSite);
  with TabImageList do begin
    Name := TCoolDockManager(Manager).DockSite.Name + '_' + 'ImageList';
  end;
  TabControl := TTabControl.Create(TCoolDockManager(AManager).DockSite); //FDockSite);
  with TabControl do begin
    Parent := TCoolDockManager(Manager).DockSite;
    Name := TCoolDockManager(Manager).DockSite.Name + '_' + 'TabControl';
    Visible := False;
    Align := alTop;
    Height := 24;
    OnChange := TabControlChange;
    PopupMenu := PopupMenuTabs;
    TTabControlNoteBookStrings(Tabs).NoteBook.OnMouseLeave := TabControlMouseLeave;
    TTabControlNoteBookStrings(Tabs).NoteBook.OnMouseDown := TabControlMouseDown;
    TTabControlNoteBookStrings(Tabs).NoteBook.OnMouseUp := TabControlMouseUp;
    OnMouseUp := TabControlMouseUp;
    Images := TabImageList;
  end;
  //TabsPos := hpTop;
  //MoveDuration := 1000; // ms

  TabControl.Visible := True;
  TabControl.Tabs.Clear;
  TabImageList.Clear;
  with TCoolDockManager(Manager) do
  for I := 0 to DockPanels.Count - 1 do begin
    TabControl.Tabs.Add(TCoolDockClientPanel(DockPanels[I]).Control.Caption);
    TabImageList.Add(TCoolDockClientPanel(DockPanels[I]).Header.Icon.Picture.Bitmap, nil);
    if Assigned(TCoolDockClientPanel(DockPanels[I]).Splitter) then
      TCoolDockClientPanel(DockPanels[I]).Splitter.Visible := False;
    TCoolDockClientPanel(DockPanels[I]).ClientAreaPanel.Visible := False;
    TCoolDockClientPanel(DockPanels[I]).Visible := False;
  end;
  TabControlChange(Self);
end;

destructor TCoolDockStyleTabs.Destroy;
begin
  TabControl.Visible := False;
  TabControl.Tabs.Clear;
  inherited Destroy;
end;

procedure TCoolDockStyleTabs.InsertControl(NewPanel: TCoolDockClientPanel;
  AControl: TControl; InsertAt: TAlign);
begin
  inherited;
  TabControl.Tabs.Add(AControl.Caption);
  TabImageList.Add(NewPanel.Header.Icon.Picture.Bitmap, nil);
  if Assigned(NewPanel.Splitter) then
    NewPanel.Splitter.Visible := False;
  NewPanel.ClientAreaPanel.Visible := False;
  NewPanel.Visible := False;
  TabControlChange(Self);
end;

procedure TCoolDockStyleTabs.UpdateClientSize;
var
  I: Integer;
begin
  inherited UpdateClientSize;
  with TCoolDockManager(Manager) do
  for I := 0 to DockPanels.Count - 1 do begin
    TCoolDockClientPanel(DockPanels[I]).ClientAreaPanel.Width := DockSite.Width;
    TCoolDockClientPanel(DockPanels[I]).ClientAreaPanel.Height := DockSite.Height - TabControl.Height;
    //TCoolDockClientPanel(FDockPanels[I]).DockPanelPaint(Self);
  end;
end;

procedure TCoolDockStyleTabs.SetTabsPos(const AValue: THeaderPos);
begin
  (*if FTabsPos = AValue then Exit;
  FTabsPos := AValue;
  with TabControl do
  case AValue of
    hpAuto, hpTop: begin
      Align := alTop;
      TabPosition := tpTop;
      Height := GrabberSize;
    end;
    hpLeft: begin
      Align := alLeft;
      TabPosition := tpLeft;
      Width := GrabberSize;
    end;
    hpRight: begin
      Align := alRight;
      TabPosition := tpRight;
      Width := GrabberSize;
    end;
    hpBottom: begin
      Align := alBottom;
      TabPosition := tpBottom;
      Height := GrabberSize;
    end;
  end;       *)
end;


end.

