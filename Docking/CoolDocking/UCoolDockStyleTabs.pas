unit UCoolDockStyleTabs;

{$mode Delphi}{$H+}

interface

uses
  Classes, Controls, ExtCtrls, ComCtrls, SysUtils, Dialogs,
  Menus, UCoolDockStyle, Forms, UCoolDockClientPanel;

type

  { TCoolDockStyleTabs }

  TCoolDockStyleTabs = class(TCoolDockStyle)
    MouseDown: Boolean;
    MouseButton: TMouseButton;
    MouseDownSkip: Boolean;
    PageControl: TPageControl;
    TabImageList: TImageList;
    procedure TabControlMouseLeave(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
    procedure TabControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TabControlMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure InsertControl(NewPanel: TCoolDockClientPanel;
      AControl: TControl; InsertAt: TAlign); override;
    procedure UpdateClientSize; override;
  private
    FTabsPos: THeaderPos;
    procedure InsertControlNoUpdate(NewPanel: TCoolDockClientPanel;
      AControl: TControl; InsertAt: TAlign);
  public
    constructor Create(AManager: TObject);
    procedure SetVisible(const AValue: Boolean); override;
    destructor Destroy; override;
    procedure ChangeVisible(Control: TWinControl; Visible: Boolean); override;
    procedure Switch(Index: Integer); override;
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
  if Assigned(PageControl.ActivePage) then
    TCoolDockClientPanel(TCoolDockManager(Manager).DockPanels[PageControl.TabIndex]).Control.Hide;
end;

procedure TCoolDockStyleTabs.TabControlMouseLeave(Sender: TObject);
begin
  if MouseDown then
  with TCoolDockManager(Manager) do
  if Assigned(PageControl.ActivePage) then begin
    TCoolDockClientPanel(DockPanels[PageControl.TabIndex]).ClientAreaPanel.DockSite := False;
    DragManager.DragStart(TCoolDockClientPanel(DockPanels[PageControl.TabIndex]).Control, False, 1);
  end;
  MouseDown := False;
end;

procedure TCoolDockStyleTabs.TabControlChange(Sender: TObject);
var
  I: Integer;
begin
  // Hide all clients
  with TCoolDockManager(Manager) do
  for I := 0 to DockPanels.Count - 1 do
    if TCoolDockClientPanel(DockPanels[I]).Control.Visible
    //and (PageControl.TabIndex <> I)
    then
    begin
      TCoolDockClientPanel(DockPanels[I]).Control.Tag := 1;
      TCoolDockClientPanel(DockPanels[I]).Control.Hide;
      TCoolDockClientPanel(DockPanels[I]).ClientAreaPanel.Hide;
      TCoolDockClientPanel(DockPanels[I]).ClientAreaPanel.Parent := PageControl.Pages[I];
      //TCoolDockClientPanel(DockPanels[I]).ClientAreaPanel.Parent := DockSite;
      TCoolDockClientPanel(DockPanels[I]).Control.Align := alClient;
      //TCoolDockClientPanel(DockPanels[I]).Control.Parent :=
      //  TCoolDockClientPanel(DockPanels[I]).ClientAreaPanel;
      //ShowMessage(TCoolDockClientPanel(DockPanels[I]).Control.ClassName);
      Application.ProcessMessages;

      // Workaround for "Cannot focus" error
      TForm(TCoolDockClientPanel(DockPanels[I]).Control).ActiveControl := nil;
    end;

  // Show selected
  with TCoolDockManager(Manager) do
  if (PageControl.TabIndex <> -1) and (DockPanels.Count > PageControl.TabIndex)
//  and not TCoolDockClientPanel(DockPanels[PageControl.TabIndex]).Control.Visible
  then begin
    with TCoolDockClientPanel(DockPanels[PageControl.TabIndex]), ClientAreaPanel do begin
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
        //Show;
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
  NewTabSheet: TTabSheet;
begin
  inherited;

  TabImageList := TImageList.Create(TCoolDockManager(AManager).DockSite); //FDockSite);
  with TabImageList do begin
    Name := TCoolDockManager(Manager).DockSite.Name + '_' + 'ImageList';
  end;
  PageControl := TPageControl.Create(TCoolDockManager(AManager).DockSite); //FDockSite);
  with PageControl do begin
    Parent := TCoolDockManager(Manager).DockSite;
    Name := TCoolDockManager(Manager).DockSite.Name + '_' + 'TabControl';
    Visible := False;
    Align := alTop;
    //Height := 24;
    Align := alClient;
    OnChange := TabControlChange;
    MultiLine := True;
    PopupMenu := TCoolDockManager(Manager).PopupMenu;
    //TTabControlNoteBookStrings(Tabs).NoteBook.OnMouseLeave := TabControlMouseLeave;
    //TTabControlNoteBookStrings(Tabs).NoteBook.OnMouseDown := TabControlMouseDown;
    //TTabControlNoteBookStrings(Tabs).NoteBook.OnMouseUp := TabControlMouseUp;
    OnMouseUp := TabControlMouseUp;
    Images := TabImageList;
  end;
  //TabsPos := hpTop;
  //MoveDuration := 1000; // ms

  PageControl.Visible := True;
  //TabImageList.Clear;
  with TCoolDockManager(Manager) do
  for I := 0 to DockPanels.Count - 1 do
    Self.InsertControlNoUpdate(TCoolDockClientPanel(DockPanels[I]),
      TCoolDockClientPanel(DockPanels[I]).Control, alNone);
  TabControlChange(Self);
end;

destructor TCoolDockStyleTabs.Destroy;
begin
  PageControl.Free;
  TabImageList.Free;
  inherited Destroy;
end;

procedure TCoolDockStyleTabs.Switch(Index: Integer);
begin
  inherited Switch(Index);
  PageControl.TabIndex := Index;
end;

procedure TCoolDockStyleTabs.InsertControlNoUpdate(NewPanel: TCoolDockClientPanel;
  AControl: TControl; InsertAt: TAlign);
var
  NewTabSheet: TTabSheet;
begin
  inherited;
  if AControl.Visible then begin
    NewTabSheet := TTabSheet.Create(PageControl);
    NewTabSheet.PageControl := PageControl;
    NewTabSheet.Caption := AControl.Caption;
    NewTabSheet.ImageIndex := TabImageList.Count;
    TabImageList.Add(NewPanel.Header.Icon.Picture.Bitmap, nil);
    if Assigned(NewPanel.Splitter) then
      NewPanel.Splitter.Visible := False;
    NewPanel.ClientAreaPanel.Visible := False;
    NewPanel.Visible := False;
    //NewPanel.Parent := NewTabSheet;
  end;
end;

procedure TCoolDockStyleTabs.InsertControl(NewPanel: TCoolDockClientPanel;
  AControl: TControl; InsertAt: TAlign);
var
  NewTabSheet: TTabSheet;
begin
  inherited;
  InsertControlNoUpdate(NewPanel, AControl, InsertAt);
  TabControlChange(Self);
end;

procedure TCoolDockStyleTabs.UpdateClientSize;
var
  I: Integer;
begin
  inherited UpdateClientSize;
  with TCoolDockManager(Manager) do
  for I := 0 to DockPanels.Count - 1 do begin
    //TCoolDockClientPanel(DockPanels[I]).ClientAreaPanel.Width := DockSite.Width;
    //TCoolDockClientPanel(DockPanels[I]).ClientAreaPanel.Height := DockSite.Height - PageControl.Height;
    //TCoolDockClientPanel(FDockPanels[I]).DockPanelPaint(Self);
  end;
end;

procedure TCoolDockStyleTabs.SetVisible(const AValue: Boolean);
begin
  inherited SetVisible(AValue);
  with TCoolDockManager(Manager) do
    if (PageControl.TabIndex >= 0) and (PageControl.TabIndex < DockPanels.Count) then
      with TCoolDockClientPanel(DockPanels[PageControl.TabIndex]) do begin
        //Show;
        if AValue and (not Control.Visible) and (Control.Tag = 1)  then begin
          Control.Show;
          Control.Tag := 0;
        end;
        //TabControl.Show;
        //ClientAreaPanel.Show;
      end;
end;

procedure TCoolDockStyleTabs.ChangeVisible(Control: TWinControl; Visible: Boolean);
var
  I: Integer;
begin
  inherited;
  if not Visible then begin
    //if Assigned(TWinControl(Control).DockManager) then
    //with TCoolDockManager(TWinControl(Control).DockManager) do
    if Assigned(Manager) then
    with TCoolDockManager(Manager) do
    begin
//    ShowMessage(IntToStr(TabControl.TabIndex) + ' ' + IntToStr(DockPanels.Count));
//    TabControl.Tabs[0].;
//    if (TabControl.TabIndex >= 0) and (TabControl.TabIndex < DockPanels.Count) then begin
//      TCoolDockClientPanel(DockPanels[TabControl.TabIndex]).Show;
//      TCoolDockClientPanel(DockPanels[TabControl.TabIndex]).Control.Show;
//    end;
    //    ShowMessage(IntToStr(DockPanels.Count));
        //TabImageList.Delete(PageControl.Tabs.IndexOf(Control.Caption));

        I := DockPanels.IndexOf(FindControlInPanels(Control));
        if Control.Tag = 0 then
        if I <> -1 then
  //        Control.Hide;
          PageControl.Page[I].TabVisible := False;
        Control.Tag := 0;
//      end;
    end;
  end else
  begin
//    if Assigned(TWinControl(Control).DockManager) then
//    with TCoolDockManager(TWinControl(Control).DockManager) do
    if Assigned(Manager) then
    with TCoolDockManager(Manager) do
    begin
//      if Control.Tag = 0 then begin
        I := DockPanels.IndexOf(FindControlInPanels(Control));
        //if  then
        if I <> -1 then
          PageControl.Page[I].TabVisible := True;
//      TabImageList.Add(TCoolDockClientPanel(TCoolDockManager(Manager).FindControlInPanels(Control)).Header.Icon.Picture.Bitmap, nil);
//      TabControl.Tabs.Add(Control.Caption);

//      end;
    end;
  end;
end;

procedure TCoolDockStyleTabs.SetTabsPos(const AValue: THeaderPos);
begin
  if FTabsPos = AValue then Exit;
  FTabsPos := AValue;
  with PageControl do
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
  end;
end;


end.

