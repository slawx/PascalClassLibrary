unit UCDManagerTabs;

{$mode Delphi}{$H+}

interface

uses
  Classes, Controls, ExtCtrls, ComCtrls, SysUtils, Dialogs,
  Menus, Forms, UCDClientPanel, UCDCommon, UCDManager;

type

  { TCDStyleTabs }

  TCDStyleTabs = class(TCDManager)
  public
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
    procedure InsertControlPanel(AControl: TControl; InsertAt: TAlign;
      DropCtl: TControl); override;
    procedure UpdateClientSize; override;
  private
    FTabsPos: THeaderPos;
    procedure InsertControlNoUpdate(AControl: TControl; InsertAt: TAlign);
    procedure RemoveControl(Control: TControl); override;
  public
    constructor Create(ADockSite: TWinControl);
    procedure DoSetVisible(const AValue: Boolean);
    destructor Destroy; override;
    procedure ChangeVisible(Control: TWinControl; Visible: Boolean); override;
    procedure Switch(Index: Integer); override;
    procedure SetTabsPos(const AValue: THeaderPos);
    procedure PopupMenuTabCloseClick(Sender: TObject);
    property TabsPos: THeaderPos read FTabsPos write SetTabsPos;
  end;

implementation

uses
  UCDClient;

{ TCDStyleTabs }

procedure TCDStyleTabs.PopupMenuTabCloseClick(Sender: TObject);
begin
  if Assigned(PageControl.ActivePage) then
    TCDClientPanel(DockPanels[PageControl.TabIndex]).Control.Hide;
end;

procedure TCDStyleTabs.TabControlMouseLeave(Sender: TObject);
begin
  if MouseDown then
  if Assigned(PageControl.ActivePage) then begin
    TCDClientPanel(DockPanels[PageControl.TabIndex]).ClientAreaPanel.DockSite := False;
    DragManager.DragStart(TCDClientPanel(DockPanels[PageControl.TabIndex]).Control, False, 1);
  end;
  MouseDown := False;
end;

procedure TCDStyleTabs.TabControlChange(Sender: TObject);
var
  I: Integer;
begin
  // Hide all clients
  for I := 0 to DockPanels.Count - 1 do
    if TCDClientPanel(DockPanels[I]).Control.Visible
    //and (PageControl.TabIndex <> I)
    then
    begin
      TCDClientPanel(DockPanels[I]).Control.Tag := Integer(dhtTemporal);
      TCDClientPanel(DockPanels[I]).Control.Hide;
      TCDClientPanel(DockPanels[I]).ClientAreaPanel.Hide;
      TCDClientPanel(DockPanels[I]).ClientAreaPanel.Parent := PageControl.Pages[I];
      //TCDClientPanel(DockPanels[I]).ClientAreaPanel.Parent := DockSite;
      TCDClientPanel(DockPanels[I]).Control.Align := alClient;
      //TCDClientPanel(DockPanels[I]).Control.Parent :=
      //  TCDClientPanel(DockPanels[I]).ClientAreaPanel;
      //ShowMessage(TCDClientPanel(DockPanels[I]).Control.ClassName);
      Application.ProcessMessages;

      // Workaround for "Cannot focus" error
      TForm(TCDClientPanel(DockPanels[I]).Control).ActiveControl := nil;
    end;

  // Show selected
  if (PageControl.TabIndex <> -1) and (DockPanels.Count > PageControl.TabIndex)
//  and not TCDClientPanel(DockPanels[PageControl.TabIndex]).Control.Visible
  then begin
    with TCDClientPanel(DockPanels[PageControl.TabIndex]), ClientAreaPanel do begin
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
  //TCDClientPanel(FDockPanels[TabControl.TabIndex]).Visible := True;
  end;
  MouseDownSkip := True;
end;

procedure TCDStyleTabs.TabControlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not MouseDownSkip then begin
    MouseDown := True;
    MouseButton := Button;
  end;
  MouseDownSkip := False;
end;

procedure TCDStyleTabs.TabControlMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MouseDown := False;
end;

constructor TCDStyleTabs.Create(ADockSite: TWinControl);
var
  NewMenuItem: TMenuItem;
  NewMenuItem2: TMenuItem;
  I: Integer;
  NewTabSheet: TTabSheet;
begin
  inherited;
  TabImageList := TImageList.Create(ADockSite); //FDockSite);
  with TabImageList do begin
    Name := DockSite.Name + 'ImageList';
  end;
  PageControl := TPageControl.Create(ADockSite); //FDockSite);
  with PageControl do begin
    Parent := ADockSite;
    Name := Self.DockSite.Name + 'TabControl';
    Visible := False;
    Align := alTop;
    //Height := 24;
    Align := alClient;
    OnChange := TabControlChange;
    MultiLine := True;
    PopupMenu := Self.PopupMenu;
    OnMouseLeave := TabControlMouseLeave;
    OnMouseDown := TabControlMouseDown;
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
  for I := 0 to DockPanels.Count - 1 do
    Self.InsertControlNoUpdate(TCDClientPanel(DockPanels[I]).Control, alNone);
  TabControlChange(Self);
end;

destructor TCDStyleTabs.Destroy;
begin
  PageControl.Free;
  TabImageList.Free;
  inherited Destroy;
end;

procedure TCDStyleTabs.Switch(Index: Integer);
begin
  PageControl.TabIndex := Index;
end;

procedure TCDStyleTabs.InsertControlNoUpdate(AControl: TControl; InsertAt: TAlign);
var
  NewTabSheet: TTabSheet;
  NewPanel: TCDClientPanel;
begin
  inherited;
  begin
    NewPanel := TCDClientPanel.Create(nil);
    with NewPanel do begin
      Parent := Self.DockSite;
      OwnerDockManager := Self;
      if DockStyle = dsList then Visible := True;
      Align := alClient;
      Header.PopupMenu := Self.PopupMenu;
      //PopupMenu.Parent := Self.DockSite;
    end;
    if (AControl is TForm) and Assigned((AControl as TForm).Icon) then
      NewPanel.Header.Icon.Picture.Assign((AControl as TForm).Icon);

    NewPanel.Control := AControl;
    AControl.AddHandlerOnVisibleChanged(NewPanel.VisibleChange);
    AControl.Parent := NewPanel.ClientAreaPanel;
    AControl.Align := alClient;
    if (InsertAt = alTop) or (InsertAt = alLeft) then
      DockPanels.Insert(0, NewPanel)
      else DockPanels.Add(NewPanel);

  end;

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

procedure TCDStyleTabs.RemoveControl(Control: TControl);
begin
  inherited RemoveControl(Control);
end;

procedure TCDStyleTabs.InsertControlPanel(AControl: TControl; InsertAt: TAlign;
  DropCtl: TControl);
var
  NewTabSheet: TTabSheet;
begin
  inherited;
  InsertControlNoUpdate(AControl, InsertAt);
  TabControlChange(Self);
end;

procedure TCDStyleTabs.UpdateClientSize;
var
  I: Integer;
begin
  inherited UpdateClientSize;
  for I := 0 to DockPanels.Count - 1 do begin
    //TCDClientPanel(DockPanels[I]).ClientAreaPanel.Width := DockSite.Width;
    //TCDClientPanel(DockPanels[I]).ClientAreaPanel.Height := DockSite.Height - PageControl.Height;
    //TCDClientPanel(FDockPanels[I]).DockPanelPaint(Self);
  end;
end;

procedure TCDStyleTabs.DoSetVisible(const AValue: Boolean);
begin
    if (PageControl.TabIndex >= 0) and (PageControl.TabIndex < DockPanels.Count) then
      with TCDClientPanel(DockPanels[PageControl.TabIndex]) do begin
        //Show;
        //ShowMessage(IntToStr(Control.Tag));
        if AValue and (not Control.Visible) and (Control.Tag = Integer(dhtTemporal))  then begin
          Control.Show;
          Control.Tag := Integer(dhtPermanent);
        end;
        //TabControl.Show;
        //ClientAreaPanel.Show;
      end;
end;

procedure TCDStyleTabs.ChangeVisible(Control: TWinControl; Visible: Boolean);
var
  I: Integer;
begin
  inherited;
  if not Visible then begin
    //if Assigned(TWinControl(Control).DockManager) then
    //with TCDManager(TWinControl(Control).DockManager) do
    begin
//    ShowMessage(IntToStr(TabControl.TabIndex) + ' ' + IntToStr(DockPanels.Count));
//    TabControl.Tabs[0].;
//    if (TabControl.TabIndex >= 0) and (TabControl.TabIndex < DockPanels.Count) then begin
//      TCDClientPanel(DockPanels[TabControl.TabIndex]).Show;
//      TCDClientPanel(DockPanels[TabControl.TabIndex]).Control.Show;
//    end;
    //    ShowMessage(IntToStr(DockPanels.Count));
        //TabImageList.Delete(PageControl.Tabs.IndexOf(Control.Caption));

        I := DockPanels.IndexOf(FindControlInPanels(Control));
        if Control.Tag = Integer(dhtPermanent) then
        if I <> -1 then
  //        Control.Hide;
          PageControl.Page[I].TabVisible := False;
        //Control.Tag := 0;
//      end;
    end;
  end else
  begin
//    if Assigned(TWinControl(Control).DockManager) then
//    with TCDManager(TWinControl(Control).DockManager) do
    begin
//      if Control.Tag = 0 then begin
        I := DockPanels.IndexOf(FindControlInPanels(Control));
        //if  then
        if I <> -1 then
          PageControl.Page[I].TabVisible := True;
//      TabImageList.Add(TCDClientPanel(TCDManager(Manager).FindControlInPanels(Control)).Header.Icon.Picture.Bitmap, nil);
//      TabControl.Tabs.Add(Control.Caption);

//      end;
    end;
  end;
end;

procedure TCDStyleTabs.SetTabsPos(const AValue: THeaderPos);
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

