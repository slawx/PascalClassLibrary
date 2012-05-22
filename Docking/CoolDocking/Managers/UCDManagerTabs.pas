unit UCDManagerTabs;

{$mode Delphi}{$H+}

interface

uses
  Classes, Controls, ExtCtrls, ComCtrls, SysUtils, Dialogs, Contnrs,
  Menus, Forms, UCDCommon, UCDManager, UCDConjoinForm,
  LCLType, LMessages, Graphics;

type

  { TCDManagerTabsItem }

  TCDManagerTabsItem = class(TCDManagerItem)
    IconImage: TImage;
    TabSheet: TTabSheet;
    constructor Create; override;
    destructor Destroy; override;
    procedure VisibleChange(Sender: TObject); override;
  end;

  { TCDManagerTabs }

  TCDManagerTabs = class(TCDManager)
  private
    MouseDown: Boolean;
    MouseButton: TMouseButton;
    FDockItems: TObjectList; // TList<TCDManagerRegionsItem>
    FUpdating: Boolean;
    procedure TabControlMouseLeave(Sender: TObject);
    procedure TabControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TabControlMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure InsertControlPanel(AControl: TControl; InsertAt: TAlign;
      DropCtl: TControl); override;
    function FindControlInPanels(Control: TControl): TCDManagerItem; override;
    function GetHeaderPos: THeaderPos; override;
    function FindTabSheet(TabSheet: TTabSheet): TCDManagerTabsItem;
  public
    MouseDownSkip: Boolean;
    TabImageList: TImageList;
    PageControl: TPageControl;
    procedure BringToFront; override;
    procedure Update; override;
    procedure SetHeaderPos(const AValue: THeaderPos); override;
    procedure InsertControlNoUpdate(Control: TControl; InsertAt: TAlign); virtual;
    procedure RemoveControl(Control: TControl); override;
    constructor Create(ADockSite: TWinControl); override;
    destructor Destroy; override;
    procedure TabControlChange(Sender: TObject); virtual;
    procedure PaintSite(DC: HDC); override;
    procedure Switch(Index: Integer); override;
    procedure PopupMenuTabCloseClick(Sender: TObject);
    procedure SetVisible(const AValue: Boolean); override;
    property DockItems: TObjectList read FDockItems write FDockItems;
  end;

implementation

uses
  UCDClient;

{ TCDManagerTabsItem }

constructor TCDManagerTabsItem.Create;
begin
  IconImage := TImage.Create(nil);
end;

destructor TCDManagerTabsItem.Destroy;
begin
  IconImage.Free;
  inherited Destroy;
end;

procedure TCDManagerTabsItem.VisibleChange(Sender: TObject);
var
  ControlVisible: Boolean;
  Temp: TControl;
  Temp2: TControl;
begin
  DebugLog('TCDManagerTabsItem.VisibleChange');
  with TCDManagerTabs(Manager) do begin
    if TControl(Sender).Visible then begin
      Update;
      Switch(DockItems.IndexOf(FindControlInPanels(TControl(Sender))));
      //TCDManagerTabsItem(DockItems[DockItems.IndexOf(
      //  FindControlInPanels(TControl(Sender)))]).HideType := dhtPermanent;
    end else Update;
  end;

  // Show current dock clients in parent dock sites
  if TControl(Sender).Visible then
    if Assigned(TControl(Sender).HostDockSite) then begin
      //TControl(Sender).HostDockSite.DockManager.;
      TControl(Sender).HostDockSite.Visible := True;
    end;
end;

{ TCDManagerTabs }

function TCDManagerTabs.FindControlInPanels(Control: TControl
  ): TCDManagerItem;
var
  I: Integer;
begin
  I := 0;
  while (I < FDockItems.Count) and
    (TCDManagerItem(FDockItems[I]).Control <> Control) do Inc(I);
  if I < FDockItems.Count then Result := TCDManagerItem(FDockItems[I])
    else Result := nil;
end;

procedure TCDManagerTabs.PopupMenuTabCloseClick(Sender: TObject);
begin
  if Assigned(PageControl.ActivePage) then
    TCDManagerItem(DockItems[PageControl.TabIndex]).Control.Hide;
end;

procedure TCDManagerTabs.SetVisible(const AValue: Boolean);
begin
  inherited;
  if (PageControl.TabIndex >= 0) and (PageControl.TabIndex < DockItems.Count) then
    with TCDManagerItem(DockItems[PageControl.TabIndex]) do
      TCDManager(Control.DockManager).DockSiteVisible := True;
end;

procedure TCDManagerTabs.TabControlMouseLeave(Sender: TObject);
begin
  if MouseDown then
  if Assigned(PageControl.ActivePage) and not Locked then begin
    //TCDManagerItem(DockItems[PageControl.TabIndex]).ClientAreaPanel.DockSite := False;
    DragManager.DragStart(TCDManagerItem(DockItems[PageControl.TabIndex]).Control, False, 1);
  end;
  MouseDown := False;
end;

procedure TCDManagerTabs.TabControlChange(Sender: TObject);
begin
  DebugLog('TCDManagerTabs.TabControlChange ' + IntToStr(PageControl.TabIndex));
  Update;
  MouseDownSkip := True;
end;

procedure TCDManagerTabs.TabControlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not MouseDownSkip then begin
    MouseDown := True;
    MouseButton := Button;
  end;
  MouseDownSkip := False;
end;

procedure TCDManagerTabs.TabControlMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MouseDown := False;
end;

constructor TCDManagerTabs.Create(ADockSite: TWinControl);
var
  NewMenuItem: TMenuItem;
  NewMenuItem2: TMenuItem;
  I: Integer;
  NewTabSheet: TTabSheet;
begin
  inherited;
  FDockStyle := dsTabs;
  FDockItems := TObjectList.Create;

  TabImageList := TImageList.Create(ADockSite); //FDockSite);
  with TabImageList do begin
    Name := GetUniqueName(DockSite.Name + 'ImageList');
  end;
  PageControl := TPageControl.Create(ADockSite); //FDockSite);
  with PageControl do begin
    Parent := ADockSite;
    Name := GetUniqueName(Self.DockSite.Name + 'TabControl');
    Visible := True;
    //Align := alTop;
    //Height := 24;
    //Color := clBlue;
    Align := alClient;
    OnChange := TabControlChange;
    MultiLine := True;
    AutoSize := True;
    PopupMenu := Self.PopupMenu;
    OnMouseLeave := TabControlMouseLeave;
    OnMouseDown := TabControlMouseDown;
    //TTabControlNoteBookStrings(Tabs).NoteBook.OnMouseLeave := TabControlMouseLeave;
    //TTabControlNoteBookStrings(Tabs).NoteBook.OnMouseDown := TabControlMouseDown;
    //TTabControlNoteBookStrings(Tabs).NoteBook.OnMouseUp := TabControlMouseUp;
    OnMouseUp := TabControlMouseUp;
    Images := TabImageList;
  end;

  //PageControl.Visible := True;
  //TabImageList.Clear;
  for I := 0 to ADockSite.DockClientCount - 1 do
    InsertControlNoUpdate(ADockSite.DockClients[I], alNone);
  //TabControlChange(Self);
  //TCDManagerTabs(Self).TabControlChange(Self);
end;

destructor TCDManagerTabs.Destroy;
begin
  FDockItems.Free;
  PageControl.Free;
  TabImageList.Free;
  inherited Destroy;
end;

procedure TCDManagerTabs.PaintSite(DC: HDC);
var
  I: Integer;
begin
  inherited PaintSite(DC);
  //PageControl.Invalidate;
end;

procedure TCDManagerTabs.Switch(Index: Integer);
begin
  DebugLog('TCDManagerTabs.Switch ' + IntToStr(Index));
  PageControl.TabIndex := Index;
end;

procedure TCDManagerTabs.InsertControlNoUpdate(Control: TControl; InsertAt: TAlign);
var
  NewTabSheet: TTabSheet;
  NewItem: TCDManagerTabsItem;
begin
  inherited;
  begin
    NewItem := TCDManagerTabsItem.Create;
    with NewItem do begin
      Manager := Self;
    end;
    if (Control is TForm) and Assigned((Control as TForm).Icon) then
      NewItem.IconImage.Picture.Assign((Control as TForm).Icon);

    NewItem.Control := TWinControl(Control);
    //NewItem.HideType := dhtTemporal;
    Control.AddHandlerOnVisibleChanged(NewItem.VisibleChange);
    Control.AddHandlerOnVisibleChanging(NewItem.VisibleChanging);
    //AControl.Parent := NewItem.ClientAreaPanel;
    Control.Align := alClient;
    if (InsertAt = alTop) or (InsertAt = alLeft) then
      DockItems.Insert(0, NewItem)
      else DockItems.Add(NewItem);
  end;
end;

procedure TCDManagerTabs.RemoveControl(Control: TControl);
var
  ManagerItem: TCDManagerItem;
  ClientCount: Integer;
begin
  ManagerItem := FindControlInPanels(Control);
  if Assigned(ManagerItem) then begin
    Control.RemoveHandlerOnVisibleChanged(ManagerItem.VisibleChange);
    Control.RemoveHandlerOnVisibleChanging(ManagerItem.VisibleChanging);
  end; //else raise Exception.Create(Format('Control %s not found in DockItems', [Control.Name]));

  //ManagerItem.Control.Visible := False;
  ManagerItem.Control.Parent := nil;
  DockItems.Remove(ManagerItem);
  ClientCount := DockItems.Count;

  //if TCDManager(Manager).DockSite.DockClientCount = 2 then FDockDirection := ddNone;
  if ClientCount = 1 then begin
    // Last removed control => Free parent if it is TCDConjoinForm
    if Self.DockSite is TCDConjoinForm then
    with TCDConjoinForm(Self.DockSite) do begin
      if Assigned(Parent) then begin
        TCDManagerItem(DockItems[0]).Control.ManualDock(HostDockSite);
      end else TCDManagerItem(DockItems[0]).Control.ManualFloat(Rect(Left, Top, Left + Width, Top + Height));
      ManualFloat(Rect(Left, Top, Left + Width, Top + Height));
      //Update;
      inherited RemoveControl(Control);
      Free;
      Exit;
    end;
  end;
  //if ClientCount > 0 then
  Update;
  inherited RemoveControl(Control);
end;

function TCDManagerTabs.GetHeaderPos: THeaderPos;
begin
  Result := inherited;
end;

function TCDManagerTabs.FindTabSheet(TabSheet: TTabSheet): TCDManagerTabsItem;
var
  I: Integer;
begin
  I := 0;
  while (I < FDockItems.Count) and
    (TCDManagerTabsItem(FDockItems[I]).TabSheet <> TabSheet) do Inc(I);
  if I < FDockItems.Count then Result := TCDManagerTabsItem(FDockItems[I])
    else Result := nil;
end;

procedure TCDManagerTabs.BringToFront;
begin
  inherited BringToFront;
  Update;
end;

procedure TCDManagerTabs.SetHeaderPos(const AValue: THeaderPos);
begin
  inherited SetHeaderPos(AValue);
  with PageControl do
  case AValue of
    hpAuto, hpTop: begin
      //Align := alTop;
      TabPosition := tpTop;
      Height := GrabberSize;
    end;
    hpLeft: begin
      //Align := alLeft;
      TabPosition := tpLeft;
      Width := GrabberSize;
    end;
    hpRight: begin
      //Align := alRight;
      TabPosition := tpRight;
      Width := GrabberSize;
    end;
    hpBottom: begin
      //Align := alBottom;
      TabPosition := tpBottom;
      Height := GrabberSize;
    end;
  end;
end;

procedure TCDManagerTabs.InsertControlPanel(AControl: TControl; InsertAt: TAlign;
  DropCtl: TControl);
var
  NewTabSheet: TTabSheet;
begin
  inherited;
  InsertControlNoUpdate(AControl, InsertAt);
  Update;
end;

procedure TCDManagerTabs.Update;
var
  I: Integer;
  NewTabSheet: TTabSheet;
  DeletedPage: TTabSheet;
begin
  if FUpdateCount = 0 then begin
  DebugLog('TCDManagerTabs.Update');
  for I := 0 to DockItems.Count - 1 do
  with TCDManagerTabsItem(DockItems[I]) do begin
    //Control.Tag := Integer(dhtTemporal);
    //Control.Visible := False;
    //if I >= DockItems.Count then
    //  Control.Parent := nil;
  end;

  // Remove old existed tabs which doesn't have item associated
  for I := PageControl.PageCount - 1 downto 0 do begin
//    TCDManagerTabsItem(DockItems[DockItems.Count - 1]).Control.Visible := False;
//    TCDManagerTabsItem(DockItems[DockItems.Count - 1]).Control.Parent := nil;

    if FindTabSheet(TTabSheet(PageControl.Pages[I])) = nil then begin
      PageControl.OnChange := nil;
      DeletedPage := PageControl.Pages[I];
      DeletedPage.Parent := nil;
      PageControl.OnChange := TabControlChange;
      DeletedPage.Free;
      TabImageList.Delete(I);
    end;
  end;

  // Create tabs for new items
  for I := 0 to DockItems.Count - 1 do
  with TCDManagerTabsItem(DockItems[I]) do begin
    if not Assigned(TabSheet) then begin
      NewTabSheet := TTabSheet.Create(PageControl);
      NewTabSheet.PageControl := PageControl;
      TabSheet := NewTabSheet;
      TabImageList.Add(IconImage.Picture.Bitmap, nil);
    end;
  end;

  for I := 0 to DockItems.Count - 1 do
  with TCDManagerTabsItem(DockItems[I]) do begin
    PageControl.Pages[I].Caption := Control.Caption;
    PageControl.Pages[I].ImageIndex := I;
    TabImageList.Replace(I, IconImage.Picture.Bitmap, nil);
    //if (I < PageControl.PageCount) and Assigned(PageControl.Pages[I]) then
    Control.Parent := PageControl.Pages[I];
    Control.Align := alClient;
    if PageControl.PageIndex = I then begin
      TCDManager(Control.DockManager).DockSiteVisible := True;
      if not Control.Visible then Control.Show;
    end else begin
      TCDManager(Control.DockManager).DockSiteVisible := False;
    end;
    PageControl.Pages[I].TabVisible := Control.Visible;

    //TCDClientPanel(DockPanels[I]).ClientAreaPanel.Width := DockSite.Width;
    //TCDClientPanel(DockPanels[I]).ClientAreaPanel.Height := DockSite.Height - PageControl.Height;
    //TCDClientPanel(FDockPanels[I]).DockPanelPaint(Self);
  end;
  end;
  inherited;
end;

end.

