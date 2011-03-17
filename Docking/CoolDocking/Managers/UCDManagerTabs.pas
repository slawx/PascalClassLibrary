unit UCDManagerTabs;

{$mode Delphi}{$H+}

interface

uses
  Classes, Controls, ExtCtrls, ComCtrls, SysUtils, Dialogs, Contnrs,
  Menus, Forms, UCDCommon, UCDManager,
  LCLType, LMessages, Graphics;

type

  { TCDManagerTabsItem }

  TCDManagerTabsItem = class(TCDManagerItem)
    Icon: TImage;
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TCDManagerTabs }

  TCDManagerTabs = class(TCDManager)
  private
    MouseDown: Boolean;
    MouseButton: TMouseButton;
    FDockItems: TObjectList; // TList<TCDManagerRegionsItem>
    procedure TabControlMouseLeave(Sender: TObject);
    procedure TabControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TabControlMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure InsertControlPanel(AControl: TControl; InsertAt: TAlign;
      DropCtl: TControl); override;
    procedure UpdateClientSize; override;
    function FindControlInPanels(Control: TControl): TCDManagerItem; override;
    procedure RemoveControl(Control: TControl); override;
    function GetHeaderPos: THeaderPos; override;
  public
    MouseDownSkip: Boolean;
    TabImageList: TImageList;
    PageControl: TPageControl;
    procedure SetHeaderPos(const AValue: THeaderPos); override;
    procedure InsertControlNoUpdate(Control: TControl; InsertAt: TAlign); virtual;
    procedure TabControlChange(Sender: TObject); virtual;
    constructor Create(ADockSite: TWinControl); override;
    destructor Destroy; override;
    procedure PaintSite(DC: HDC); override;
    procedure DoSetVisible(const AValue: Boolean); override;
    procedure ChangeVisible(Control: TWinControl; Visible: Boolean); override;
    procedure Switch(Index: Integer); override;
    procedure PopupMenuTabCloseClick(Sender: TObject);
    property DockItems: TObjectList read FDockItems write FDockItems;
  end;

implementation

uses
  UCDClient;

{ TCDManagerTabsItem }

constructor TCDManagerTabsItem.Create;
begin
  Icon := TImage.Create(nil);
end;

destructor TCDManagerTabsItem.Destroy;
begin
  Icon.Free;
  inherited Destroy;
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
var
  I: Integer;
begin
  UpdateClientSize;
{  // Hide all clients
  for I := 0 to DockItems.Count - 1 do
    if TCDManagerItem(DockItems[I]).Control.Visible
    //and (PageControl.TabIndex <> I)
    then
    begin
      TCDManagerItem(DockItems[I]).Control.Tag := Integer(dhtTemporal);
      TCDManagerItem(DockItems[I]).Control.Hide;
      //TCDClientPanel(DockItems[I]).ClientAreaPanel.Hide;
      //TCDClientPanel(DockItems[I]).ClientAreaPanel.Parent := PageControl.Pages[I];
      //TCDClientPanel(DockPanels[I]).ClientAreaPanel.Parent := DockSite;
      TCDManagerItem(DockItems[I]).Control.Align := alClient;
      //TCDClientPanel(DockPanels[I]).Control.Parent :=
      //  TCDClientPanel(DockPanels[I]).ClientAreaPanel;
      //ShowMessage(TCDClientPanel(DockPanels[I]).Control.ClassName);
      //Application.ProcessMessages;

      // Workaround for "Cannot focus" error
      TForm(TCDManagerItem(DockItems[I]).Control).ActiveControl := nil;
    end;

  // Show selected
  if (PageControl.TabIndex <> -1) and (DockItems.Count > PageControl.TabIndex)
//  and not TCDClientPanel(DockPanels[PageControl.TabIndex]).Control.Visible
  then begin
    with TCDManagerItem(DockItems[PageControl.TabIndex]) do begin
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
  MouseDownSkip := True;        }
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
  PageControl.Invalidate;
end;

procedure TCDManagerTabs.Switch(Index: Integer);
begin
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
      //Panel.Parent := Self.DockSite;
      Manager := Self;
      //if DockStyle = dsList then Visible := True;
      //Align := alClient;
      //Header.PopupMenu := Self.PopupMenu;
      //PopupMenu.Parent := Self.DockSite;
    end;
    if (Control is TForm) and Assigned((Control as TForm).Icon) then
      NewItem.Icon.Picture.Assign((Control as TForm).Icon);

    NewItem.Control := Control;
    Control.AddHandlerOnVisibleChanged(NewItem.VisibleChange);
    //AControl.Parent := NewItem.ClientAreaPanel;
    Control.Align := alClient;
    if (InsertAt = alTop) or (InsertAt = alLeft) then
      DockItems.Insert(0, NewItem)
      else DockItems.Add(NewItem);

  end;

    NewTabSheet := TTabSheet.Create(PageControl);
    NewTabSheet.PageControl := PageControl;
    NewTabSheet.Caption := Control.Caption;
    NewTabSheet.ImageIndex := TabImageList.Count;
    NewTabSheet.TabVisible := Control.Visible;
    Control.Parent := NewTabSheet;
    TabImageList.Add(NewItem.Icon.Picture.Bitmap, nil);
//    if Assigned(NewItem.Splitter) then
//      NewItem.Splitter.Visible := False;
//    NewItem.ClientAreaPanel.Visible := False;
//    NewItem.Visible := False;
    //NewItem.Parent := NewTabSheet;
end;

procedure TCDManagerTabs.RemoveControl(Control: TControl);
begin
  inherited RemoveControl(Control);
end;

function TCDManagerTabs.GetHeaderPos: THeaderPos;
begin
  Result := inherited;
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
  TabControlChange(Self);
end;

procedure TCDManagerTabs.UpdateClientSize;
var
  I: Integer;
  NewTabSheet: TTabSheet;
begin
  inherited UpdateClientSize;
  for I := 0 to DockItems.Count - 1 do
  with TCDManagerTabsItem(DockItems[I]) do begin
    Control.Parent := PageControl.Pages[I];
    Control.Align := alClient;

    //TCDClientPanel(DockPanels[I]).ClientAreaPanel.Width := DockSite.Width;
    //TCDClientPanel(DockPanels[I]).ClientAreaPanel.Height := DockSite.Height - PageControl.Height;
    //TCDClientPanel(FDockPanels[I]).DockPanelPaint(Self);
  end;
end;

procedure TCDManagerTabs.DoSetVisible(const AValue: Boolean);
begin
  inherited;
    if (PageControl.TabIndex >= 0) and (PageControl.TabIndex < DockItems.Count) then
      with TCDManagerItem(DockItems[PageControl.TabIndex]) do begin
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

procedure TCDManagerTabs.ChangeVisible(Control: TWinControl; Visible: Boolean);
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

        I := DockItems.IndexOf(FindControlInPanels(Control));
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
        I := DockItems.IndexOf(FindControlInPanels(Control));
        //if  then
        if I <> -1 then
          PageControl.Page[I].TabVisible := True;
//      TabImageList.Add(TCDClientPanel(TCDManager(Manager).FindControlInPanels(Control)).Header.Icon.Picture.Bitmap, nil);
//      TabControl.Tabs.Add(Control.Caption);

//      end;
    end;
  end;
end;

end.

