unit UCDManagerRegions;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, StdCtrls, Forms,
  Graphics, Contnrs, Buttons, UCDCommon, UCDManager,
  LCLType, LMessages;

type

  { TCDManagerRegionsItem }

  TCDManagerRegionsItem = class(TCDManagerItem)
  private
  public
    PanelHeader: TCDPanelHeader;
    Splitter: TSplitter;
    procedure VisibleChange(Sender: TObject); override;
    procedure Paint(Sender: TObject); override;
    procedure PanelResize(Sender: TObject);
    constructor Create; override;
    destructor Destroy; override;
    procedure SetControl(const AValue: TWinControl); override;
    procedure SetCenter;
  end;

  { TCDManagerRegions }

  TCDManagerRegions = class(TCDManager)
  private
    FDockItems: TObjectList; // TList<TCDManagerRegionsItem>
    FLastVisibleItemsCount: Integer;
    function GetDirection(InsertAt: TAlign): TCDDirection;
    procedure ResizePanels;
    procedure ClearItemsAlignment;
    procedure UpdateItemsAlignment;
    function PanelsVisible: Integer;
  protected
    FDockDirection: TCDDirection;
    procedure SetHeaderPos(const AValue: THeaderPos); override;
  public
    //Panels: TObjectList; // TObjectList<TCDStyleRegionsPanel>
    function GetHeaderPos: THeaderPos; override;
    procedure BringToFront; override;
    function FindControlInPanels(Control: TControl): TCDManagerItem; override;
    procedure InsertControlNoUpdate(Control: TControl; InsertAt: TAlign);
    procedure InsertControlPanel(Control: TControl; InsertAt: TAlign;
      DropCtl: TControl); override;
    procedure RemoveControl(Control: TControl); override;
    constructor Create(ADockSite: TWinControl); override;
    destructor Destroy; override;
    procedure PaintSite(DC: HDC); override;
    procedure Update; override;
    procedure SetVisible(const AValue: Boolean); override;
    procedure ChangeVisible(Control: TWinControl; Visible: Boolean); override;
    property DockDirection: TCDDirection read FDockDirection
      write FDockDirection;
    property DockItems: TObjectList read FDockItems write FDockItems;
  end;

implementation

uses
  UCDClient, UCDConjoinForm;

{ TCDManagerRegionsItem }

procedure TCDManagerRegionsItem.SetCenter;
begin
  TCDManagerRegions(Manager).ClearItemsAlignment;
  PanelHeader.Align := alClient;
  Manager.Update;
end;

procedure TCDManagerRegionsItem.VisibleChange(Sender: TObject);
begin
  inherited VisibleChange(Sender);
  with TCDManagerRegions(Manager) do begin
    //if TControl(Sender).Visible then begin
    //  TCDManagerRegionsItem(DockItems[DockItems.IndexOf(FindControlInPanels(TControl(Sender)))]).HideType := dhtPermanent;
    //end;
    PanelHeader.Visible := Control.Visible;
    Update;

    // if any region is visible, show parent docksite
    //if DockSite is TForm then
    //  DockSite.Visible := (DockSite.VisibleDockClientCount > 0);
  end;
end;

procedure TCDManagerRegionsItem.Paint(Sender: TObject);
begin
  inherited Paint(Sender);
  with PanelHeader do
  if not (csDesigning in ComponentState) then
  if Assigned(Control) then begin
    //R := Control.ClientRect;
    //Canvas.FillRect(R);
    if Visible then begin
      Header.Invalidate;
    end;
  end;
end;

procedure TCDManagerRegionsItem.PanelResize(Sender: TObject);
begin
  TCDManagerRegions(Manager).ResizePanels;
end;

constructor TCDManagerRegionsItem.Create;
begin
  PanelHeader := TCDPanelHeader.Create(nil);
//  PanelHeader.Header.ManagerItem := Self;
  PanelHeader.Header.OnMouseDown := DockPanelMouseDown;
  PanelHeader.Header.Icon.OnMouseDown := DockPanelMouseDown;
  PanelHeader.OnResize := PanelResize;

  Splitter := TSplitter.Create(nil);
  with Splitter do begin
    Width := 3;
    Height := 3;
    //Parent := Panel;
    //Color := clRed;
  end;
end;

destructor TCDManagerRegionsItem.Destroy;
begin
  PanelHeader.Parent := nil;
  PanelHeader.Free;
  Splitter.Parent := nil;
  FreeAndNil(Splitter);
  if Assigned(Control) then Control.Parent := nil;
  inherited;
end;

procedure TCDManagerRegionsItem.SetControl(const AValue: TWinControl);
begin
  inherited;
  PanelHeader.Header.Control := AValue;
end;


{ TCDManagerRegions }

function TCDManagerRegions.GetHeaderPos: THeaderPos;
begin
  Result := inherited;
end;

procedure TCDManagerRegions.SetHeaderPos(const AValue: THeaderPos);
begin
  inherited SetHeaderPos(AValue);
  if Assigned(DockSite.Parent) then
    TCDManager(DockSite.Parent.DockManager).Update;
(*  case AValue of
    hpBottom, hpTop: FDockDirection := ddVertical;
    hpLeft, hpRight: FDockDirection := ddHorizontal;
  end;*)
end;

function TCDManagerRegions.GetDirection(InsertAt: TAlign): TCDDirection;
begin
  Result := ddHorizontal;
  if (InsertAt = alTop) or (InsertAt = alBottom) then
    Result := ddVertical
  else
  if (InsertAt = alLeft) or (InsertAt = alRight) then
    Result := ddHorizontal
  else;
end;

procedure TCDManagerRegions.ResizePanels;
var
  I: Integer;
  CenterPanelSize: TPoint;
  Zoom: Real;
const
  MinSize = 30;
begin
  I := 0;
  while (I < DockItems.Count) and
    (TCDManagerRegionsItem(DockItems[I]).PanelHeader.Align <> alClient) do
      Inc(I);
  if I < DockItems.Count then begin
    CenterPanelSize.X := TCDManagerRegionsItem(DockItems[I]).PanelHeader.Width;
    CenterPanelSize.Y := TCDManagerRegionsItem(DockItems[I]).PanelHeader.Height;
  end else Exit;
  if (CenterPanelSize.X < MinSize) and (FDockDirection = ddHorizontal) then begin
    Zoom := CenterPanelSize.X / MinSize;
    for I := 0 to DockItems.Count - 1 do
      with TCDManagerRegionsItem(DockItems[I]).PanelHeader do
        if Align <> alClient then Width := Round(Width * Zoom);
  end;
  if (CenterPanelSize.Y < MinSize) and (FDockDirection = ddVertical) then begin
    Zoom := CenterPanelSize.Y / MinSize;
    for I := 0 to DockItems.Count - 1 do
      with TCDManagerRegionsItem(DockItems[I]).PanelHeader do
        if Align <> alClient then Height := Round(Height * Zoom);
  end;
end;

procedure TCDManagerRegions.ClearItemsAlignment;
var
  I: Integer;
begin
  for I := 0 to FDockItems.Count - 1 do
  with TCDManagerRegionsItem(FDockItems[I]) do begin
    PanelHeader.Align := alNone;
    Splitter.Align := alNone;
  end;
end;

procedure TCDManagerRegions.UpdateItemsAlignment;
var
  I: Integer;
  ClientPanelIndex: Integer;
begin
  // Find alClient panel item index
  I := 0;
  while (I < FDockItems.Count) and
    (TCDManagerRegionsItem(FDockItems[I]).PanelHeader.Align <> alClient) do Inc(I);
  if I < FDockItems.Count then ClientPanelIndex := I
    else ClientPanelIndex := FDockItems.Count div 2;

  // Normalize alignment
  for I := 0 to FDockItems.Count - 1 do
  with TCDManagerRegionsItem(FDockItems[I]) do begin
    if FDockDirection = ddHorizontal then begin
      if I < ClientPanelIndex then PanelHeader.Align := alLeft
        else if I = ClientPanelIndex then PanelHeader.Align := alClient
        else if I > ClientPanelIndex then PanelHeader.Align := alRight;
    end;
    if FDockDirection = ddVertical then begin
      if I < ClientPanelIndex then PanelHeader.Align := alTop
        else if I = ClientPanelIndex then PanelHeader.Align := alClient
        else if I > ClientPanelIndex then PanelHeader.Align := alBottom;
    end;
  end;
end;

function TCDManagerRegions.PanelsVisible: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FDockItems.Count - 1 do
  with TCDManagerRegionsItem(FDockItems[I]) do begin
    if PanelHeader.Visible then Inc(Result);
  end;
end;

procedure TCDManagerRegions.BringToFront;
begin
  inherited BringToFront;
end;

function TCDManagerRegions.FindControlInPanels(Control: TControl
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

procedure TCDManagerRegions.InsertControlNoUpdate(Control: TControl;
  InsertAt: TAlign);
var
  NewItem: TCDManagerRegionsItem;
begin
  NewItem := TCDManagerRegionsItem.Create;
  with NewItem do begin
    PanelHeader.DockItem := NewItem;
    PanelHeader.Parent := Self.DockSite;
    Manager := Self;
    if DockStyle = dsList then Visible := True;
    PanelHeader.Header.PopupMenu := Self.PopupMenu;
  end;
  if (Control is TForm) and Assigned((Control as TForm).Icon) then begin
    NewItem.PanelHeader.Header.Icon.Picture.Assign((Control as TForm).Icon);
    NewItem.PanelHeader.Header.Icon.Width := NewItem.PanelHeader.Header.Icon.Picture.Bitmap.Width;
    NewItem.PanelHeader.Header.Icon.Height := NewItem.PanelHeader.Header.Icon.Picture.Bitmap.Height;
  end;

  NewItem.PanelHeader.Parent := DockSite;

  NewItem.Control := TWinControl(Control);
  Control.AddHandlerOnVisibleChanged(NewItem.VisibleChange);
  Control.AddHandlerOnVisibleChanging(NewItem.VisibleChanging);
  Control.Parent := NewItem.PanelHeader.ControlPanel;
  Control.Align := alClient;
  if (InsertAt = alTop) or (InsertAt = alLeft) then
    DockItems.Insert(0, NewItem)
    else DockItems.Add(NewItem);
end;

procedure TCDManagerRegions.InsertControlPanel(Control: TControl; InsertAt: TAlign;
  DropCtl: TControl);
var
  NewItem: TCDManagerRegionsItem;
  I: Integer;
  NewDirection: TCDDirection;
  NewConjoinDockForm: TCDConjoinForm;
  NewDockSite: TWinControl;
begin
  inherited;
  begin
    if DockSite.DockClientCount <= 2 then FDockDirection := GetDirection(InsertAt)
    else
    if (DockSite.DockClientCount > 2) then begin
      NewDirection := GetDirection(InsertAt);
      if (NewDirection <> FDockDirection) then begin
        // Direction change, create conjoin form
        NewConjoinDockForm := CreateConjoinForm;
        try
          FreeParentIfEmpty := False;
          for I := DockSite.DockClientCount - 1 downto 0 do begin
            DockSite.DockClients[I].ManualDock(NewConjoinDockForm);
          end;
        finally
          FreeParentIfEmpty := True;
        end;
        NewConjoinDockForm.ManualDock(DockSite);
        Control.ManualDock(DockSite, nil, InsertAt);
        NewConjoinDockForm.UpdateCaption;
        Update;
        Exit;
      end;
    end;
    InsertControlNoUpdate(Control, InsertAt);
  end;
  Update;
end;

procedure TCDManagerRegions.RemoveControl(Control: TControl);
var
  ManagerItem: TCDManagerItem;
  ClientCount: Integer;
begin
  ManagerItem := FindControlInPanels(Control);
  if Assigned(ManagerItem) then begin
    Control.RemoveHandlerOnVisibleChanged(ManagerItem.VisibleChange);
    Control.RemoveHandlerOnVisibleChanging(ManagerItem.VisibleChanging);
  end;

  DockItems.Remove(ManagerItem);
  ClientCount := DockItems.Count;

  //if TCDManager(Manager).DockSite.DockClientCount = 2 then FDockDirection := ddNone;
  if FreeParentIfEmpty and (ClientCount = 1) then begin
    // Last removed control => Free parent if it is TCDConjoinForm
    if Self.DockSite is TCDConjoinForm then
    with TCDConjoinForm(Self.DockSite) do begin
      if Assigned(Parent) then begin
        TCDManagerItem(DockItems[0]).Control.ManualDock(HostDockSite);
      end else TCDManagerItem(DockItems[0]).Control.ManualFloat(Rect(Left, Top, Left + Width, Top + Height));
      if FreeIfEmpty then ManualFloat(Rect(Left, Top, Left + Width, Top + Height));
      inherited;
      if FreeIfEmpty then Free;
      Exit;
    end;
  end;
  inherited;
  if ClientCount > 1 then Update;
end;

constructor TCDManagerRegions.Create(ADockSite: TWinControl);
var
  I: Integer;
  NewItem: TCDManagerRegionsItem;
begin
  inherited;
  FDockStyle := dsList;
  FDockItems := TObjectList.Create;

  for I := 0 to ADockSite.DockClientCount - 1 do
    InsertControlNoUpdate(ADockSite.DockClients[I], alLeft);
  Update;
end;

destructor TCDManagerRegions.Destroy;
begin
  FDockItems.Free;
  inherited Destroy;
end;

procedure TCDManagerRegions.PaintSite(DC: HDC);
var
  I: Integer;
begin
  inherited PaintSite(DC);
  for I := 0 to FDockItems.Count - 1 do
    with TCDManagerRegionsItem(FDockItems[I]) do begin
      PanelHeader.Invalidate;
    end;
end;

procedure TCDManagerRegions.Update;
var
  I: Integer;
  PositionLeft: Integer;
  PositionTop: Integer;
  VisibleControlsCount: Integer;
const
  MinSize = 30;
begin
  inherited;
  if FUpdateCount = 0 then begin
  DebugLog('TCDManagerRegions.UpdateClientSize');
  VisibleControlsCount := PanelsVisible;
  if DockSite is TForm then begin
    //DockSiteVisible := VisibleControlsCount > 0;
  end;
  if VisibleControlsCount = 0 then VisibleControlsCount := 1;

  PositionLeft := 0;
  PositionTop := 0;
  UpdateItemsAlignment;
  for I := 0 to DockItems.Count - 1 do
  with TCDManagerRegionsItem(DockItems[I]) do
  begin
    PanelHeader.Left := PositionLeft;
    PanelHeader.Top := PositionTop;
    if (FLastVisibleItemsCount <> VisibleControlsCount) then begin
      PanelHeader.Height := Self.DockSite.Height div
        VisibleControlsCount;
      PanelHeader.Width := Self.DockSite.Width div
        VisibleControlsCount;
    end;
    if PanelHeader.Height < MinSize then PanelHeader.Height := MinSize;
    if PanelHeader.Width < MinSize then PanelHeader.Width := MinSize;

    if Assigned(TWinControl(Control).DockManager) then
      PanelHeader.Header.Visible := TCDManager(TWinControl(Control).DockManager).HeaderVisible;
    PanelHeader.Visible := Control.Visible;
    Paint(Self);

    //PositionLeft := PanelHeader.Left;
    //PositionTop := PanelHeader.Top;
    if PanelHeader.Align = alLeft then Splitter.Left := PositionLeft + PanelHeader.Width + 1;
    if PanelHeader.Align = alRight then Splitter.Left := PositionLeft - 1;
    if PanelHeader.Align = alTop then Splitter.Top := PositionTop + PanelHeader.Height + 1;
    if PanelHeader.Align = alBottom then Splitter.Top := PositionTop - 1;
    Splitter.Align := PanelHeader.Align;
    Splitter.Parent := Self.DockSite;
    Splitter.Visible := PanelHeader.Visible and (PanelHeader.Align <> alClient);
    Splitter.Width := 3;
    Splitter.Height := 3;
    Inc(PositionLeft, Splitter.Width + PanelHeader.Width);
    Inc(PositionTop, Splitter.Height + PanelHeader.Height);

    Paint(Self);
    //PanelHeader.Align := BaseAlign;
  end;
    FLastVisibleItemsCount := VisibleControlsCount;
  end;
end;

procedure TCDManagerRegions.SetVisible(const AValue: Boolean);
var
  I: Integer;
begin
  inherited;
  //if DockSite.Visible <> AValue then
  if DockItems.Count > 0 then
  try
    BeginUpdate;
    for I := 0 to DockItems.Count - 1 do
      with TCDManagerRegionsItem(DockItems[I]) do begin
        TCDManager(Control.DockManager).DockSiteVisible := AValue;
      end;
        //ClientAreaPanel.Show;
  finally
    EndUpdate;
  end;
end;

procedure TCDManagerRegions.ChangeVisible(Control: TWinControl;
  Visible: Boolean);
begin
  inherited;
end;

end.

