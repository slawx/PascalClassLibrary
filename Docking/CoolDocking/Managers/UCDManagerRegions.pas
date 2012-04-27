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
    PanelHeader: TCDPanelHeader;
    Splitter: TSplitter;
    Align: TAlign;
    procedure VisibleChange(Sender: TObject); override;
    procedure Paint(Sender: TObject); override;
    constructor Create;
    destructor Destroy; override;
    procedure SetControl(const AValue: TWinControl); override;
  end;

  { TCDManagerRegions }

  TCDManagerRegions = class(TCDManager)
  private
    FDockItems: TObjectList; // TList<TCDManagerRegionsItem>
    function GetHeaderPos: THeaderPos; override;
    procedure SetHeaderPos(const AValue: THeaderPos); override;
    function GetDirection(InsertAt: TAlign): TCDDirection;
  public
    FDockDirection: TCDDirection;
    //Panels: TObjectList; // TObjectList<TCDStyleRegionsPanel>
    function FindControlInPanels(Control: TControl): TCDManagerItem; override;
    procedure InsertControlNoUpdate(Control: TControl; InsertAt: TAlign);
    procedure InsertControlPanel(Control: TControl; InsertAt: TAlign;
      DropCtl: TControl); override;
    procedure RemoveControl(Control: TControl); override;
    constructor Create(ADockSite: TWinControl);
    destructor Destroy; override;
    procedure PaintSite(DC: HDC); override;
    procedure Update; override;
    procedure SetVisible(const AValue: Boolean); override;
    procedure ChangeVisible(Control: TWinControl; Visible: Boolean);
    property DockDirection: TCDDirection read FDockDirection
      write FDockDirection;
    property DockItems: TObjectList read FDockItems write FDockItems;
  end;

implementation

uses
  UCDClient, UCDConjoinForm;

{ TCDManagerRegionsItem }

procedure TCDManagerRegionsItem.VisibleChange(Sender: TObject);
begin
  inherited VisibleChange(Sender);
  with TCDManagerRegions(Manager) do begin
    //if TControl(Sender).Visible then begin
    //  TCDManagerRegionsItem(DockItems[DockItems.IndexOf(FindControlInPanels(TControl(Sender)))]).HideType := dhtPermanent;
    //end;
    Update;

    // if any region is visible, show parent docksite
    //if DockSite is TForm then
    //  DockSite.Visible := (DockSite.VisibleDockClientCount > 0);
  end;
end;

procedure TCDManagerRegionsItem.Paint(Sender: TObject);
var
  I: Integer;
  R: TRect;
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

constructor TCDManagerRegionsItem.Create;
begin
  PanelHeader := TCDPanelHeader.Create(nil);
//  PanelHeader.Header.ManagerItem := Self;
  PanelHeader.Header.OnMouseDown := DockPanelMouseDown;
  PanelHeader.Header.Icon.OnMouseDown := DockPanelMouseDown;

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
  Splitter.Free;
  Control.Parent := nil;
  inherited Destroy;
end;

procedure TCDManagerRegionsItem.SetControl(const AValue: TWinControl);
begin
  inherited SetControl(AValue);
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

  if DockItems.Count = 0 then NewItem.Align := alClient
    else NewItem.Align := InsertAt;

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
      ManualFloat(Rect(Left, Top, Left + Width, Top + Height));
      inherited RemoveControl(Control);
      Free;
      Exit;
    end;
  end;
  inherited RemoveControl(Control);
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
  SplitterLeft: Integer;
  SplitterTop: Integer;
  BaseAlign: TAlign;
  VisibleControlsCount: Integer;
begin
  inherited;
  if FUpdateCount = 0 then begin
  DebugLog('TCDManagerRegions.UpdateClientSize');
  VisibleControlsCount := DockSite.VisibleDockClientCount;
  if DockSite is TForm then begin
    DockSiteVisible := VisibleControlsCount > 0;
  end;
  if VisibleControlsCount = 0 then VisibleControlsCount := 1;

  for I := 0 to DockItems.Count - 1 do
  with TCDManagerRegionsItem(DockItems[I]) do
  begin
    PanelHeader.Left := SplitterLeft;
    PanelHeader.Top := SplitterTop;
    PanelHeader.Height := Self.DockSite.Height div
      VisibleControlsCount;
    PanelHeader.Width := Self.DockSite.Width div
      VisibleControlsCount;
    if Assigned(TWinControl(Control).DockManager) then
      PanelHeader.Header.Visible := TCDManager(TWinControl(Control).DockManager).HeaderVisible;
    PanelHeader.Visible := Control.Visible;
    Paint(Self);
    if FDockDirection = ddHorizontal then
      BaseAlign := alLeft else BaseAlign := alTop;

    if I < Trunc((DockItems.Count - 1) / 2) then BaseAlign := BaseAlign
        else if I = Trunc((DockItems.Count - 1) / 2) then BaseAlign := alClient
        else if I > Trunc((DockItems.Count - 1) / 2) then begin
          if BaseAlign = alTop then BaseAlign := alBottom
          else if BaseAlign = alLeft then BaseAlign := alRight;
        end;
    PanelHeader.Align := BaseAlign;

    Splitter.Align := BaseAlign;
    SplitterLeft := PanelHeader.Left;
    SplitterTop := PanelHeader.Top;
    Splitter.Left := SplitterLeft;
    Splitter.Top := SplitterTop;
    Splitter.Parent := Self.DockSite;
    Splitter.Visible := I <> (Trunc(DockItems.Count - 1) / 2);
    Inc(SplitterLeft, Splitter.Width);
    Inc(SplitterTop, Splitter.Height);

    Paint(Self);
    PanelHeader.Align := BaseAlign;
  end;
  end;
end;

procedure TCDManagerRegions.SetVisible(const AValue: Boolean);
var
  I: Integer;
begin
  inherited;
  //if DockSite.Visible <> AValue then
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

