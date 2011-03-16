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
    constructor Create;
    destructor Destroy; override;
  end;

  { TCDManagerRegions }

  TCDManagerRegions = class(TCDManager)
  private
    FDockItems: TObjectList; // TList<TCDManagerRegionsItem>
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
    procedure UpdateClientSize; override;
    procedure DoSetVisible(const AValue: Boolean); override;
    procedure ChangeVisible(Control: TWinControl; Visible: Boolean);
    property DockDirection: TCDDirection read FDockDirection
      write FDockDirection;
    property DockItems: TObjectList read FDockItems write FDockItems;
  end;

implementation

uses
  UCDClient, UCDConjoinForm;

{ TCDManagerRegionsItem }

constructor TCDManagerRegionsItem.Create;
begin
  PanelHeader := TCDPanelHeader.Create(nil);
  PanelHeader.Header.ManagerItem := Self;
  PanelHeader.Header.OnMouseDown := DockPanelMouseDown;
  PanelHeader.Header.Title.OnMouseDown := DockPanelMouseDown;

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


{ TCDManagerRegions }

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
  if (Control is TForm) and Assigned((Control as TForm).Icon) then
    NewItem.PanelHeader.Header.Icon.Picture.Assign((Control as TForm).Icon);
    NewItem.PanelHeader.Parent := DockSite;
    NewItem.PanelHeader.Header.Title.Caption := TForm(Control).Caption;

    NewItem.Control := Control;
    Control.AddHandlerOnVisibleChanged(NewItem.VisibleChange);
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
        NewConjoinDockForm := CreateContainer(InsertAt);
        NewDockSite := DockSite.HostDockSite;
        // FDockSite.ManualFloat(FDockSite.BoundsRect);
        NewConjoinDockForm.ManualDock(NewDockSite);
        Control.ManualDock(NewConjoinDockForm, nil, InsertAt);
        if DockSite is TForm then
          DockSite.ManualDock(NewConjoinDockForm)
        else
        if DockSite is TPanel then
          DockSite.Parent.ManualDock(NewConjoinDockForm);
        UpdateClientSize;
        Exit;
      end;
    end;
    InsertControlNoUpdate(Control, InsertAt);
  end;
  UpdateClientSize;
end;

procedure TCDManagerRegions.RemoveControl(Control: TControl);
var
  ManagerItem: TCDManagerItem;
  ClientCount: Integer;
begin
  ManagerItem := FindControlInPanels(Control);
  if Assigned(ManagerItem) then begin
    Control.RemoveHandlerOnVisibleChanged(ManagerItem.VisibleChange);
  end;

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
      Free;
    end;
  end;
  inherited RemoveControl(Control);
  if ClientCount > 1 then UpdateClientSize;
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
  UpdateClientSize;
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

procedure TCDManagerRegions.UpdateClientSize;
var
  I: Integer;
  SplitterLeft: Integer;
  SplitterTop: Integer;
  BaseAlign: TAlign;
begin
  inherited UpdateClientSize;
  if FDockDirection = ddHorizontal then
    BaseAlign := alLeft else BaseAlign := alTop;

  SplitterLeft := 0;
  SplitterTop := 0;
  for I := 0 to DockItems.Count - 1 do
  with TCDManagerRegionsItem(DockItems[I]) do begin
    PanelHeader.Left := SplitterLeft;
    PanelHeader.Top := SplitterTop;
    PanelHeader.Height := Self.DockSite.Height div
      Self.DockSite.DockClientCount;
    PanelHeader.Width := Self.DockSite.Width div
      Self.DockSite.DockClientCount;
    //TCDClientPanel(FDockPanels[I]).DockPanelPaint(Self);
    if I < (DockItems.Count - 1) then PanelHeader.Align := BaseAlign
      else PanelHeader.Align := alClient;

    Inc(SplitterLeft, PanelHeader.Width);
    Inc(SplitterTop, PanelHeader.Height);
    Splitter.Left := SplitterLeft;
    Splitter.Top := SplitterTop;
    Splitter.Parent := Self.DockSite;
    Splitter.Align := BaseAlign;
    Splitter.Visible := I < (DockItems.Count - 1);
    Inc(SplitterLeft, Splitter.Width);
    Inc(SplitterTop, Splitter.Height);

    Paint(Self);
    if I < (DockItems.Count - 1) then begin
      if DockDirection = ddHorizontal then PanelHeader.Align := alLeft
        else PanelHeader.Align := alTop;
    end else PanelHeader.Align := alClient;
  end;
end;

procedure TCDManagerRegions.DoSetVisible(const AValue: Boolean);
var
  I: Integer;
begin
  inherited;
  for I := 0 to DockItems.Count - 1 do

        //Show;
        //ShowMessage(IntToStr(Control.Tag));
      with TCDManagerRegionsItem(DockItems[I]) do begin
        if AValue and (not Control.Visible) and (Control.Tag = Integer(dhtTemporal))  then begin
          Control.Show;
          Control.Tag := Integer(dhtPermanent);
        end else
        if not AValue then begin
          Control.Tag := Integer(dhtTemporal);
          Control.Hide;
        end;
      end;
        //ClientAreaPanel.Show;
end;

procedure TCDManagerRegions.ChangeVisible(Control: TWinControl;
  Visible: Boolean);
begin
  inherited;
end;

end.

