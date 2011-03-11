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
    Panel: TPanel;
    Splitter: TSplitter;
    ClientAreaPanel: TPanel;
    constructor Create;
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
  Panel := TPanel.Create(nil);

  ClientAreaPanel := TPanel.Create(Panel);
  with ClientAreaPanel do begin
    Parent := Panel;
    Visible := True;
    DockSite := True;
    UseDockManager := True;
    Align := alClient;
    BevelInner := bvNone;
    BevelOuter := bvNone;
    //Color := clGreen;
  end;
  Splitter := TSplitter.Create(Panel);
  with Splitter do begin
    Parent := Panel;
    //Color := clRed;
  end;
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

procedure TCDManagerRegions.InsertControlPanel(Control: TControl; InsertAt: TAlign;
  DropCtl: TControl);
var
  NewPanel: TCDManagerRegionsItem;
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

    NewPanel := TCDManagerRegionsItem.Create;
    with NewPanel do begin
      Panel.Parent := Self.DockSite;
      Manager := Self;
      if DockStyle = dsList then Visible := True;
      Header.PopupMenu := Self.PopupMenu;
      //PopupMenu.Parent := Self.DockSite;
    end;
    if (Control is TForm) and Assigned((Control as TForm).Icon) then
      NewPanel.Header.Icon.Picture.Assign((Control as TForm).Icon);

    NewPanel.Control := Control;
    Control.AddHandlerOnVisibleChanged(NewPanel.VisibleChange);
    Control.Parent := NewPanel.ClientAreaPanel;
    Control.Align := alClient;
    if (InsertAt = alTop) or (InsertAt = alLeft) then
      DockItems.Insert(0, NewPanel)
      else DockItems.Add(NewPanel);
  end;
  UpdateClientSize;
end;

procedure TCDManagerRegions.RemoveControl(Control: TControl);
var
  ClientPanel: TCDManagerItem;
  ClientCount: Integer;
begin
  ClientPanel := FindControlInPanels(Control);
  Control.RemoveHandlerOnVisibleChanged(ClientPanel.VisibleChange);

  DockItems.Remove(ClientPanel);
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
begin
  inherited;
  FDockStyle := dsList;
  FDockItems := TObjectList.Create;

  for I := 0 to DockItems.Count - 1 do begin
    if Assigned(TCDManagerRegionsItem(DockItems[I]).Splitter) then
    with TCDManagerRegionsItem(DockItems[I]) do begin
      Splitter.Visible := True;
      Panel.Visible := True;
      ClientAreaPanel.Parent := TCDManagerRegionsItem(DockItems[I]).Panel;
      ClientAreaPanel.Visible := True;
      Control.Visible := True;
    end;
  end;
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
      Panel.Invalidate;
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
    Panel.Left := SplitterLeft;
    Panel.Top := SplitterTop;
    Panel.Height := Self.DockSite.Height div
      Self.DockSite.DockClientCount;
    Panel.Width := Self.DockSite.Width div
      Self.DockSite.DockClientCount;
    //TCDClientPanel(FDockPanels[I]).DockPanelPaint(Self);
    if I < (DockItems.Count - 1) then Panel.Align := BaseAlign
      else Panel.Align := alClient;

    Inc(SplitterLeft, Panel.Width);
    Inc(SplitterTop, Panel.Height);
    Splitter.Left := SplitterLeft;
    Splitter.Top := SplitterTop;
    Splitter.Parent := Self.DockSite;
    Splitter.Align := BaseAlign;
    Splitter.Visible := I < (DockItems.Count - 1);
    Inc(SplitterLeft, Splitter.Width);
    Inc(SplitterTop, Splitter.Height);

    Paint(Self);
    if I < (DockItems.Count - 1) then begin
      if DockDirection = ddHorizontal then Panel.Align := alLeft
        else Panel.Align := alTop;
    end else Panel.Align := alClient;
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

