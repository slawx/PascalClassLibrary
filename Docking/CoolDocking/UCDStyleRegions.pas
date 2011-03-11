unit UCDStyleRegions;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, StdCtrls, UCDStyle, Forms,
  Graphics, Contnrs, Buttons, UCDClientPanel, UCDCommon;

type
  TCDStyleRegionsPanel = class(TPanel)

  end;

  { TCDStyleRegions }

  TCDStyleRegions = class(TCDStyle)
  private
    function GetDirection(InsertAt: TAlign): TCDDirection;
  public
    FDockDirection: TCDDirection;
    //Panels: TObjectList; // TObjectList<TCDStyleRegionsPanel>
    procedure InsertControl(AControl: TControl; InsertAt: TAlign); override;
    procedure RemoveControl(Control: TControl); override;
    function GetHeaderPos: THeaderPos; override;
    procedure SetHeaderPos(const AValue: THeaderPos); override;
    constructor Create(AManager: TCDManagerBase);
    destructor Destroy; override;
    procedure UpdateClientSize; override;
    procedure SetVisible(const AValue: Boolean); override;
    procedure ChangeVisible(Control: TWinControl; Visible: Boolean); override;
    property DockDirection: TCDDirection read FDockDirection
      write FDockDirection;
  end;

implementation

uses
  UCDClient, UCDConjoinForm, UCDManager;


{ TCDStyleRegions }

function TCDStyleRegions.GetDirection(InsertAt: TAlign): TCDDirection;
begin
  Result := ddHorizontal;
  if (InsertAt = alTop) or (InsertAt = alBottom) then
    Result := ddVertical
  else
  if (InsertAt = alLeft) or (InsertAt = alRight) then
    Result := ddHorizontal
  else;
end;

procedure TCDStyleRegions.InsertControl(AControl: TControl; InsertAt: TAlign);
var
  NewPanel: TCDClientPanel;
  I: Integer;
  NewDirection: TCDDirection;
  NewConjoinDockForm: TCDConjoinForm;
  NewDockSite: TWinControl;
begin
  inherited;
  with TCDManager(Manager) do begin
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
        AControl.ManualDock(NewConjoinDockForm, nil, InsertAt);
        if DockSite is TForm then
          DockSite.ManualDock(NewConjoinDockForm)
        else
        if DockSite is TPanel then
          DockSite.Parent.ManualDock(NewConjoinDockForm);
        UpdateClientSize;
        Exit;
      end;
    end;

    NewPanel := TCDClientPanel.Create(nil);
    with NewPanel do begin
      Parent := TCDManager(Manager).DockSite;
      OwnerDockManager := Manager;
      if DockStyle = dsList then Visible := True;
      Header.PopupMenu := TCDManager(Manager).PopupMenu;
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
  UpdateClientSize;
end;

procedure TCDStyleRegions.RemoveControl(Control: TControl);
var
  ClientPanel: TCDClientPanel;
  ClientCount: Integer;
begin
  ClientPanel := TCDManager(Manager).FindControlInPanels(Control);
  Control.RemoveHandlerOnVisibleChanged(ClientPanel.VisibleChange);

  TCDManager(Manager).DockPanels.Remove(ClientPanel);
  ClientCount := TCDManager(Manager).DockPanels.Count;

  //if TCDManager(Manager).DockSite.DockClientCount = 2 then FDockDirection := ddNone;
  if ClientCount = 1 then begin
    // Last removed control => Free parent if it is TCDConjoinForm
    if TCDManager(Manager).DockSite is TCDConjoinForm then
    with TCDConjoinForm(TCDManager(Manager).DockSite) do begin
      if Assigned(Parent) then begin
        TCDClientPanel(TCDManager(Manager).DockPanels[0]).Control.ManualDock(HostDockSite);
      end else TCDClientPanel(TCDManager(Manager).DockPanels[0]).Control.ManualFloat(Rect(Left, Top, Left + Width, Top + Height));
      ManualFloat(Rect(Left, Top, Left + Width, Top + Height));
      Free;
    end;
  end;
  inherited RemoveControl(Control);
  if ClientCount > 1 then UpdateClientSize;
end;

function TCDStyleRegions.GetHeaderPos: THeaderPos;
begin
//  Result := TCDManager(Manager).;
end;

procedure TCDStyleRegions.SetHeaderPos(const AValue: THeaderPos);
begin
  inherited SetHeaderPos(AValue);
end;

constructor TCDStyleRegions.Create(AManager: TCDManagerBase);
var
  I: Integer;
begin
  inherited;
  //Panels := TObjectList.Create;

  with TCDManager(AManager) do
  for I := 0 to DockPanels.Count - 1 do begin
    if Assigned(TCDClientPanel(DockPanels[I]).Splitter) then
      TCDClientPanel(DockPanels[I]).Splitter.Visible := True;
    TCDClientPanel(DockPanels[I]).Visible := True;
    TCDClientPanel(DockPanels[I]).ClientAreaPanel.Parent := TCDClientPanel(DockPanels[I]);
    TCDClientPanel(DockPanels[I]).ClientAreaPanel.Visible := True;
    TCDClientPanel(DockPanels[I]).Control.Visible := True;
  end;
end;

destructor TCDStyleRegions.Destroy;
begin
  //Panels.Free;
  inherited Destroy;
end;

procedure TCDStyleRegions.UpdateClientSize;
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
  with TCDManager(Manager) do
  for I := 0 to DockPanels.Count - 1 do
  with TCDClientPanel(DockPanels[I]) do begin
    Left := SplitterLeft;
    Top := SplitterTop;
    Height := TCDManager(Manager).DockSite.Height div
      TCDManager(Manager).DockSite.DockClientCount;
    Width := TCDManager(Manager).DockSite.Width div
      TCDManager(Manager).DockSite.DockClientCount;
    //TCDClientPanel(FDockPanels[I]).DockPanelPaint(Self);
    if I < (DockPanels.Count - 1) then Align := BaseAlign
      else Align := alClient;

    Inc(SplitterLeft, Width);
    Inc(SplitterTop, Height);
    Splitter.Left := SplitterLeft;
    Splitter.Top := SplitterTop;
    Splitter.Parent := TCDManager(Manager).DockSite;
    Splitter.Align := BaseAlign;
    Splitter.Visible := I < (DockPanels.Count - 1);
    Inc(SplitterLeft, Splitter.Width);
    Inc(SplitterTop, Splitter.Height);

    DockPanelPaint(Self);
    if I < (DockPanels.Count - 1) then begin
      if DockDirection = ddHorizontal then Align := alLeft
        else Align := alTop;
    end else Align := alClient;
  end;
end;

procedure TCDStyleRegions.SetVisible(const AValue: Boolean);
var
  I: Integer;
begin
  inherited SetVisible(AValue);
  with TCDManager(Manager) do
  for I := 0 to DockPanels.Count - 1 do

        //Show;
        //ShowMessage(IntToStr(Control.Tag));
      with TCDClientPanel(DockPanels[I]) do begin
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

procedure TCDStyleRegions.ChangeVisible(Control: TWinControl;
  Visible: Boolean);
begin
  inherited;
end;

end.

