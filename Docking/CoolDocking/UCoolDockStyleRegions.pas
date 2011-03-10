unit UCoolDockStyleRegions;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, StdCtrls, UCoolDockStyle, Forms,
  Graphics, Contnrs, Buttons, UCoolDockClientPanel, UCoolDockCommon;

type
  TCoolDockStyleRegionsPanel = class(TPanel)

  end;

  { TCoolDockStyleRegions }

  TCoolDockStyleRegions = class(TCoolDockStyle)
  private
    function GetDirection(InsertAt: TAlign): TDockDirection;
  public
    FDockDirection: TDockDirection;
    //Panels: TObjectList; // TObjectList<TCoolDockStyleRegionsPanel>
    procedure InsertControl(AControl: TControl; InsertAt: TAlign); override;
    procedure RemoveControl(Control: TControl); override;
    function GetHeaderPos: THeaderPos; override;
    procedure SetHeaderPos(const AValue: THeaderPos); override;
    constructor Create(AManager: TCoolDockManagerBase);
    destructor Destroy; override;
    procedure UpdateClientSize; override;
    procedure SetVisible(const AValue: Boolean); override;
    procedure ChangeVisible(Control: TWinControl; Visible: Boolean); override;
    property DockDirection: TDockDirection read FDockDirection
      write FDockDirection;
  end;

implementation

uses
  UCoolDocking;


{ TCoolDockStyleRegions }

function TCoolDockStyleRegions.GetDirection(InsertAt: TAlign): TDockDirection;
begin
  Result := ddHorizontal;
  if (InsertAt = alTop) or (InsertAt = alBottom) then
    Result := ddVertical
  else
  if (InsertAt = alLeft) or (InsertAt = alRight) then
    Result := ddHorizontal
  else;
end;

procedure TCoolDockStyleRegions.InsertControl(AControl: TControl; InsertAt: TAlign);
var
  NewPanel: TCoolDockClientPanel;
  I: Integer;
  NewDirection: TDockDirection;
  NewConjoinDockForm: TCoolDockConjoinForm;
  NewDockSite: TWinControl;
begin
  inherited;
  with TCoolDockManager(Manager) do begin
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
        DockSite.ManualDock(NewConjoinDockForm.Panel);
        AControl.ManualDock(NewConjoinDockForm.Panel, nil, InsertAt);
        Exit;
      end;
    end;

    NewPanel := TCoolDockClientPanel.Create(nil);
    with NewPanel do begin
      Parent := TCoolDockManager(Manager).DockSite;
      OwnerDockManager := Self;
      if DockStyle = dsList then Visible := True;
      Header.PopupMenu := TCoolDockManager(Manager).PopupMenu;
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

procedure TCoolDockStyleRegions.RemoveControl(Control: TControl);
begin
  if TCoolDockManager(Manager).DockSite.DockClientCount = 2 then FDockDirection := ddNone;
  inherited RemoveControl(Control);
end;

function TCoolDockStyleRegions.GetHeaderPos: THeaderPos;
begin
//  Result := TCoolDockManager(Manager).;
end;

procedure TCoolDockStyleRegions.SetHeaderPos(const AValue: THeaderPos);
begin
  inherited SetHeaderPos(AValue);
end;

constructor TCoolDockStyleRegions.Create(AManager: TCoolDockManagerBase);
var
  I: Integer;
begin
  inherited;
  //Panels := TObjectList.Create;

  with TCoolDockManager(AManager) do
  for I := 0 to DockPanels.Count - 1 do begin
    if Assigned(TCoolDockClientPanel(DockPanels[I]).Splitter) then
      TCoolDockClientPanel(DockPanels[I]).Splitter.Visible := True;
    TCoolDockClientPanel(DockPanels[I]).Visible := True;
    TCoolDockClientPanel(DockPanels[I]).ClientAreaPanel.Parent := TCoolDockClientPanel(DockPanels[I]);
    TCoolDockClientPanel(DockPanels[I]).ClientAreaPanel.Visible := True;
    TCoolDockClientPanel(DockPanels[I]).Control.Visible := True;
  end;
end;

destructor TCoolDockStyleRegions.Destroy;
begin
  //Panels.Free;
  inherited Destroy;
end;

procedure TCoolDockStyleRegions.UpdateClientSize;
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
  with TCoolDockManager(Manager) do
  for I := 0 to DockPanels.Count - 1 do
  with TCoolDockClientPanel(DockPanels[I]) do begin
    Left := SplitterLeft;
    Top := SplitterTop;
    Height := TCoolDockManager(Manager).DockSite.Height div
      TCoolDockManager(Manager).DockSite.DockClientCount;
    Width := TCoolDockManager(Manager).DockSite.Width div
      TCoolDockManager(Manager).DockSite.DockClientCount;
    //TCoolDockClientPanel(FDockPanels[I]).DockPanelPaint(Self);
    if I < (DockPanels.Count - 1) then Align := BaseAlign
      else Align := alClient;

    Splitter.Left := SplitterLeft;
    Splitter.Top := SplitterTop;
    Inc(SplitterLeft, Width);
    Inc(SplitterTop, Height);
    Splitter.Parent := TCoolDockManager(Manager).DockSite;
    Splitter.Align := BaseAlign;
    Splitter.Visible := True;


    DockPanelPaint(Self);
    if I < (DockPanels.Count - 1) then begin
      if DockDirection = ddHorizontal then Align := alLeft
        else Align := alTop;
    end else Align := alClient;
  end;
end;

procedure TCoolDockStyleRegions.SetVisible(const AValue: Boolean);
var
  I: Integer;
begin
  inherited SetVisible(AValue);
  with TCoolDockManager(Manager) do
  for I := 0 to DockPanels.Count - 1 do

        //Show;
        //ShowMessage(IntToStr(Control.Tag));
      with TCoolDockClientPanel(DockPanels[I]) do begin
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

procedure TCoolDockStyleRegions.ChangeVisible(Control: TWinControl;
  Visible: Boolean);
begin
  inherited;
end;

end.

