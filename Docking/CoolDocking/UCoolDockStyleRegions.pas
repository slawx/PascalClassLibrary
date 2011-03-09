unit UCoolDockStyleRegions;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, StdCtrls, UCoolDockStyle,
  Graphics, Contnrs, Buttons, UCoolDockClientPanel, UCoolDockCommon;

type
  TCoolDockStyleRegionsPanel = class(TPanel)

  end;

  { TCoolDockStyleRegions }

  TCoolDockStyleRegions = class(TCoolDockStyle)
  private
  public
    Panels: TObjectList; // TObjectList<TCoolDockStyleRegionsPanel>
    function GetHeaderPos: THeaderPos; override;
    procedure SetHeaderPos(const AValue: THeaderPos); override;
    constructor Create(AManager: TCoolDockManagerBase);
    destructor Destroy; override;
    procedure UpdateClientSize; override;
    procedure SetVisible(const AValue: Boolean); override;
    procedure ChangeVisible(Control: TWinControl; Visible: Boolean); override;
  end;

implementation

uses
  UCoolDocking;


{ TCoolDockStyleRegions }

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
  Panels := TObjectList.Create;

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
  Panels.Free;
  inherited Destroy;
end;

procedure TCoolDockStyleRegions.UpdateClientSize;
var
  I: Integer;
begin
  inherited UpdateClientSize;
  with TCoolDockManager(Manager) do
  for I := 0 to DockPanels.Count - 1 do
  with TCoolDockClientPanel(DockPanels[I]) do begin
    Height := TCoolDockManager(Manager).DockSite.Height div
      TCoolDockManager(Manager).DockSite.DockClientCount;
    Width := TCoolDockManager(Manager).DockSite.Width div
      TCoolDockManager(Manager).DockSite.DockClientCount;
    //TCoolDockClientPanel(FDockPanels[I]).DockPanelPaint(Self);
    DockPanelPaint(Self);
    if I < (DockPanels.Count - 1) then begin
      if TCoolDockManager(Manager).DockDirection = ddHorizontal then Align := alLeft
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

