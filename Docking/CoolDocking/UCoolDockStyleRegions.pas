unit UCoolDockStyleRegions;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, StdCtrls, UCoolDockStyle,
  Graphics, Contnrs, Buttons, UCoolDockClientPanel;

type
  TCoolDockStyleRegionsPanel = class(TPanel)

  end;

  { TCoolDockStyleRegions }

  TCoolDockStyleRegions = class(TCoolDockStyle)
    Header: TCoolDockHeader;
    Panels: TObjectList; // TObjectList<TCoolDockStyleRegionsPanel>
    constructor Create(AManager: TObject);
    destructor Destroy; override;
    procedure UpdateClientSize; override;
  end;

implementation

uses
  UCoolDocking;


{ TCoolDockStyleRegions }

constructor TCoolDockStyleRegions.Create(AManager: TObject);
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
  for I := 0 to DockPanels.Count - 1 do begin
    TCoolDockClientPanel(DockPanels[I]).Height := DockSite.Height div
      DockSite.DockClientCount;
    TCoolDockClientPanel(DockPanels[I]).Width := DockSite.Width div
      DockSite.DockClientCount;
    //TCoolDockClientPanel(FDockPanels[I]).DockPanelPaint(Self);
    TCoolDockClientPanel(DockPanels[I]).DockPanelPaint(Self);
  end;
end;



end.

