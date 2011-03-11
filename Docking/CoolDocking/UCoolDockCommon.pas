unit UCoolDockCommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Contnrs, StdCtrls, ExtCtrls, ComCtrls;

type
  TDockStyle = (dsList, dsTabs, dsPopupTabs, dsPopupList);
  TDockHideType = (dhtPermanent, dhtTemporal);
  TDockDirection = (ddNone, ddHorizontal, ddVertical);

  TCoolDockMasterBase = class;
  TCoolDockClientBase = class;

  { TCoolDockManagerBase }

  TCoolDockManagerBase = class(TDockManager)
  private
    FMaster: TCoolDockMasterBase;
    procedure SetMaster(const AValue: TCoolDockMasterBase);
  public
    property Master: TCoolDockMasterBase read FMaster write SetMaster;
  end;

  TCoolDockConjoinFormBase = class(TForm)
    constructor Create(TheOwner: TComponent); override;
  end;

  TCoolDockCustomizeBase = class(TComponent)
  private
    FMaster: TCoolDockMasterBase;
    procedure SetMaster(const AValue: TCoolDockMasterBase);
  published
    property Master: TCoolDockMasterBase read FMaster write SetMaster;
  end;

  { TCoolDockMasterBase }

  TCoolDockMasterBase = class(TComponent)
  private
    FCoolDockCustomize: TCoolDockCustomizeBase;
    FClients: TObjectList; // TList<TCoolDockClientBase>
    function GetClient(Index: Integer): TCoolDockClientBase;
    procedure SetCustomize(const AValue: TCoolDockCustomizeBase);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RegisterClient(Client: TCoolDockClientBase);
    procedure UnRegisterClient(Client: TCoolDockClientBase);
    property Clients[Index: Integer]: TCoolDockClientBase read GetClient;
  published
    property Customize: TCoolDockCustomizeBase read FCoolDockCustomize
      write SetCustomize;
  end;

  { TCoolDockClientBase }

  TCoolDockClientBase = class(TComponent)
  private
    FMaster: TCoolDockMasterBase;
    FPanel: TPanel;
    procedure SetMaster(const AValue: TCoolDockMasterBase);
    procedure SetPanel(const AValue: TPanel);
  published
    property Master: TCoolDockMasterBase read FMaster
      write SetMaster;
    property Panel: TPanel read FPanel
      write SetPanel;
  end;

function GetUniqueName(BaseName: string): string;

implementation

function GetUniqueName(BaseName: string): string;
var
  I: Integer;
begin
  I := 1;
  while Assigned(FindGlobalComponent(BaseName + IntToStr(I))) do Inc(I);
  Result := BaseName + IntToStr(I);
end;


{ TCoolDockManagerBase }

procedure TCoolDockManagerBase.SetMaster(const AValue: TCoolDockMasterBase);
begin
  if FMaster = AValue then Exit;
  FMaster := AValue;
end;

{ TCoolDockClientBase }

procedure TCoolDockClientBase.SetMaster(const AValue: TCoolDockMasterBase);
var
  FOldMaster: TCoolDockMasterBase;
begin
  if FMaster = AValue then Exit;
  FOldMaster := FMaster;
  FMaster := AValue;
  if Assigned(FOldMaster) then
    FOldMaster.UnregisterClient(Self);
  if Assigned(FMaster) then begin
    FMaster.RegisterClient(Self);
    if not (csDesigning in ComponentState) then begin
      if Assigned(TWinControl(Owner).DockManager) then
        TCoolDockManagerBase(TWinControl(Owner).DockManager).Master := FMaster;
      if Assigned(FPanel) then
        TCoolDockManagerBase(FPanel.DockManager).Master := FMaster;
    end;
  end;
end;

procedure TCoolDockClientBase.SetPanel(const AValue: TPanel);
var
  OldPanel: TPanel;
begin
  if FPanel = AValue then exit;
  OldPanel := FPanel;
  FPanel := AValue;
  if not (csDesigning in ComponentState) then begin
    if Assigned(FPanel) then
    with FPanel do begin
      DockSite := True;
      UseDockManager := True;
      //DockManager := TCoolDockManager.Create(FPanel);
    end else begin
      OldPanel.DockSite := False;
    end;
  end;
end;

{ TCoolDockConjoinFormBase }

constructor TCoolDockConjoinFormBase.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

procedure TCoolDockCustomizeBase.SetMaster(const AValue: TCoolDockMasterBase);
var
  OldMaster: TCoolDockMasterBase;
begin
  if FMaster = AValue then Exit;
  OldMaster := FMaster;
  FMaster := AValue;
  if Assigned(AValue) then begin
    FMaster.Customize := Self;
  end else begin
    OldMaster.Customize := nil;
  end;
end;

procedure TCoolDockMasterBase.SetCustomize(const AValue: TCoolDockCustomizeBase
  );
var
  OldCustomize: TCoolDockCustomizeBase;
begin
  if FCoolDockCustomize = AValue then Exit;
  OldCustomize := FCoolDockCustomize;
  FCoolDockCustomize := AValue;
  if Assigned(AValue) then begin
    FCoolDockCustomize.Master := Self;
  end else begin
    OldCustomize.Master := nil;
  end;
end;

constructor TCoolDockMasterBase.Create(AOwner: TComponent);
begin
  inherited;
  FClients := TObjectList.Create;
  FClients.OwnsObjects := False;
end;

destructor TCoolDockMasterBase.Destroy;
var
  I: Integer;
begin
  for I := FClients.Count - 1 downto 0 do
    TCoolDockClientBase(FClients[I]).Master := nil;
  FClients.Free;
  inherited Destroy;
end;

procedure TCoolDockMasterBase.RegisterClient(Client: TCoolDockClientBase);
begin
  if Assigned(Client) then
    if FClients.IndexOf(Client) = -1 then begin
      FClients.Add(Client);
      Client.Master := Self;
    end;
end;

procedure TCoolDockMasterBase.UnRegisterClient(Client: TCoolDockClientBase);
begin
  if Assigned(Client) then begin
    Client.Master := nil;
    FClients.Remove(Client);
  end;
end;

function TCoolDockMasterBase.GetClient(Index: Integer): TCoolDockClientBase;
begin
  Result := TCoolDockClientBase(FClients[Index]);
end;




end.

