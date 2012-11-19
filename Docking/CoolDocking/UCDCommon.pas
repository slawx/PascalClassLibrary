unit UCDCommon;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Contnrs, ExtCtrls, ComCtrls;

type
  TCDStyleType = (dsList, dsTabs, dsPopupTabs, dsPopupList);
  TCDHideType = (dhtPermanent, dhtTemporal);
  TCDDirection = (ddNone, ddHorizontal, ddVertical);
  THeaderPos = (hpAuto, hpLeft, hpTop, hpRight, hpBottom);

  TCDMasterBase = class;
  TCDClientBase = class;

  { TCDManagerBase }

  TCDManagerBase = class(TDockManager)
  private
    FMaster: TCDMasterBase;
    procedure SetMaster(const AValue: TCDMasterBase);
  public
    property Master: TCDMasterBase read FMaster write SetMaster;
  end;

  TCDConjoinFormBase = class(TForm)
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  TCDCustomizeBase = class(TComponent)
  private
    FMaster: TCDMasterBase;
    procedure SetMaster(const AValue: TCDMasterBase);
  published
    property Master: TCDMasterBase read FMaster write SetMaster;
  end;

  { TCDMasterBase }

  TCDMasterBase = class(TComponent)
  private
    FCoolDockCustomize: TCDCustomizeBase;
    FClients: TObjectList; // TList<TCoolDockClientBase>
    function GetClient(Index: Integer): TCDClientBase;
    procedure SetCustomize(const AValue: TCDCustomizeBase);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RegisterClient(Client: TCDClientBase);
    procedure UnRegisterClient(Client: TCDClientBase);
    property Clients[Index: Integer]: TCDClientBase read GetClient;
  published
    property Customize: TCDCustomizeBase read FCoolDockCustomize
      write SetCustomize;
  end;

  { TCDClientBase }

  TCDClientBase = class(TComponent)
  private
    FMaster: TCDMasterBase;
    FPanel: TPanel;
    procedure SetMaster(const AValue: TCDMasterBase);
  protected
    procedure SetPanel(const AValue: TPanel); virtual;
  published
    property Master: TCDMasterBase read FMaster
      write SetMaster;
    property Panel: TPanel read FPanel
      write SetPanel;
  end;

function GetUniqueName(BaseName: string): string;
function HeaderPosToTabPos(HeaderPos: THeaderPos): TTabPosition;
procedure DebugLog(Text: string); inline;

implementation

var
  UniqueNameCounter: Integer;

procedure DebugLog(Text: string); inline;
begin
  {$IFDEF DEBUG_COOLDOCK}
  WriteLn(FormatDateTime('hh:nn:ss.zzz', Time) + ' ' + Text);
  {$ENDIF}
end;

function GetUniqueName(BaseName: string): string;
begin
  Result := BaseName + IntToStr(UniqueNameCounter);
  Inc(UniqueNameCounter);
end;

function HeaderPosToTabPos(HeaderPos: THeaderPos): TTabPosition;
begin
  case HeaderPos of
    hpBottom: Result := tpBottom;
    hpLeft: Result := tpLeft;
    hpTop: Result := tpTop;
    hpRight: Result := tpRight;
    hpAuto: Result := tpTop;
  end;
end;

{ TCDManagerBase }

procedure TCDManagerBase.SetMaster(const AValue: TCDMasterBase);
begin
  if FMaster = AValue then Exit;
  FMaster := AValue;
end;

{ TCDClientBase }

procedure TCDClientBase.SetMaster(const AValue: TCDMasterBase);
var
  FOldMaster: TCDMasterBase;
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
        TCDManagerBase(TWinControl(Owner).DockManager).Master := FMaster;
      if Assigned(FPanel) then
        TCDManagerBase(FPanel.DockManager).Master := FMaster;
    end;
  end;
end;

procedure TCDClientBase.SetPanel(const AValue: TPanel);
begin
  if FPanel = AValue then exit;
  if Assigned(FPanel) then FPanel.DockSite := False;
  FPanel := AValue;
end;

{ TCDConjoinFormBase }

constructor TCDConjoinFormBase.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

procedure TCDCustomizeBase.SetMaster(const AValue: TCDMasterBase);
var
  OldMaster: TCDMasterBase;
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

procedure TCDMasterBase.SetCustomize(const AValue: TCDCustomizeBase
  );
var
  OldCustomize: TCDCustomizeBase;
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

constructor TCDMasterBase.Create(AOwner: TComponent);
begin
  inherited;
  FClients := TObjectList.Create;
  FClients.OwnsObjects := False;
end;

destructor TCDMasterBase.Destroy;
var
  I: Integer;
begin
  for I := FClients.Count - 1 downto 0 do
    TCDClientBase(FClients[I]).Master := nil;
  FClients.Free;
  inherited Destroy;
end;

procedure TCDMasterBase.RegisterClient(Client: TCDClientBase);
begin
  if Assigned(Client) then
    if FClients.IndexOf(Client) = -1 then begin
      FClients.Add(Client);
      Client.Master := Self;
    end;
end;

procedure TCDMasterBase.UnRegisterClient(Client: TCDClientBase);
begin
  if Assigned(Client) then begin
    Client.Master := nil;
    FClients.Remove(Client);
  end;
end;

function TCDMasterBase.GetClient(Index: Integer): TCDClientBase;
begin
  Result := TCDClientBase(FClients[Index]);
end;

initialization

UniqueNameCounter := 1;

end.

