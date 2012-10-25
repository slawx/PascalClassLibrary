unit UModularSystem;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, URegistry, SpecializedList;

type
  TModuleManager = class;
  TModule = class;
  TListModule = class;

  TAPI = class(TComponent)

  end;

  TModuleCondition = (mcAll, mcEnabled, mcNotEnabled, mcInstalled, mcNotInstalled,
    mcRunning, mcNotRunning);
  TModuleConditions = set of TModuleCondition;
  TModuleAction = (maStart, maStop, maInstall, maUninstall, maUpgrade, maEnable,
    maDisable);
  TModuleActions = array of TModuleAction;

  { TModule }

  TModule = class(TComponent)
  private
    FCategory: string;
    FEnabled: Boolean;
    FReleaseTime: TDateTime;
    FRunning: Boolean;
    FInstalled: Boolean;
    FManager: TModuleManager;
    FVersion: string;
    FIdentification: string;
    FTitle: string;
    FLicense: string;
    FAuthor: string;
    FDependencies: TListString;
    FDescription: TListString;
    FFileName: string;
    FWebSite: string;
    procedure SetEnabled(AValue: Boolean);
    procedure SetInstalled(AValue: Boolean);
    procedure SetManager(AValue: TModuleManager);
    procedure SetRunning(AValue: Boolean);
  protected
    procedure DoStart; virtual;
    procedure DoStop; virtual;
    procedure DoInstall; virtual;
    procedure DoUninstall; virtual;
    procedure DoUpgrade; virtual;
  public
    API: TAPI;
    procedure Enable;
    procedure Disable;
    procedure Start;
    procedure Stop;
    procedure Restart;
    procedure Install;
    procedure Uninstall;
    procedure Reinstall;
    procedure Upgrade;
    procedure EnumDependenciesCascade(ModuleList: TListModule;
      Conditions: TModuleConditions = [mcAll]);
    procedure EnumSuperiorDependenciesCascade(ModuleList: TListModule;
      Conditions: TModuleConditions = [mcAll]);
    procedure SetInstalledState(Value: Boolean);
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    property Running: Boolean read FRunning write SetRunning;
    property Installed: Boolean read FInstalled write SetInstalled;
    property Enabled: Boolean read FEnabled write SetEnabled;
  published
    property Identification: string read FIdentification write FIdentification; // Unique system name
    property Manager: TModuleManager read FManager write SetManager;
    property Version: string read FVersion write FVersion;
    property ReleaseTime: TDateTime read FReleaseTime write FReleaseTime;
    property Title: string read FTitle write FTitle;
    property License: string read FLicense write FLicense;
    property Author: string read FAuthor write FAuthor;
    property Dependencies: TListString read FDependencies write FDependencies;
    property Description: TListString read FDescription write FDescription;
    property FileName: string read FFileName write FFileName;
    property Category: string read FCategory write FCategory;
    property WebSite: string read FWebSite write FWebSite;
    // Screenshots, reviews, icon, weak dependencies, ...
  end;

  { TListModule }

  TListModule = class(TListObject)
  private
  public
    procedure Perform(Actions: array of TModuleAction; Conditions: TModuleConditions = [mcAll]);
    function FindByName(Name: string): TModule;
  end;

  TModuleManagerOption = (moAutoInstallOnRun, moAuto);
  TModuleManagerOptions = set of TModuleManagerOption;
  { TModuleManager }

  TModuleManager = class(TComponent)
  private
    FAPI: TAPI;
    FOnUpdate: TNotifyEvent;
    FUpdateCount: Integer;
    FOptions: TModuleManagerOptions;
    procedure SetAPI(AValue: TAPI);
    procedure DoUpdate;
  public
    Modules: TListModule; // TObjectList<TModule>
    function ModuleRunning(Name: string): Boolean;
    procedure EnumDependenciesCascade(Module: TModule; ModuleList: TListModule;
      Conditions: TModuleConditions = [mcAll]);
    procedure EnumSuperiorDependenciesCascade(Module: TModule;
      ModuleList: TListModule; Conditions: TModuleConditions = [mcAll]);
    procedure RegisterModule(Module: TModule);
    procedure UnregisterModule(Module: TModule);
    procedure LoadFromRegistry(Context: TRegistryContext);
    procedure SaveToRegistry(Context: TRegistryContext);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Update;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property API: TAPI read FAPI write SetAPI;
  published
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
    property Options: TModuleManagerOptions read FOptions write FOptions;
  end;

procedure Register;


implementation

resourcestring
  SModuleNotFound = 'Module "%1:s" not found as dependency for module "%0:s"';

procedure Register;
begin
  RegisterComponents('ModularSystem', [TModuleManager, TModule]);
end;

{ TListModule }

procedure TListModule.Perform(Actions:  array of TModuleAction;
  Conditions: TModuleConditions = [mcAll]);
var
  I: Integer;
  A: Integer;
begin
  for I := 0 to Count - 1 do
  with TModule(Items[I]) do
    if (mcAll in Conditions) or
    (Running and (mcRunning in Conditions)) or
    (not Running and (mcNotRunning in Conditions)) or
    (Installed and (mcInstalled in Conditions)) or
    (not Installed and (mcNotInstalled in Conditions)) or
    (Enabled and (mcEnabled in Conditions)) or
    (not Enabled and (mcNotEnabled in Conditions)) then
    for A := 0 to High(Actions) do begin
      if Actions[A] = maStart then Start;
      if Actions[A] = maStop then Stop;
      if Actions[A] = maInstall then Install;
      if Actions[A] = maUninstall then Uninstall;
      if Actions[A] = maUpgrade then Upgrade;
      if Actions[A] = maEnable then Enabled := True;
      if Actions[A] = maDisable then Enabled := False;
    end;
end;

function TListModule.FindByName(Name: string): TModule;
var
  I: Integer;
begin
  I := 0;
  while (I < Count) and (TModule(Items[I]).Identification <> Name) do Inc(I);
  if I < Count then Result := TModule(Items[I])
    else Result := nil;
end;

{ TModuleManager }

procedure TModuleManager.SetAPI(AValue: TAPI);
var
  I: Integer;
begin
  if FAPI = AValue then Exit;
  FAPI := AValue;
  for I := 0 to Modules.Count - 1 do
    TModule(Modules[I]).API := FAPI;
end;

procedure TModuleManager.DoUpdate;
begin
  if Assigned(FOnUpdate) then FOnUpdate(Self);
end;

function TModuleManager.ModuleRunning(Name: string): Boolean;
var
  Module: TModule;
begin
  Module := Modules.FindByName(Name);
  if Assigned(Module) then begin
    Result := Module.Running;
  end else Result := False;
end;

procedure TModuleManager.EnumDependenciesCascade(Module: TModule;
  ModuleList: TListModule; Conditions: TModuleConditions = [mcAll]);
var
  DepModule: TModule;
  I: Integer;
begin
  for I := 0 to Module.Dependencies.Count - 1 do begin
    DepModule := Modules.FindByName(Module.Dependencies[I]);
    if Assigned(DepModule) then
    with DepModule do begin
      if (ModuleList.IndexOf(DepModule) = -1) and
        ((mcAll in Conditions) or
        (Running and (mcRunning in Conditions)) or
        (not Running and (mcNotRunning in Conditions)) or
        (Installed and (mcInstalled in Conditions)) or
        (not Installed and (mcNotInstalled in Conditions)) or
        (Enabled and (mcEnabled in Conditions)) or
        (not Enabled and (mcNotEnabled in Conditions))) then begin
          ModuleList.Add(DepModule);
          Self.EnumDependenciesCascade(DepModule, ModuleList);
        end;
    end else raise Exception.CreateFmt(SModuleNotFound, [DepModule.Identification]);
  end;
end;

procedure TModuleManager.EnumSuperiorDependenciesCascade(Module: TModule;
  ModuleList: TListModule; Conditions: TModuleConditions = [mcAll]);
var
  I: Integer;
begin
  for I := 0 to Modules.Count - 1 do
  with TModule(Modules[I]) do begin
    if (Dependencies.IndexOf(Module.Identification) <> -1) and
      (ModuleList.IndexOf(TModule(Modules[I])) = -1) and
    ((mcAll in Conditions) or
    (Running and (mcRunning in Conditions)) or
    (not Running and (mcNotRunning in Conditions)) or
    (Installed and (mcInstalled in Conditions)) or
    (not Installed and (mcNotInstalled in Conditions)) or
    (Enabled and (mcEnabled in Conditions)) or
    (not Enabled and (mcNotEnabled in Conditions))) then begin
      ModuleList.Add(TModule(Modules[I]));
      Self.EnumSuperiorDependenciesCascade(TModule(Modules[I]), ModuleList);
    end;
  end;
end;

procedure TModuleManager.RegisterModule(Module: TModule);
begin
  Modules.Add(Module);
  Module.FManager := Self;
  Module.API := API;
  Update;
end;

procedure TModuleManager.UnregisterModule(Module: TModule);
begin
  Modules.Remove(Module);
  Update;
end;

constructor TModuleManager.Create(AOwner: TComponent);
begin
  inherited;
  Modules := TListModule.Create;
  Modules.OwnsObjects := False;
end;

destructor TModuleManager.Destroy;
begin
  Modules.Perform([maStop]);
  FreeAndNil(Modules);
  inherited;
end;

procedure TModuleManager.LoadFromRegistry(Context: TRegistryContext);
var
  I: Integer;
begin
  with TRegistryEx.Create do
  try
    RootKey := Context.RootKey;
    for I := 0 to Modules.Count - 1 do
    with TModule(Modules[I]) do begin
      OpenKey(Context.Key + '\' + Identification, True);
      Enabled := ReadBoolWithDefault('Enable', Enabled);
    end;
  finally
    Free;
  end;
end;

procedure TModuleManager.SaveToRegistry(Context: TRegistryContext);
var
  I: Integer;
begin
  with TRegistryEx.Create do
  try
    RootKey := Context.RootKey;
    for I := 0 to Modules.Count - 1 do
    with TModule(Modules[I]) do begin
      OpenKey(Context.Key + '\' + Identification, True);
      WriteBool('Enable', Enabled);
    end;
  finally
    Free;
  end;
end;

procedure TModuleManager.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TModuleManager.EndUpdate;
begin
  if FUpdateCount > 0 then Dec(FUpdateCount);
  if FUpdateCount = 0 then DoUpdate;
end;

procedure TModuleManager.Update;
begin
  if FUpdateCount = 0 then DoUpdate;
end;

{ TModule }

procedure TModule.SetRunning(AValue: Boolean);
begin
  if FRunning = AValue then Exit;
  if AValue then Start else Stop;
end;

procedure TModule.DoStart;
begin

end;

procedure TModule.DoStop;
begin

end;

procedure TModule.DoInstall;
begin

end;

procedure TModule.DoUninstall;
begin

end;

procedure TModule.DoUpgrade;
begin

end;

procedure TModule.Enable;
var
  List: TListModule;
begin
  if Enabled then Exit;
  FEnabled := True;
  try
    List := TListModule.Create;
    List.OwnsObjects := False;
    EnumSuperiorDependenciesCascade(List);
    List.Perform([maEnable], [mcNotEnabled]);
  finally
    List.Free;
  end;
  Start; // Auto start enabled modules
  //Manager.Update;
end;

procedure TModule.Disable;
var
  List: TListModule;
begin
  if not Enabled then Exit;
  if FRunning then Stop; // Auto stop running modules
  FEnabled := False;
  try
    List := TListModule.Create;
    List.OwnsObjects := False;
    EnumSuperiorDependenciesCascade(List);
    List.Perform([maDisable], [mcEnabled]);
  finally
    List.Free;
  end;
  Manager.Update;
end;

procedure TModule.SetInstalled(AValue: Boolean);
begin
  if FInstalled = AValue then Exit;
  if AValue then Install else Uninstall;
end;

procedure TModule.SetManager(AValue: TModuleManager);
begin
  if FManager = AValue then Exit;
  if Assigned(FManager) then FManager.UnregisterModule(Self);
  FManager := AValue;
  if Assigned(FManager) then AValue.RegisterModule(Self);
end;

procedure TModule.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then Exit;
  if FEnabled then Enable else Disable;
end;

procedure TModule.Start;
var
  List: TListModule;
begin
  if not Enabled or Running then Exit;
  if not Installed then Install;  // Auto install not installed modules
  try
    List := TListModule.Create;
    List.OwnsObjects := False;
    EnumDependenciesCascade(List);
    List.Perform([maStart], [mcNotRunning]);
  finally
    List.Free;
  end;
  DoStart;
  FRunning := True;
  Manager.Update;
end;

procedure TModule.Stop;
var
  List: TListModule;
begin
  if not Running then Exit;
  FRunning := False;
  try
    List := TListModule.Create;
    List.OwnsObjects := False;
    EnumSuperiorDependenciesCascade(List);
    List.Perform([maStop], [mcRunning]);
  finally
    List.Free;
  end;
  DoStop;
  Manager.Update;
end;

procedure TModule.Restart;
begin
  Stop;
  Start;
end;

procedure TModule.Install;
var
  List: TListModule;
begin
  if Installed then Exit;
  try
    List := TListModule.Create;
    List.OwnsObjects := False;
    EnumDependenciesCascade(List);
    List.Perform([maInstall], [mcNotInstalled]);
  finally
    List.Free;
  end;
  FInstalled := True;
  DoInstall;
  Enable; // Auto enable installed module
  Manager.Update;
end;

procedure TModule.Uninstall;
var
  List: TListModule;
begin
  if not Installed then Exit;
  if Enabled then Disable; // Auto disable uninstalled module
  try
    List := TListModule.Create;
    List.OwnsObjects := False;
    EnumSuperiorDependenciesCascade(List);
    List.Perform([maUninstall], [mcInstalled]);
  finally
    List.Free;
  end;
  FInstalled := False;
  DoUninstall;
  Manager.Update;
end;

procedure TModule.Reinstall;
begin
  Uninstall;
  Install;
end;

procedure TModule.Upgrade;
begin
  if not Enabled or not Installed then Exit;
  if Running then try
    Stop;
    DoUpgrade;
  finally
    Start;
  end else DoUpgrade;
  Manager.Update;
end;

procedure TModule.EnumDependenciesCascade(ModuleList: TListModule;
  Conditions: TModuleConditions = [mcAll]);
begin
  ModuleList.Clear;
  Manager.EnumDependenciesCascade(Self, ModuleList, Conditions);
end;

procedure TModule.EnumSuperiorDependenciesCascade(ModuleList: TListModule;
  Conditions: TModuleConditions = [mcAll]);
begin
  ModuleList.Clear;
  Manager.EnumSuperiorDependenciesCascade(Self, ModuleList, Conditions);
end;

procedure TModule.SetInstalledState(Value: Boolean);
begin
  FInstalled := Value;
  Manager.Update;
end;

constructor TModule.Create(Owner: TComponent);
begin
  inherited;
  Dependencies := TListString.Create;
  Description := TListString.Create;
end;

destructor TModule.Destroy;
begin
  Running := False;
  FreeAndNil(FDescription);
  FreeAndNil(FDependencies);
  inherited;
end;

end.

