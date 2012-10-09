unit UModularSystem;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, URegistry;

type
  TModuleManager = class;

  TAPI = class(TComponent)

  end;

  { TModule }

  TModule = class(TComponent)
  private
    FEnabled: Boolean;
    FRunning: Boolean;
    FInstalled: Boolean;
    Manager: TModuleManager;
    FVersion: string;
    FIdentification: string;
    FTitle: string;
    FLicense: string;
    FAuthor: string;
    FDependencies: TStringList;
    FDescription: TStringList;
    procedure SetEnabled(AValue: Boolean);
    procedure SetInstalled(AValue: Boolean);
    procedure SetRunning(AValue: Boolean);
  protected
    procedure BeforeStart; virtual;
    procedure AfterStart; virtual;
    procedure BeforeStop; virtual;
    procedure AfterStop; virtual;
  public
    API: TAPI;
    procedure Start; virtual;
    procedure Stop; virtual;
    procedure Install; virtual;
    procedure Uninstall; virtual;
    procedure Upgrade; virtual;
    procedure EnumModulesStart(ModuleList: TStringList);
    procedure EnumModulesStop(ModuleList: TStringList);
    procedure EnumModulesInstall(ModuleList: TStringList);
    procedure EnumModulesUninstall(ModuleList: TStringList);
    procedure SetInstalledState(Value: Boolean);
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    property Running: Boolean read FRunning write SetRunning;
    property Installed: Boolean read FInstalled write SetInstalled;
    property Enabled: Boolean read FEnabled write SetEnabled;
  published
    property Version: string read FVersion write FVersion;
    property Identification: string read FIdentification write FIdentification;
    property Title: string read FTitle write FTitle;
    property License: string read FLicense write FLicense;
    property Author: string read FAuthor write FAuthor;
    property Dependencies: TStringList read FDependencies write FDependencies;
    property Description: TStringList read FDescription write FDescription;
  end;

  TModuleEvent = procedure (Sender: TObject; Module: TModule) of object;

  { TModuleManager }

  TModuleManager = class(TComponent)
  private
    FAPI: TAPI;
    FOnModuleChange: TModuleEvent;
    procedure SetAPI(AValue: TAPI);
  public
    Modules: TObjectList; // TObjectList<TModule>
    function FindModuleByName(Name: string): TModule;
    function ModuleRunning(Name: string): Boolean;
    procedure StartDependencies(ModuleName: string; Dependencies: TStringList);
    procedure StopDependencies(ModuleName: string);
    procedure EnumModulesStart(Dependencies, ModuleList: TStringList);
    procedure EnumModulesStop(ModuleName: string; ModuleList: TStringList);
    procedure InstallDependencies(ModuleName: string; Dependencies: TStringList);
    procedure UninstallDependencies(ModuleName: string);
    procedure EnumModulesInstall(Dependencies, ModuleList: TStringList);
    procedure EnumModulesUninstall(ModuleName: string; ModuleList: TStringList);
    procedure RegisterModule(Module: TModule; Enabled: Boolean = True);
    procedure UnregisterModule(Module: TModule);
    procedure StartInstalled;
    procedure InstallEnabled;
    procedure StopAll;
    procedure UninstallAll;
    procedure LoadFromRegistry(Context: TRegistryContext);
    procedure SaveToRegistry(Context: TRegistryContext);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property API: TAPI read FAPI write SetAPI;
    property OnModuleChange: TModuleEvent read FOnModuleChange write FOnModuleChange;
  end;

procedure Register;


implementation

resourcestring
  SModuleNotFound = 'Module "%1:s" not found as dependency for module "%0:s"';

procedure Register;
begin
  RegisterComponents('ModularSystem', [TModuleManager, TModule]);
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

function TModuleManager.FindModuleByName(Name: string): TModule;
var
  I: Integer;
begin
  I := 0;
  while (I < Modules.Count) and (TModule(Modules[I]).Identification <> Name) do Inc(I);
  if I < Modules.Count then Result := TModule(Modules[I])
    else Result := nil;
end;

function TModuleManager.ModuleRunning(Name: string): Boolean;
var
  Module: TModule;
begin
  Module := FindModuleByName(Name);
  if Assigned(Module) then begin
    Result := Module.Running;
  end else Result := False;
end;

procedure TModuleManager.StartDependencies(ModuleName: string; Dependencies: TStringList);
var
  Module: TModule;
  I: Integer;
begin
  for I := 0 to Dependencies.Count - 1 do begin
    Module := FindModuleByName(Dependencies[I]);
    if Assigned(Module) and Module.Enabled then begin
      if not Module.Running then Module.Start;
    end else raise Exception.CreateFmt(SModuleNotFound, [ModuleName, Dependencies[I]]);
  end;
end;

procedure TModuleManager.StopDependencies(ModuleName: string);
var
  I: Integer;
begin
  for I := 0 to Modules.Count - 1 do
  with TModule(Modules[I]) do begin
    if (Dependencies.IndexOf(ModuleName) <> - 1) and Running then Stop;
  end;
end;

procedure TModuleManager.EnumModulesStart(Dependencies,
  ModuleList: TStringList);
var
  Module: TModule;
  I: Integer;
begin
  for I := 0 to Dependencies.Count - 1 do begin
    Module := FindModuleByName(Dependencies[I]);
    if Assigned(Module) then begin
      if not Module.Running and (ModuleList.IndexOf(Module.Identification) = -1) then begin
        ModuleList.Add(Module.Identification);
        EnumModulesStart(Module.Dependencies, ModuleList);
      end;
    end else raise Exception.CreateFmt(SModuleNotFound, [Module.Identification]);
  end;
end;

procedure TModuleManager.EnumModulesStop(ModuleName: string;
  ModuleList: TStringList);
var
  I: Integer;
begin
  for I := 0 to Modules.Count - 1 do
  with TModule(Modules[I]) do begin
    if (Dependencies.IndexOf(ModuleName) <> -1) and Running and
      (ModuleList.IndexOf(Identification) = -1) then begin
      ModuleList.Add(Identification);
      Self.EnumModulesStop(Identification, ModuleList);
    end;
  end;
end;

procedure TModuleManager.InstallDependencies(ModuleName: string;
  Dependencies: TStringList);
var
  Module: TModule;
  I: Integer;
begin
  for I := 0 to Dependencies.Count - 1 do begin
    Module := FindModuleByName(Dependencies[I]);
    if Assigned(Module) and Module.Enabled then begin
      if not Module.Installed then Module.Install;
    end else raise Exception.CreateFmt(SModuleNotFound, [ModuleName, Dependencies[I]]);
  end;
end;

procedure TModuleManager.UninstallDependencies(ModuleName: string);
var
  I: Integer;
begin
  for I := 0 to Modules.Count - 1 do
  with TModule(Modules[I]) do begin
    if (Dependencies.IndexOf(ModuleName) <> - 1) and Installed then Uninstall;
  end;
end;

procedure TModuleManager.EnumModulesInstall(Dependencies,
  ModuleList: TStringList);
var
  Module: TModule;
  I: Integer;
begin
  for I := 0 to Dependencies.Count - 1 do begin
    Module := FindModuleByName(Dependencies[I]);
    if Assigned(Module) then begin
      if not Module.Installed and (ModuleList.IndexOf(Module.Identification) = -1) then begin
        ModuleList.Add(Module.Identification);
        EnumModulesInstall(Module.Dependencies, ModuleList);
      end;
    end else raise Exception.CreateFmt(SModuleNotFound, [Module.Identification]);
  end;
end;

procedure TModuleManager.EnumModulesUninstall(ModuleName: string;
  ModuleList: TStringList);
var
  I: Integer;
begin
  for I := 0 to Modules.Count - 1 do
  with TModule(Modules[I]) do begin
    if (Dependencies.IndexOf(ModuleName) <> -1) and Installed and
      (ModuleList.IndexOf(Identification) = -1) then begin
      ModuleList.Add(Identification);
      Self.EnumModulesUninstall(Identification, ModuleList);
    end;
  end;
end;

procedure TModuleManager.RegisterModule(Module: TModule;
  Enabled: Boolean = True);
begin
  Modules.Add(Module);
  Module.Manager := Self;
  Module.API := API;
  Module.Enabled := Enabled;
end;

procedure TModuleManager.UnregisterModule(Module: TModule);
begin
  Modules.Remove(Module);
end;

procedure TModuleManager.StartInstalled;
var
  I: Integer;
begin
  for I := 0 to Modules.Count - 1 do
  with TModule(Modules[I]) do
    if not Running and Installed then Start;
end;

procedure TModuleManager.InstallEnabled;
var
  I: Integer;
begin
  for I := 0 to Modules.Count - 1 do
  with TModule(Modules[I]) do
    if not Installed and Enabled then Install;
end;

procedure TModuleManager.StopAll;
var
  I: Integer;
begin
  for I := 0 to Modules.Count - 1 do
  with TModule(Modules[I]) do
    if Running then Stop;
end;

procedure TModuleManager.UninstallAll;
var
  I: Integer;
begin
  for I := 0 to Modules.Count - 1 do
  with TModule(Modules[I]) do
    if Installed then Uninstall;
end;

constructor TModuleManager.Create(AOwner: TComponent);
begin
  inherited;
  Modules := TObjectList.Create;
end;

destructor TModuleManager.Destroy;
begin
  StopAll;
  FreeAndNil(Modules);
  inherited Destroy;
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
      Running := ReadBoolWithDefault('Run',  Enabled);
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
      WriteBool('Run', Running);
    end;
  finally
    Free;
  end;
end;

{ TModule }

procedure TModule.SetRunning(AValue: Boolean);
begin
  if FRunning = AValue then Exit;
  if AValue then Start else Stop;
end;

procedure TModule.BeforeStart;
begin
  if Running then Exit;
  if not Installed then Install;
  Manager.StartDependencies(Identification, Dependencies);
end;

procedure TModule.AfterStart;
begin
  FRunning := True;
end;

procedure TModule.BeforeStop;
begin
  if not Running then Exit;
  FRunning := False;
  Manager.StopDependencies(Identification);
end;

procedure TModule.AfterStop;
begin
end;

procedure TModule.SetInstalled(AValue: Boolean);
begin
  if FInstalled = AValue then Exit;
  if AValue then Install else Uninstall;
end;

procedure TModule.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then Exit;
  FEnabled := AValue;
  if not FEnabled and FInstalled then Uninstall;
end;

procedure TModule.Start;
begin
  BeforeStart;
  AfterStart;
end;

procedure TModule.Stop;
begin
  BeforeStop;
  AfterStop;
end;

procedure TModule.Install;
begin
  if Installed then Exit;
  Manager.InstallDependencies(Identification, Dependencies);
  FInstalled := True;
  if Assigned(Manager.FOnModuleChange) then
    Manager.FOnModuleChange(Manager, Self);
end;

procedure TModule.Uninstall;
begin
  if not Installed then Exit;
  if Running then Stop;
  Manager.UninstallDependencies(Identification);
  FInstalled := False;
  if Assigned(Manager.FOnModuleChange) then
    Manager.FOnModuleChange(Manager, Self);
end;

procedure TModule.Upgrade;
begin
  if not Running then Exit;
end;

procedure TModule.EnumModulesStart(ModuleList: TStringList);
begin
  ModuleList.Clear;
  Manager.EnumModulesStart(Dependencies, ModuleList);
end;

procedure TModule.EnumModulesStop(ModuleList: TStringList);
begin
  ModuleList.Clear;
  Manager.EnumModulesStop(Identification, ModuleList);
end;

procedure TModule.EnumModulesInstall(ModuleList: TStringList);
begin
  ModuleList.Clear;
  Manager.EnumModulesInstall(Dependencies, ModuleList);
end;

procedure TModule.EnumModulesUninstall(ModuleList: TStringList);
begin
  ModuleList.Clear;
  Manager.EnumModulesUninstall(Identification, ModuleList);
end;

procedure TModule.SetInstalledState(Value: Boolean);
begin
  FInstalled := Value;
  if Assigned(Manager.FOnModuleChange) then
    Manager.FOnModuleChange(Manager, Self);
end;

constructor TModule.Create(Owner: TComponent);
begin
  inherited;
  Dependencies := TStringList.Create;
  Description := TStringList.Create;
end;

destructor TModule.Destroy;
begin
  Running := False;
  Description.Free;
  Dependencies.Free;
  inherited Destroy;
end;

end.

