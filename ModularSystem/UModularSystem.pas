unit UModularSystem;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

type
  TModuleManager = class;

  TAPI = class

  end;

  { TModule }

  TModule = class
  private
    FInstalled: Boolean;
    Manager: TModuleManager;
    procedure SetInstalled(AValue: Boolean);
  public
    Version: string;
    Name: string;
    Title: string;
    Dependencies: TStringList;
    Author: string;
    Description: TStringList;
    License: string;
    API: TAPI;
    MarkForInstall: Boolean;
    procedure Install; virtual;
    procedure Uninstall; virtual;
    procedure Update; virtual;
    procedure EnumModulesInstall(ModuleList: TStringList);
    procedure EnumModulesUninstall(ModuleList: TStringList);
    constructor Create; virtual;
    destructor Destroy; override;
    property Installed: Boolean read FInstalled write SetInstalled;
  end;

  { TModuleManager }

  TModuleManager = class(TComponent)
  private
    FAPI: TAPI;
    procedure SetAPI(AValue: TAPI);
  public
    Modules: TObjectList; // TObjectList<TModule>
    function FindModuleByName(Name: string): TModule;
    procedure InstallDependencies(Dependencies: TStringList);
    procedure UninstallDependencies(ModuleName: string);
    procedure EnumModulesInstall(Dependencies, ModuleList: TStringList);
    procedure EnumModulesUninstall(ModuleName: string; ModuleList: TStringList);
    procedure RegisterModule(Module: TModule; MarkForInstall: Boolean = False);
    procedure UnregisterModule(Module: TModule);
    procedure InstallMarked;
    procedure UninstallAll;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property API: TAPI read FAPI write SetAPI;
  end;


implementation

resourcestring
  SModuleNotFound = 'Module %s not found';

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
  while (I < Modules.Count) and (TModule(Modules[I]).Name <> Name) do Inc(I);
  if I < Modules.Count then Result := TModule(Modules[I])
    else Result := nil;
end;

procedure TModuleManager.InstallDependencies(Dependencies: TStringList);
var
  Module: TModule;
  I: Integer;
begin
  for I := 0 to Dependencies.Count - 1 do begin
    Module := FindModuleByName(Dependencies[I]);
    if Assigned(Module) then begin
      if not Module.Installed then Module.Install;
    end else raise Exception.CreateFmt(SModuleNotFound, [Module.Name]);
  end;
end;

procedure TModuleManager.UninstallDependencies(ModuleName: string);
var
  I: Integer;
begin
  for I := 0 to Modules.Count - 1 do
  with TModule(Modules[I]) do begin
    if Dependencies.IndexOf(ModuleName) <> - 1 then Uninstall;
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
      if not Module.Installed and (ModuleList.IndexOf(Module.Name) = -1) then begin
        ModuleList.Add(Module.Name);
        EnumModulesInstall(Module.Dependencies, ModuleList);
      end;
    end else raise Exception.CreateFmt(SModuleNotFound, [Module.Name]);
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
      (ModuleList.IndexOf(Name) = -1) then begin
      ModuleList.Add(Name);
      Self.EnumModulesUninstall(Name, ModuleList);
    end;
  end;
end;

procedure TModuleManager.RegisterModule(Module: TModule;
  MarkForInstall: Boolean = False);
begin
  Modules.Add(Module);
  Module.Manager := Self;
  Module.API := API;
  Module.MarkForInstall := MarkForInstall;
end;

procedure TModuleManager.UnregisterModule(Module: TModule);
begin
  Modules.Remove(Module);
end;

procedure TModuleManager.InstallMarked;
var
  I: Integer;
begin
  for I := 0 to Modules.Count - 1 do
  with TModule(Modules[I]) do
    if not Installed and MarkForInstall then Install;
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
  UninstallAll;
  FreeAndNil(FAPI);
  FreeAndNil(Modules);
  inherited Destroy;
end;

{ TModule }

procedure TModule.SetInstalled(AValue: Boolean);
begin
  if FInstalled = AValue then Exit;
  if AValue then Install else Uninstall;
end;

procedure TModule.Install;
begin
  if Installed then Exit;
  Manager.InstallDependencies(Dependencies);
  FInstalled := True;
end;

procedure TModule.Uninstall;
begin
  if not Installed then Exit;
  Manager.UninstallDependencies(Name);
  FInstalled := False;
end;

procedure TModule.Update;
begin
  if not Installed then Exit;
end;

procedure TModule.EnumModulesInstall(ModuleList: TStringList);
begin
  ModuleList.Clear;
  Manager.EnumModulesInstall(Dependencies, ModuleList);
end;

procedure TModule.EnumModulesUninstall(ModuleList: TStringList);
begin
  ModuleList.Clear;
  Manager.EnumModulesUninstall(Name, ModuleList);
end;

constructor TModule.Create;
begin
  Dependencies := TStringList.Create;
  Description := TStringList.Create;
end;

destructor TModule.Destroy;
begin
  Installed := False;
  Description.Free;
  Dependencies.Free;
  inherited Destroy;
end;

end.

