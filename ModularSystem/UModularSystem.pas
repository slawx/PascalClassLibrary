unit UModularSystem;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

type
  TModuleManager = class;

  { TModule }

  TModule = class
  private
    FInstalled: Boolean;
    Manager: TModuleManager;
  public
    Version: string;
    Name: string;
    Title: string;
    Dependencies: TStringList;
    Author: string;
    Description: TStringList;
    License: string;
    procedure Install; virtual;
    procedure Uninstall; virtual;
    procedure Update; virtual;
    procedure EnumModulesInstall(ModuleList: TStringList);
    procedure EnumModulesUninstall(ModuleList: TStringList);
    constructor Create; virtual;
    destructor Destroy; override;
    property Installed: Boolean read FInstalled;
  end;

  { TModuleManager }

  TModuleManager = class
    Modules: TObjectList; // TObjectList<TModule>
    function FindModuleByName(Name: string): TModule;
    procedure InstallDependencies(Dependencies: TStringList);
    procedure UninstallDependencies(ModuleName: string);
    procedure EnumModulesInstall(Dependencies, ModuleList: TStringList);
    procedure EnumModulesUninstall(ModuleName: string; ModuleList: TStringList);
    procedure RegisterModule(Module: TModule);
    constructor Create;
    destructor Destroy; override;
  end;


implementation

resourcestring
  SModuleNotFound = 'Module %s not found';

{ TModuleManager }

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

procedure TModuleManager.RegisterModule(Module: TModule);
begin
  Modules.Add(Module);
  Module.Manager := Self;
end;

constructor TModuleManager.Create;
begin
  Modules := TObjectList.Create
end;

destructor TModuleManager.Destroy;
begin
  Modules.Free;
  inherited Destroy;
end;

{ TModule }

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
  Description.Free;
  Dependencies.Free;
  inherited Destroy;
end;

end.

