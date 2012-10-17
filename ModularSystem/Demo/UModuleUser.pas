unit UModuleUser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UModularSystem;

type

  { TModuleUser }

  TModuleUser = class(TModule)
  protected
    procedure DoInstall; override;
    procedure DoStart; override;
    procedure DoStop; override;
    procedure DoUninstall; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  UMainForm;

{ TModuleUser }

procedure TModuleUser.DoStart;
begin
  MainForm.Log(Identification + ' started');
end;

procedure TModuleUser.DoStop;
begin
  MainForm.Log(Identification + ' stopped');
end;

procedure TModuleUser.DoInstall;
begin
  MainForm.Log(Identification + ' installed');
end;

procedure TModuleUser.DoUninstall;
begin
  MainForm.Log(Identification + ' uninstalled');
end;

constructor TModuleUser.Create(AOwner: TComponent);
begin
  inherited;
  Identification := 'User';
  Title := 'User';
  Version := '1.0';
  License := 'GNU/LGPLv3';
  Dependencies.Add('Base');
end;

destructor TModuleUser.Destroy;
begin
  inherited Destroy;
end;

end.

