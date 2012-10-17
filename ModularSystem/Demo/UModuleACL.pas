unit UModuleACL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UModularSystem;

type
  { TModuleACL }

  TModuleACL = class(TModule)
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

{ TModuleACL }

procedure TModuleACL.DoStart;
begin
  MainForm.Log(Identification + ' started');
end;

procedure TModuleACL.DoStop;
begin
  MainForm.Log(Identification + ' stopped');
end;

procedure TModuleACL.DoInstall;
begin
  MainForm.Log(Identification + ' installed');
end;

procedure TModuleACL.DoUninstall;
begin
  MainForm.Log(Identification + ' uninstalled');
end;

constructor TModuleACL.Create(AOwner: TComponent);
begin
  inherited;
  Identification := 'UserACL';
  Title := 'User ACL';
  Version := '1.0';
  License := 'GNU/LGPLv3';
  Dependencies.Add('User');
end;

destructor TModuleACL.Destroy;
begin
  inherited Destroy;
end;

end.

