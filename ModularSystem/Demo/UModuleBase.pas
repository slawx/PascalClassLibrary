unit UModuleBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UModularSystem;

type

  { TModuleBase }

  TModuleBase = class(TModule)
  protected
    procedure DoStart; override;
    procedure DoStop; override;
    procedure DoInstall; override;
    procedure DoUninstall; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


implementation

uses
  UMainForm;

{ TModuleUser }

procedure TModuleBase.DoStart;
begin
  MainForm.Log(Identification + ' started');
end;

procedure TModuleBase.DoStop;
begin
  MainForm.Log(Identification + ' stopped');
end;

procedure TModuleBase.DoInstall;
begin
  MainForm.Log(Identification + ' installed');
end;

procedure TModuleBase.DoUninstall;
begin
  MainForm.Log(Identification + ' uninstalled');
end;

constructor TModuleBase.Create(AOwner: TComponent);
begin
inherited;
  Identification := 'Base';
  Title := 'Base';
  Version := '1.0';
  License := 'GNU/LGPLv3';
end;

destructor TModuleBase.Destroy;
begin
  inherited Destroy;
end;

end.

