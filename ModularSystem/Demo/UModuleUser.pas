unit UModuleUser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UModularSystem;

type

  { TModuleUser }

  TModuleUser = class(TModule)
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TModuleUser }

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

