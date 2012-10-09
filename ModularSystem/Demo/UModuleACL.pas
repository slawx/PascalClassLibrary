unit UModuleACL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UModularSystem;

type
  { TModuleACL }

  TModuleACL = class(TModule)
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


implementation

{ TModuleACL }

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

