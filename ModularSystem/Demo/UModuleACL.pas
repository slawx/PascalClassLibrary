unit UModuleACL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UModularSystem;

type
  { TModuleACL }

  TModuleACL = class(TModule)
    constructor Create; override;
    destructor Destroy; override;
  end;


implementation

{ TModuleACL }

constructor TModuleACL.Create;
begin
  inherited;
  Name := 'UserACL';
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

