unit UModuleUser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UModularSystem;

type

  { TModuleUser }

  TModuleUser = class(TModule)
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TModuleUser }

constructor TModuleUser.Create;
begin
  inherited;
  Name := 'User';
  Version := '1.0';
  Dependencies.Add('Base');
end;

destructor TModuleUser.Destroy;
begin
  inherited Destroy;
end;

end.

