unit UModuleBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UModularSystem;

type
  TModuleBase = class(TModule)
    constructor Create; override;
    destructor Destroy; override;
  end;


implementation

{ TModuleUser }

constructor TModuleBase.Create;
begin
inherited;
  Name := 'Base';
  Version := '1.0';
end;

destructor TModuleBase.Destroy;
begin
  inherited Destroy;
end;

end.

