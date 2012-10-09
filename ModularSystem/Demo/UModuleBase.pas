unit UModuleBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UModularSystem;

type
  TModuleBase = class(TModule)
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


implementation

{ TModuleUser }

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

