{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ModularSystem;

interface

uses
  UModularSystem, UFormModuleList, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('UModularSystem', @UModularSystem.Register);
end;

initialization
  RegisterPackage('ModularSystem', @Register);
end.
