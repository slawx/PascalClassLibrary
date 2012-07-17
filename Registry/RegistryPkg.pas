{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RegistryPkg;

interface

uses
  xmlreg, URegistry, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('RegistryPkg', @Register);
end.
