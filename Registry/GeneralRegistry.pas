{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GeneralRegistry;

interface

uses
  UGeneralRegistry, UTest, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('UGeneralRegistry', @UGeneralRegistry.Register);
end;

initialization
  RegisterPackage('GeneralRegistry', @Register);
end.
