{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GeneralRegistry;

interface

uses
  xmlreg, UGeneralRegistry, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('GeneralRegistry', @Register);
end.
