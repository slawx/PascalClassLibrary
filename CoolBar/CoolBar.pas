{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CoolBar;

interface

uses
  UCoolBar, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('UCoolBar', @UCoolBar.Register);
end;

initialization
  RegisterPackage('CoolBar', @Register);
end.
