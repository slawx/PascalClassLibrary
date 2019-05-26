{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit DpiControls;

{$warn 5023 off : no warning about unused units}
interface

uses
  UDpiControls, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('UDpiControls', @UDpiControls.Register);
end;

initialization
  RegisterPackage('DpiControls', @Register);
end.
