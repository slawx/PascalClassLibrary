{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CoolTranslator;

interface

uses
  UCoolTranslator, ULanguages, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('UCoolTranslator', @UCoolTranslator.Register);
end;

initialization
  RegisterPackage('CoolTranslator', @Register);
end.
