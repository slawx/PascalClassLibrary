{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CoolDocking; 

interface

uses
  UCoolDocking, UCoolDockCustomize, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('UCoolDocking', @UCoolDocking.Register); 
end; 

initialization
  RegisterPackage('CoolDocking', @Register); 
end.
