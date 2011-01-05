{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit TEditExtensions; 

interface

uses
  UEditTime, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('UEditTime', @UEditTime.Register); 
end; 

initialization
  RegisterPackage('TEditExtensions', @Register); 
end.
