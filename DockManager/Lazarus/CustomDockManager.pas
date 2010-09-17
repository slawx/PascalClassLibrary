{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CustomDockManager; 

interface

uses
  UCustomDockManager, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('UCustomDockManager', @UCustomDockManager.Register); 
end; 

initialization
  RegisterPackage('CustomDockManager', @Register); 
end.
