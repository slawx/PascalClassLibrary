{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MicroThreading; 

interface

uses
  UMicroThreading, UPlatform, UMicroThreadList, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('UMicroThreading', @UMicroThreading.Register); 
end; 

initialization
  RegisterPackage('MicroThreading', @Register); 
end.
