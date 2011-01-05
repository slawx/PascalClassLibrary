{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Common; 

interface

uses
    StopWatch, UCommon, UDebugLog, UDelay, UPrefixMultiplier, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('Common', @Register); 
end.
