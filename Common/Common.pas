{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Common; 

interface

uses
  StopWatch, UCommon, UDebugLog, UDelay, UPrefixMultiplier, UURI, UThreading, 
  UMemory, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('UDebugLog', @UDebugLog.Register); 
end; 

initialization
  RegisterPackage('Common', @Register); 
end.
