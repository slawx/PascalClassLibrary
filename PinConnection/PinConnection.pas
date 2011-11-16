{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit PinConnection; 

interface

uses
  UCommFrame, UCommHub, UCommPin, UCommSerialPort, UCommSocket, UCommThread, 
  UPacketBurst, USerialPort, UCommConcentrator, UCommDelay, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('PinConnection', @Register); 
end.
