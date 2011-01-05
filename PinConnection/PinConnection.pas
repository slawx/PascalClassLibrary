{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit PinConnection; 

interface

uses
    UCommFrame, UCommHub, UCommPin, UCommProtocol, UCommSerialPort, 
  UCommSocket, UCommThread, UPacketBurst, USerialPort, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('PinConnection', @Register); 
end.
