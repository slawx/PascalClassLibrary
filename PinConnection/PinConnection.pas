{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit PinConnection;

interface

uses
  USerialPort, UPacketBurst, UCommFrame, UCommHub, UCommPin, UCommSerialPort, 
  UCommThread, UCommConcentrator, UCommDelay, UCommTCPClient, UCommTCPServer, 
  UCommTelnet, UCommTelnetComPortOption, UCommMark, UPinConnection, 
  UCommConnector, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('UPinConnection', @UPinConnection.Register);
end;

initialization
  RegisterPackage('PinConnection', @Register);
end.
