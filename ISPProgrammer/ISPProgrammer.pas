{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ISPProgrammer;

interface

uses
  UCPUType, UISPProgrammer, UProgrammerType, UDallasProgrammer, UPrestoDLL, 
  UPresto, URFUProgrammer, Processors, SerialFlash, SPI, UISPprog, CfgMgr, 
  DataFlash, Delays, Globals, InpOut32, ISP, ISP_EEPROM, ISP_Flash, 
  ISP_Fusebits, ISP_Lockbits, ISP_UserSign, ISPLib, MemBuffer, PinsIO, 
  PortsIO, UIntelHexFile, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('ISPProgrammer', @Register);
end.
