{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit PersistentData;

interface

uses
  UPDServer, UPDClient, UPDClientXMLRPC, UPDClientINI, UPDClientMySQL, 
  UPDClientRegistry, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('PersistentData', @Register);
end.
