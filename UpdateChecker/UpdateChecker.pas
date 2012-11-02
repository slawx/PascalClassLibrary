{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit UpdateChecker;

interface

uses
  UUpdateChecker, UFormDownloadProgress, UFormNewVersionOffer, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('UUpdateChecker', @UUpdateChecker.Register);
end;

initialization
  RegisterPackage('UpdateChecker', @Register);
end.
