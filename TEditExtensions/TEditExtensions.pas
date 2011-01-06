{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit TEditExtensions; 

interface

uses
  UTimeEdit, UIPv4Edit, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('UTimeEdit', @UTimeEdit.Register); 
  RegisterUnit('UIPv4Edit', @UIPv4Edit.Register); 
end; 

initialization
  RegisterPackage('TEditExtensions', @Register); 
end.
