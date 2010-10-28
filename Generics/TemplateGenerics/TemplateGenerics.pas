{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit TemplateGenerics; 

interface

uses
    ListString, ListInteger, ListObject, ListPointer, TreeString, TreeInteger, 
  TreeObject, TreePointer, ListDouble, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('TemplateGenerics', @Register); 
end.
