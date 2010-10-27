{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit TemplateGenerics; 

interface

uses
    StringList, IntegerList, ObjectList, PointerList, StringTree, IntegerTree, 
  ObjectTree, PointerTree, DoubleList, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('TemplateGenerics', @Register); 
end.
