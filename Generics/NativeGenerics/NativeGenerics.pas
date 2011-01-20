{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit NativeGenerics; 

interface

uses
  GenericList, GenericTree, GenericDictionary, GenericQueue, GenericRange, 
  GenericSet, GenericStack, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('NativeGenerics', @Register); 
end.
