{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit TemplateGenerics;

interface

uses
  SpecializedList, SpecializedDictionary, SpecializedStack, SpecializedTree, 
  SpecializedQueue, SpecializedSet, SpecializedPoint, SpecializedMatrix, 
  SpecializedBitmap, SpecializedStream, SpecializedRectangle, 
  UBinarySerializer, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('TemplateGenerics', @Register);
end.
