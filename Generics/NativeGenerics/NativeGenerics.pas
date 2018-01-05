{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit NativeGenerics;

{$warn 5023 off : no warning about unused units}
interface

uses
  GenericBitmap, GenericDictionary, GenericList, GenericMatrix, GenericPoint, 
  GenericQueue, GenericRange, GenericRectangle, GenericSet, GenericStack, 
  GenericStream, GenericTree, SpecializedList, SpecializedRectangle, 
  SpecializedStream, UBinarySerializer, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('NativeGenerics', @Register);
end.
