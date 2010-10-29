{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit TemplateGenerics; 

interface

uses
    ListDouble, ListInteger, ListObject, ListPointer, ListString, TreeInteger, 
  TreeObject, TreePointer, TreeString, DictionaryString, StackDouble, 
  StackInteger, StackObject, StackPointer, StackString, QueueDouble, 
  QueueInteger, QueueObject, QueuePointer, QueueString, ListChar, ListVariant, 
  SetInteger, SetString, SetChar, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('TemplateGenerics', @Register); 
end.
