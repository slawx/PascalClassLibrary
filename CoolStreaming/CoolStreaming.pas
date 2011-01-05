{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CoolStreaming; 

interface

uses
    UStreamHelper, USubStream, UVarBlockSerializer, UBufferedFileStream, 
  UTextFileStream, UBitStream, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('CoolStreaming', @Register); 
end.
