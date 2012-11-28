{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit PrintPreview;

interface

uses
  UPrintPreview, UVectorCanvas, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('UPrintPreview', @UPrintPreview.Register);
end;

initialization
  RegisterPackage('PrintPreview', @Register);
end.
