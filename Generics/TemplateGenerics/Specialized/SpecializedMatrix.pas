unit SpecializedMatrix;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type
  {$MACRO ON}

// TMatrixInteger<Integer, Integer, Integer>
{$DEFINE TGMatrixIndexX := Integer}
{$DEFINE TGMatrixIndexY := Integer}
{$DEFINE TGMatrixItem := Integer}
{$DEFINE TGMatrixIndex := TMatrixIntegerIndex}
{$DEFINE TGMatrixRow := TMatrixIntegerRow}
{$DEFINE TGMatrix := TMatrixInteger}
{$DEFINE INTERFACE}
{$I 'GenericMatrix.inc'}

// TMatrixByte<Integer, Integer, Byte>
{$DEFINE TGMatrixIndexX := Integer}
{$DEFINE TGMatrixIndexY := Integer}
{$DEFINE TGMatrixItem := Byte}
{$DEFINE TGMatrixIndex := TMatrixByteIndex}
{$DEFINE TGMatrixRow := TMatrixByteRow}
{$DEFINE TGMatrix := TMatrixByte}
{$DEFINE INTERFACE}
{$I 'GenericMatrix.inc'}

// TMatrixObject<Integer, Integer, TObject>
{$DEFINE TGMatrixIndexX := Integer}
{$DEFINE TGMatrixIndexY := Integer}
{$DEFINE TGMatrixItem := TObject}
{$DEFINE TGMatrixIndex := TMatrixObjectIndex}
{$DEFINE TGMatrixRow := TMatrixObjectRow}
{$DEFINE TGMatrix := TMatrixObject}
{$DEFINE INTERFACE}
{$I 'GenericMatrix.inc'}

implementation

{$DEFINE IMPLEMENTATION_USES}
{$I 'GenericMatrix.inc'}

// TMatrixInteger<Integer, Integer, Integer>
{$DEFINE TGMatrixIndexX := Integer}
{$DEFINE TGMatrixIndexY := Integer}
{$DEFINE TGMatrixItem := Integer}
{$DEFINE TGMatrixIndex := TMatrixIntegerIndex}
{$DEFINE TGMatrixRow := TMatrixIntegerRow}
{$DEFINE TGMatrix := TMatrixInteger}
{$DEFINE IMPLEMENTATION}
{$I 'GenericMatrix.inc'}

// TMatrixByte<Integer, Integer, Byte>
{$DEFINE TGMatrixIndexX := Integer}
{$DEFINE TGMatrixIndexY := Integer}
{$DEFINE TGMatrixItem := Byte}
{$DEFINE TGMatrixIndex := TMatrixByteIndex}
{$DEFINE TGMatrixRow := TMatrixByteRow}
{$DEFINE TGMatrix := TMatrixByte}
{$DEFINE IMPLEMENTATION}
{$I 'GenericMatrix.inc'}

// TMatrixObject<Integer, Integer, TObject>
{$DEFINE TGMatrixIndexX := Integer}
{$DEFINE TGMatrixIndexY := Integer}
{$DEFINE TGMatrixItem := TObject}
{$DEFINE TGMatrixIndex := TMatrixObjectIndex}
{$DEFINE TGMatrixRow := TMatrixObjectRow}
{$DEFINE TGMatrix := TMatrixObject}
{$DEFINE IMPLEMENTATION}
{$I 'GenericMatrix.inc'}

end.

