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
{$DEFINE TGMatrixSortCompare := TMatrixIntegerSortCompare}
{$DEFINE TGMatrixToStringConverter := TMatrixIntegerToStringConverter}
{$DEFINE TGMatrixFromStringConverter := TMatrixIntegerFromStringConverter}
{$DEFINE TGMatrixMerge := TMatrixIntegerMerge}
{$DEFINE INTERFACE}
{$I 'GenericMatrix.inc'}

// TMatrixByte<Integer, Integer, Byte>
{$DEFINE TGMatrixIndexX := Integer}
{$DEFINE TGMatrixIndexY := Integer}
{$DEFINE TGMatrixItem := Byte}
{$DEFINE TGMatrixIndex := TMatrixByteIndex}
{$DEFINE TGMatrixRow := TMatrixByteRow}
{$DEFINE TGMatrix := TMatrixByte}
{$DEFINE TGMatrixSortCompare := TMatrixByteSortCompare}
{$DEFINE TGMatrixToStringConverter := TMatrixByteToStringConverter}
{$DEFINE TGMatrixFromStringConverter := TMatrixByteFromStringConverter}
{$DEFINE TGMatrixMerge := TMatrixByteMerge}
{$DEFINE INTERFACE}
{$I 'GenericMatrix.inc'}

// TMatrixObject<Integer, Integer, TObject>
{$DEFINE TGMatrixIndexX := Integer}
{$DEFINE TGMatrixIndexY := Integer}
{$DEFINE TGMatrixItem := TObject}
{$DEFINE TGMatrixIndex := TMatrixObjectIndex}
{$DEFINE TGMatrixRow := TMatrixObjectRow}
{$DEFINE TGMatrix := TMatrixObject}
{$DEFINE TGMatrixSortCompare := TMatrixObjectSortCompare}
{$DEFINE TGMatrixToStringConverter := TMatrixObjectToStringConverter}
{$DEFINE TGMatrixFromStringConverter := TMatrixObjectFromStringConverter}
{$DEFINE TGMatrixMerge := TMatrixStringMerge}
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
{$DEFINE TGMatrixSortCompare := TMatrixIntegerSortCompare}
{$DEFINE TGMatrixToStringConverter := TMatrixIntegerToStringConverter}
{$DEFINE TGMatrixFromStringConverter := TMatrixIntegerFromStringConverter}
{$DEFINE TGMatrixMerge := TMatrixIntegerMerge}
{$DEFINE IMPLEMENTATION}
{$I 'GenericMatrix.inc'}

// TMatrixByte<Integer, Integer, Byte>
{$DEFINE TGMatrixIndexX := Integer}
{$DEFINE TGMatrixIndexY := Integer}
{$DEFINE TGMatrixItem := Byte}
{$DEFINE TGMatrixIndex := TMatrixByteIndex}
{$DEFINE TGMatrixRow := TMatrixByteRow}
{$DEFINE TGMatrix := TMatrixByte}
{$DEFINE TGMatrixSortCompare := TMatrixByteSortCompare}
{$DEFINE TGMatrixToStringConverter := TMatrixByteToStringConverter}
{$DEFINE TGMatrixFromStringConverter := TMatrixByteFromStringConverter}
{$DEFINE TGMatrixMerge := TMatrixByteMerge}
{$DEFINE IMPLEMENTATION}
{$I 'GenericMatrix.inc'}

// TMatrixObject<Integer, Integer, TObject>
{$DEFINE TGMatrixIndexX := Integer}
{$DEFINE TGMatrixIndexY := Integer}
{$DEFINE TGMatrixItem := TObject}
{$DEFINE TGMatrixIndex := TMatrixObjectIndex}
{$DEFINE TGMatrixRow := TMatrixObjectRow}
{$DEFINE TGMatrix := TMatrixObject}
{$DEFINE TGMatrixSortCompare := TMatrixObjectSortCompare}
{$DEFINE TGMatrixToStringConverter := TMatrixObjectToStringConverter}
{$DEFINE TGMatrixFromStringConverter := TMatrixObjectFromStringConverter}
{$DEFINE TGMatrixMerge := TMatrixStringMerge}
{$DEFINE IMPLEMENTATION}
{$I 'GenericMatrix.inc'}

end.

