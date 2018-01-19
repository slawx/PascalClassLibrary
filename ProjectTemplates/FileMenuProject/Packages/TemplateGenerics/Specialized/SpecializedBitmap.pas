  unit SpecializedBitmap;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils,
  Graphics, FPImage;

type
  {$MACRO ON}

// TBitmapTColor<Integer, Integer, TColor>
{$DEFINE TGBitmapIndexX := Integer}
{$DEFINE TGBitmapIndexY := Integer}
{$DEFINE TGBitmapItem := TColor}
{$DEFINE TGBitmapIndex := TBitmapTColorIndex}
{$DEFINE TGBitmapRow := TBitmapTColorRow}
{$DEFINE TGBitmapMatrix := TBitmapTColorMatrix}
{$DEFINE TGBitmap := TBitmapTColor}
{$DEFINE TGBitmapSortCompare := TBitmapTColorSortCompare}
{$DEFINE TGBitmapToStringConverter := TBitmapTColorToStringConverter}
{$DEFINE TGBitmapFromStringConverter := TBitmapTColorFromStringConverter}
{$DEFINE TGBitmapMerge := TBitmapTColorMerge}
{$DEFINE INTERFACE}
{$I 'GenericBitmap.inc'}

// TBitmapTFPColor<Integer, Integer, TFPColor>
{$DEFINE TGBitmapIndexX := Integer}
{$DEFINE TGBitmapIndexY := Integer}
{$DEFINE TGBitmapItem := TFPColor}
{$DEFINE TGBitmapIndex := TBitmapTFPColorIndex}
{$DEFINE TGBitmapRow := TBitmapTFPColorRow}
{$DEFINE TGBitmapMatrix := TBitmapTFPColorMatrix}
{$DEFINE TGBitmap := TBitmapTFPColor}
{$DEFINE TGBitmapSortCompare := TBitmapTFPColorSortCompare}
{$DEFINE TGBitmapToStringConverter := TBitmapTFPColorToStringConverter}
{$DEFINE TGBitmapFromStringConverter := TBitmapTFPColorFromStringConverter}
{$DEFINE TGBitmapMerge := TBitmapTFPColorMerge}
{$DEFINE INTERFACE}
{$I 'GenericBitmap.inc'}

implementation

{$DEFINE IMPLEMENTATION_USES}
{$I 'GenericBitmap.inc'}

// TBitmapTColor<Integer, Integer, TColor>
{$DEFINE TGBitmapIndexX := Integer}
{$DEFINE TGBitmapIndexY := Integer}
{$DEFINE TGBitmapItem := TColor}
{$DEFINE TGBitmapIndex := TBitmapTColorIndex}
{$DEFINE TGBitmapRow := TBitmapTColorRow}
{$DEFINE TGBitmapMatrix := TBitmapTColorMatrix}
{$DEFINE TGBitmap := TBitmapTColor}
{$DEFINE TGBitmapSortCompare := TBitmapTColorSortCompare}
{$DEFINE TGBitmapToStringConverter := TBitmapTColorToStringConverter}
{$DEFINE TGBitmapFromStringConverter := TBitmapTColorFromStringConverter}
{$DEFINE TGBitmapMerge := TBitmapTColorMerge}
{$DEFINE IMPLEMENTATION}
{$I 'GenericBitmap.inc'}

// TBitmapTFPColor<Integer, Integer, TFPColor>
{$DEFINE TGBitmapIndexX := Integer}
{$DEFINE TGBitmapIndexY := Integer}
{$DEFINE TGBitmapItem := TFPColor}
{$DEFINE TGBitmapIndex := TBitmapTFPColorIndex}
{$DEFINE TGBitmapRow := TBitmapTFPColorRow}
{$DEFINE TGBitmapMatrix := TBitmapTFPColorMatrix}
{$DEFINE TGBitmap := TBitmapTFPColor}
{$DEFINE TGBitmapSortCompare := TBitmapTFPColorSortCompare}
{$DEFINE TGBitmapToStringConverter := TBitmapTFPColorToStringConverter}
{$DEFINE TGBitmapFromStringConverter := TBitmapTFPColorFromStringConverter}
{$DEFINE TGBitmapMerge := TBitmapTFPColorMerge}
{$DEFINE IMPLEMENTATION}
{$I 'GenericBitmap.inc'}

end.

