unit SpecializedPoint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{$MACRO ON}

type
  TPoint2DIndex = (piX, piY);

// TPoint2D<TPoint2DIndex, Integer>
{$DEFINE TGPointIndex := TPoint2DIndex}
{$DEFINE TGPointType := Integer}
{$DEFINE TGPoint := TPoint2D}
{$DEFINE INTERFACE}
{$I 'GenericPoint.inc'}
implementation

// TPoint2D<TPoint2DIndex, Integer>
{$DEFINE TGPointIndex := T2DPointIndex}
{$DEFINE TGPointType := Integer}
{$DEFINE TGPoint := TPoint2D}
{$DEFINE IMPLEMENTATION}
{$I 'GenericPoint.inc'}

end.

