unit SpecializedPoint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{$MACRO ON}

type
// TPoint<Integer>
{$DEFINE TGPointType := Integer}
{$DEFINE TGPoint := TPoint}
{$DEFINE INTERFACE}
{$I 'GenericPoint.inc'}

// TSmallPoint<SmallInt>
{$DEFINE TGPointType := SmallInt}
{$DEFINE TGPoint := TSmallPoint}
{$DEFINE INTERFACE}
{$I 'GenericPoint.inc'}

// TPointSingle<Single>
{$DEFINE TGPointType := Single}
{$DEFINE TGPoint := TPointSingle}
{$DEFINE INTERFACE}
{$I 'GenericPoint.inc'}

// TPointDouble<Double>
{$DEFINE TGPointType := Double}
{$DEFINE TGPoint := TPointDouble}
{$DEFINE INTERFACE}
{$I 'GenericPoint.inc'}

implementation

// TPoint<Integer>
{$DEFINE TGPointType := Integer}
{$DEFINE TGPoint := TPoint}
{$DEFINE IMPLEMENTATION}
{$I 'GenericPoint.inc'}

// TSmallPoint<SmallInt>
{$DEFINE TGPointType := SmallInt}
{$DEFINE TGPoint := TSmallPoint}
{$DEFINE IMPLEMENTATION}
{$I 'GenericPoint.inc'}

// TPointSingle<Single>
{$DEFINE TGPointType := Single}
{$DEFINE TGPoint := TPointSingle}
{$DEFINE IMPLEMENTATION}
{$I 'GenericPoint.inc'}

// TPointDouble<Double>
{$DEFINE TGPointType := Double}
{$DEFINE TGPoint := TPointDouble}
{$DEFINE IMPLEMENTATION}
{$I 'GenericPoint.inc'}
end.

