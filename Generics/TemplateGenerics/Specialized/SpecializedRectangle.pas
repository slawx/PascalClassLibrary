unit SpecializedRectangle;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, SpecializedPoint, Types;

type
{$MACRO ON}

// TRectangle = TGRectangle<Integer>
{$DEFINE TGRectangleDimension := Integer}
{$DEFINE TGRectangle := TBaseRectangle}
{$DEFINE TGRectanglePoint := TPoint}
{$DEFINE INTERFACE}
{$I 'GenericRectangle.inc'}
  TRectangle = class(TBaseRectangle)
  private
    procedure SetTRect(const AValue: TRect);
    function GetTRect: TRect;
  public
    property AsTRect: TRect read GetTRect write SetTRect;
  end;

// TRectangleDouble = TGRectangle<Double>
{$DEFINE TGRectangleDimension := Double}
{$DEFINE TGRectangle := TRectangleDouble}
{$DEFINE TGRectanglePoint := TPointDouble}
{$DEFINE INTERFACE}
{$I 'GenericRectangle.inc'}

implementation

{$DEFINE IMPLEMENTATION_USES}
{$I 'GenericRectangle.inc'}

// TRectangleInteger = TGRectangle<Integer>
{$DEFINE TGRectangleDimension := Integer}
{$DEFINE TGRectangle := TBaseRectangle}
{$DEFINE TGRectanglePoint := TPoint}
{$DEFINE IMPLEMENTATION}
{$I 'GenericRectangle.inc'}

function TRectangle.GetTRect: TRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Bottom := Bottom;
  Result.Right := Right;
end;

procedure TRectangle.SetTRect(const AValue: TRect);
begin
  Left := AValue.Left;
  Top := AValue.Top;
  Bottom := AValue.Bottom;
  Right := AValue.Right;
end;

// TRectangleDouble = TGRectangle<Double>
{$DEFINE TGRectangleDimension := Double}
{$DEFINE TGRectangle := TRectangleDouble}
{$DEFINE TGRectanglePoint := TPointDouble}
{$DEFINE IMPLEMENTATION}
{$I 'GenericRectangle.inc'}

end.

