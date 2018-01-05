unit SpecializedRectangle;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, GenericPoint, Types, GenericRectangle;

type
  TRectangle = class(TGRectangle<Integer>)
  private
    procedure SetTRect(const AValue: TRect);
    function GetTRect: TRect;
  public
    property AsTRect: TRect read GetTRect write SetTRect;
  end;


implementation

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


end.

