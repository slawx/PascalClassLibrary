unit UObjectPoint;

interface

uses
  UObjectTypeBase, UObjectBoolean, Types, SysUtils;

type
  TPoint = class(TInterfacedObject, IComparable, IAssignable)
    Value: Types.TPoint;
    procedure Assign(Source: TInterfacedObject);
    function EqualTo(Operand: IComparable): TBoolean;
    function HigherThen(Operand: TInterfacedObject): TBoolean;
    function LowerThan(Operand: TInterfacedObject): TBoolean;
  end;

implementation

{ TPoint }

procedure TPoint.Assign(Source: TInterfacedObject);
begin
  if Source is TPoint then
    Value := TPoint(Source).Value
    else raise EInvalidCast.Create('Typecast error');
end;

function TPoint.EqualTo(Operand: IComparable): TBoolean;
begin
  Result := TBoolean.Create;
  if TInterfacedObject(Operand) is TPoint then begin
    Result.Value := (Value.X = TPoint(Operand).Value.X) and (Value.Y = TPoint(Operand).Value.Y);
  end else raise EInvalidCast.Create('Typecast error');
end;

function TPoint.HigherThen(Operand: TInterfacedObject): TBoolean;
begin

end;

function TPoint.LowerThan(Operand: TInterfacedObject): TBoolean;
begin

end;

end.
