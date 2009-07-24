unit UObjectBoolean;

interface

uses
  Classes, SysUtils, UObjectTypeBase;

type
  TBoolean = class;

  IComparable = interface
    function EqualTo(Operand: TInterfacedObject): TBoolean;
    function HigherThen(Operand: TInterfacedObject): TBoolean;
    function LowerThan(Operand: TInterfacedObject): TBoolean;
  end;

  TBoolean = class(TInterfacedObject, IAssignable, IComparable, IOrdinal)
    Value: Boolean;
    function Ordinal: IOrdinal;
    function Predecessor: IOrdinal;
    function Successor: IOrdinal;
    function Low: IOrdinal;
    function High: IOrdinal;
    procedure Invert;
    function EqualTo(Operand: TInterfacedObject): TBoolean;
    function HigherThen(Operand: TInterfacedObject): TBoolean;
    function LowerThan(Operand: TInterfacedObject): TBoolean;
    procedure Assign(Source: TInterfacedObject);
    function AndTo(Operand: TBoolean): TBoolean;
    function OrTo(Operand: TBoolean): TBoolean;
  end;

implementation

uses
  UObjectInteger;

{ TBoolean }

function TBoolean.AndTo(Operand: TBoolean): TBoolean;
begin
  if Operand is TBoolean then begin
    Result := TBoolean.Create;
    Result.Value := TBoolean(Operand).Value and Value;
  end else raise EInvalidCast.Create('Typecast error');
end;

procedure TBoolean.Assign(Source: TInterfacedObject);
begin
  if Source is TBoolean then begin
    Value := TBoolean(Source).Value;
  end;
end;

function TBoolean.EqualTo(Operand: TInterfacedObject): TBoolean;
begin
  if Operand is TBoolean then begin
    Result := TBoolean.Create;
    Result.Value := Value = TBoolean(Operand).Value;
  end else raise EInvalidCast.Create('Typecast error');
end;

function TBoolean.High: IOrdinal;
begin
  Result := TBoolean.Create;
  TBoolean(Result).Value := True;
end;

function TBoolean.HigherThen(Operand: TInterfacedObject): TBoolean;
begin
  if Operand is TBoolean then begin
    Result := TBoolean.Create;
    Result.Value := Value > TBoolean(Operand).Value;
  end else raise EInvalidCast.Create('Typecast error');
end;

procedure TBoolean.Invert;
begin
  Value := not Value;
end;

function TBoolean.Low: IOrdinal;
begin
  Result := TBoolean.Create;
  TBoolean(Result).Value := False;
end;

function TBoolean.LowerThan(Operand: TInterfacedObject): TBoolean;
begin
  if Operand is TBoolean then begin
    Result := TBoolean.Create;
    Result.Value := Value < TBoolean(Operand).Value;
  end else raise EInvalidCast.Create('Typecast error');
end;

function TBoolean.Ordinal: IOrdinal;
begin
  Result := TInteger.Create;
  TInteger(Result).Value := Integer(Value);
end;

function TBoolean.OrTo(Operand: TBoolean): TBoolean;
begin
  if Operand is TBoolean then begin
    Result := TBoolean.Create;
    Result.Value := TBoolean(Operand).Value or Value;
  end else raise EInvalidCast.Create('Typecast error');
end;

function TBoolean.Predecessor: IOrdinal;
begin
  raise ENotImplemented.Create('Not implemented');
end;

function TBoolean.Successor: IOrdinal;
begin

end;

end.
