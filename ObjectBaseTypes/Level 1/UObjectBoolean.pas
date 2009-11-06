unit UObjectBoolean;

interface

uses
  Classes, SysUtils, UObjectTypeBase;

type
  TBoolean = class;

  IComparable = interface
    function EqualTo(Operand: IComparable): TBoolean;
  end;

  TBoolean = class(TInterfacedObject, IComparable, IAssignable)
    Value: Boolean;
    procedure Invert;
    function EqualTo(Operand: IComparable): TBoolean;
    function HigherThen(Operand: TInterfacedObject): TBoolean;
    function LowerThan(Operand: TInterfacedObject): TBoolean;
    procedure Assign(Source: TInterfacedObject);
    function AndTo(Operand: TBoolean): TBoolean;
    function OrTo(Operand: TBoolean): TBoolean;
  end;

  IOrderable = interface
    function HigherThen(Operand: IOrderable): TBoolean;
    function LowerThan(Operand: IOrderable): TBoolean;
    function Max(Operand1, Operand2: IOrderable): IOrderable;
    function Min(Operand1, Operand2: IOrderable): IOrderable;
    function Predecessor: IOrderable;
    function Successor: IOrderable;
    function Low: IOrderable;
    function High: IOrderable;
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

function TBoolean.EqualTo(Operand: IComparable): TBoolean;
begin
  if TInterfacedObject(Operand) is TBoolean then begin
    Result := TBoolean.Create;
    Result.Value := Value = TBoolean(Operand).Value;
  end else raise EInvalidCast.Create('Typecast error');
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

function TBoolean.LowerThan(Operand: TInterfacedObject): TBoolean;
begin
  if Operand is TBoolean then begin
    Result := TBoolean.Create;
    Result.Value := Value < TBoolean(Operand).Value;
  end else raise EInvalidCast.Create('Typecast error');
end;

function TBoolean.OrTo(Operand: TBoolean): TBoolean;
begin
  if Operand is TBoolean then begin
    Result := TBoolean.Create;
    Result.Value := TBoolean(Operand).Value or Value;
  end else raise EInvalidCast.Create('Typecast error');
end;


end.
