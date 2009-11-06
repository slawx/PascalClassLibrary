unit UObjectInteger;

interface

uses
  SysUtils, Classes, UObjectTypeBase, UObjectBoolean;

type
  TInteger = class(TInterfacedObject, IAssignable, IOrderable)
  public
    Value: Integer;
    function Predecessor: IOrderable;
    function Successor: IOrderable;
    function Low: IOrderable;
    function High: IOrderable;
    procedure Assign(Source: TInterfacedObject);
    function EqualTo(Operand: TInterfacedObject): TBoolean;
    function HigherThen(Operand: IOrderable): TBoolean;
    function LowerThan(Operand: IOrderable): TBoolean;
    function Max(Operand1, Operand2: IOrderable): IOrderable;
    function Min(Operand1, Operand2: IOrderable): IOrderable;
  end;

implementation

uses
  UObjectString;

{ TInteger }

procedure TInteger.Assign(Source: TInterfacedObject);
begin
  if Source is TInteger then Value := (Source as TInteger).Value
  else if Source is TString then Value := StrToInt((Source as TString).Value)
  else raise EConvertError.Create('');
end;

function TInteger.EqualTo(Operand: TInterfacedObject): TBoolean;
begin
  if Operand is TInteger then begin
    Result := TBoolean.Create;
    Result.Value := Value = TInteger(Operand).Value;
  end else raise EInvalidCast.Create('Typecast error.');
end;

function TInteger.High: IOrderable;
begin

end;

function TInteger.HigherThen(Operand: IOrderable): TBoolean;
begin
  if TInterfacedObject(Operand) is TInteger then begin
    Result := TBoolean.Create;
    Result.Value := Value > TInteger(Operand).Value;
  end else raise EInvalidCast.Create('Typecast error.');
end;

function TInteger.Low: IOrderable;
begin

end;

function TInteger.LowerThan(Operand: IOrderable): TBoolean;
begin
  if TInterfacedObject(Operand) is TInteger then begin
    Result := TBoolean.Create;
    Result.Value := Value < TInteger(Operand).Value;
  end else raise EInvalidCast.Create('Typecast error.');
end;

function TInteger.Max(Operand1, Operand2: IOrderable): IOrderable;
begin

end;

function TInteger.Min(Operand1, Operand2: IOrderable): IOrderable;
begin

end;

function TInteger.Predecessor: IOrderable;
begin
  Result := TInteger.Create;
  TInteger(Result).Value := Pred(Value);
end;

function TInteger.Successor: IOrderable;
begin
  Result := TInteger.Create;
  TInteger(Result).Value := Succ(Value);
end;

end.
