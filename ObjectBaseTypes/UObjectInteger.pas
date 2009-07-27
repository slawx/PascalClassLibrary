unit UObjectInteger;

interface

uses
  SysUtils, Classes, UObjectTypeBase, UObjectBoolean, UObjectNumber;

type
  TInteger = class(TInterfacedObject, IAssignable, IOrdinal)
  private
  public
    Value: Integer;
    function Ordinal: IOrdinal;
    function Predecessor: IOrdinal;
    function Successor: IOrdinal;
    function Low: IOrdinal;
    function High: IOrdinal;
    procedure Assign(Source: TInterfacedObject);
    function EqualTo(Operand: TInterfacedObject): TBoolean;
    function HigherThen(Operand: TInterfacedObject): TBoolean;
    function LowerThan(Operand: TInterfacedObject): TBoolean;
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

function TInteger.High: IOrdinal;
begin

end;

function TInteger.HigherThen(Operand: TInterfacedObject): TBoolean;
begin
  if Operand is TInteger then begin
    Result := TBoolean.Create;
    Result.Value := Value > TInteger(Operand).Value;
  end else raise EInvalidCast.Create('Typecast error.');
end;

function TInteger.Low: IOrdinal;
begin

end;

function TInteger.LowerThan(Operand: TInterfacedObject): TBoolean;
begin
  if Operand is TInteger then begin
    Result := TBoolean.Create;
    Result.Value := Value < TInteger(Operand).Value;
  end else raise EInvalidCast.Create('Typecast error.');
end;

function TInteger.Ordinal: IOrdinal;
begin

end;

function TInteger.Predecessor: IOrdinal;
begin
  Result := TInteger.Create;
  TInteger(Result).Value := Pred(Value);
end;

function TInteger.Successor: IOrdinal;
begin
  Result := TInteger.Create;
  TInteger(Result).Value := Succ(Value);
end;

end.
