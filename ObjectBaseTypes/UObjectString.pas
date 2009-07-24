unit UObjectString;

interface

uses
  SysUtils, UObjectTypeBase, UObjectBoolean, UObjectInteger;

type
  TString = class(TInterfacedObject, IAssignable, IComparable)
    Value: string;
    procedure Assign(Source: TInterfacedObject);
    function EqualTo(Operand: TInterfacedObject): TBoolean;
    function HigherThen(Operand: TInterfacedObject): TBoolean;
    function LowerThan(Operand: TInterfacedObject): TBoolean;
    function Length: TInteger;
    procedure UpperCase;
    procedure LowerCase;
    procedure Delete(Index: TInteger; Count: TInteger);
    procedure Insert(Index: TInteger; SubString: TString);
    function Pos(SubString: TString): TInteger;
  end;

implementation

{ TString }

procedure TString.Assign(Source: TInterfacedObject);
begin

end;

procedure TString.Delete(Index, Count: TInteger);
begin
  System.Delete(Value, Index.Value, Count.Value);
end;

function TString.EqualTo(Operand: TInterfacedObject): TBoolean;
begin
  if Operand is TString then begin
    Result := TBoolean.Create;
    Result.Value := Value = TString(Operand).Value;
  end else raise EInvalidCast.Create('Typecast error.');
end;

function TString.HigherThen(Operand: TInterfacedObject): TBoolean;
begin

end;

procedure TString.Insert(Index: TInteger; SubString: TString);
begin
  System.Insert(SubString.Value, Value, Index.Value);
end;

function TString.Length: TInteger;
begin
  Result.Value := System.Length(Value);
end;

procedure TString.LowerCase;
begin
  Value := SysUtils.LowerCase(Value);
end;

function TString.LowerThan(Operand: TInterfacedObject): TBoolean;
begin

end;

function TString.Pos(SubString: TString): TInteger;
begin
  Result.Value := System.Pos(SubString.Value, Value);
end;

procedure TString.UpperCase;
begin
  Value := SysUtils.UpperCase(Value);
end;


end.
