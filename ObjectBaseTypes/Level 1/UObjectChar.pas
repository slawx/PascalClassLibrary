unit UObjectChar;

interface

uses
  UObjectTypeBase, UObjectBoolean, Types, SysUtils, UObjectInteger;

type
  TCharacter = class(TInterfacedObject, IComparable, IAssignable)
    Value: Char;
    procedure Assign(Source: TInterfacedObject);
    function EqualTo(Operand: IComparable): TBoolean;
    function Ordinal: TInteger;
  end;

implementation

{ TCharacter }

procedure TCharacter.Assign(Source: TInterfacedObject);
begin

end;

function TCharacter.EqualTo(Operand: IComparable): TBoolean;
begin

end;

function TCharacter.Ordinal: TInteger;
begin
  Result.Value := Ord(Value);
end;

end.
