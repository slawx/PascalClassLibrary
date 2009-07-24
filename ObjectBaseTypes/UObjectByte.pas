unit UObjectByte;

interface

uses
  Classes, SysUtils, UObjectTypeBase, UInterfacedBits;

type
  TByte = class(TInterfacedObject, IAssignable)
  private
    function GetAsBits: TInterfacedBits;
    procedure SetAsBits(const Value: TInterfacedBits);
  public
    Value: Byte;
    procedure Assign(Source: TInterfacedObject);
    property AsBits: TInterfacedBits read GetAsBits write SetAsBits;
  end;

implementation

{ TByte }

procedure TByte.Assign(Source: TInterfacedObject);
begin
  if Source is TByte then Value := (Source as TByte).Value
  else raise EConvertError.Create('');
end;

function TByte.GetAsBits: TInterfacedBits;
var
  I: Integer;
begin
  Result := TInterfacedBits.Create;
  Result.Size := 8;
  for I := 0 to Result.Size - 1 do
    Result[I] := ((Value shr I) and 1) = 1;
end;

procedure TByte.SetAsBits(const Value: TInterfacedBits);
var
  Count: Integer;
  I: Integer;
begin
  Count := 8;
  if Value.Size > 8 then Count := 8;
  if Value.Size < 8 then Count := Value.Size;
  Self.Value := 0;
  for I := 0 to Count - 1 do
    Self.Value := Self.Value or (Ord(Value[I]) shl I);
end;

end.
