unit UInt128;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

type
  { Int128 }

  Int128 = record
    Low: Int64;
    High: Int64;
    class operator Add(const A, B: Int128): Int128;
    class operator Subtract(const A, B: Int128): Int128;
    class operator Implicit(const A: Int64): Int128; overload;
    class operator Implicit(const A: UInt64): Int128; overload;
    class operator Implicit(const A: ShortInt): Int128; overload;
    class operator Implicit(const A: Int128): Int64; overload;
    class operator Implicit(const A: Int128): UInt64; overload;
    class operator Implicit(const A: Int128): Integer; overload;
    class operator Implicit(const A: Int128): ShortInt; overload;
    class operator Multiply(const A, B: Int128): Int128;
    class operator IntDivide(const A, B: Int128): Int128;
    class operator Divide(const A, B: Int128): Int128;
    class operator Modulus(const A, B: Int128): Int128;
    class operator LeftShift(const A: Int128; B: Byte): Int128;
    class operator RightShift(const A: Int128; B: Byte): Int128;
    class operator BitwiseAnd(const A, B: Int128): Int128;
    class operator BitwiseOr(const A, B: Int128): Int128;
    class operator LogicalAnd(const A: Boolean; B: Int128): Int128;
    class operator LogicalOr(const A: Boolean; B: Int128): Int128;
    class operator Equal(const A, B: Int128): Boolean;
    class operator NotEqual(const A, B: Int128): Boolean;
    class operator GreaterThan(const A, B: Int128): Boolean;
    class operator LessThan(const A, B: Int128): Boolean;
  end;

  UInt128 = record
    Low: UInt64;
    High: UInt64;
  end;

function IntToStr(Value: Int128): string;
function IntToHex(Value: Int128; Digits: integer): string;

const
   HexDigits: array[0..15] of Char = '0123456789ABCDEF';

resourcestring
  SRangeCheckError = 'Range check error';


implementation

function IntToStr(Value: Int128): string;
begin
  Result := '';
  while Value > 0 do begin
    Result := IntToStr(Value mod 10) + Result;
    Value := Value div 10;
  end;
end;

function IntToHex(Value: Int128; Digits: integer): string;
var
  I: Integer;
begin
  if Digits = 0 then
    Digits := 1;
  SetLength(Result, Digits);
  for I := 0 to Digits - 1 do
  begin
    Result[Digits - I] := HexDigits[Value and 15];
    Value := Value shr 4;
  end ;
  while Value <> 0 do begin
    Result := HexDigits[Value and 15] + Result;
    Value := Value shr 4;
  end;
end;

{ Int128 }

class operator Int128.LogicalAnd(const A: Boolean; B: Int128): Int128;
begin

end;

class operator Int128.LogicalOr(const A: Boolean; B: Int128): Int128;
begin

end;

class operator Int128.Add(const A, B: Int128): Int128;
begin
  Result.Low := A.Low + B.Low;
  Result.High := A.High + B.High;
end;

class operator Int128.Subtract(const A, B: Int128): Int128;
begin
  Result.Low := A.Low - B.Low;
  Result.High := A.High - B.High;
end;

class operator Int128.Implicit(const A: Int128): Integer;
begin

end;

class operator Int128.Implicit(const A: Int128): Int64;
begin

end;

class operator Int128.Implicit(const A: Int128): UInt64;
begin

end;

class operator Int128.Implicit(const A: Int128): ShortInt;
begin
  if (A.Low <= System.High(ShortInt)) and (A.Low >= System.Low(ShortInt)) then
    Result := A.Low
    else raise ERangeError.Create(SRangeCheckError);
end;

class operator Int128.Implicit(const A: ShortInt): Int128;
begin
  Result.Low := A;
  Result.High := 0;
end;

class operator Int128.Implicit(const A: Int64): Int128;
begin
  Result.Low := A;
  Result.High := 0;
end;

class operator Int128.Implicit(const A: UInt64): Int128;
begin
  Result.Low := A;
  Result.High := 0;
end;

class operator Int128.Equal(const A, B: Int128): Boolean;
begin
  Result := (A.Low = B.Low) and (A.High = B.High);
end;

class operator Int128.NotEqual(const A, B: Int128): Boolean;
begin
  Result := not ((A.Low = B.Low) and (A.High = B.High));
end;

class operator Int128.LessThan(const A, B: Int128): Boolean;
begin

end;

class operator Int128.GreaterThan(const A, B: Int128): Boolean;
begin

end;

class operator Int128.LeftShift(const A: Int128; B: Byte): Int128;
begin
  if B < 64 then begin
    Result.High := (A.High shl B);
    Result.High := Result.High or ((A.Low shr (64 - B)) and ((1 shl B) - 1));
    Result.Low := A.Low shl B;
  end else begin
    Result.High := A.High shl (B - 64);
    Result.Low := 0;
  end;
end;

class operator Int128.RightShift(const A: Int128; B: Byte): Int128;
begin
  if B < 64 then begin
    Result.Low := (A.Low shr B);
    Result.Low := Result.Low or (A.High and ((1 shl B) - 1)) shl (64 - B);
    Result.High := A.High shr B;
  end else begin
    Result.Low := A.High shr (B - 64);
    Result.High := 0;
  end;
end;

class operator Int128.BitwiseAnd(const A, B: Int128): Int128;
begin
  Result.Low := A.Low and B.Low;
  Result.High := A.High and B.High;
end;

class operator Int128.BitwiseOr(const A, B: Int128): Int128;
begin
  Result.Low := A.Low or B.Low;
  Result.High := A.High or B.High;
end;

class operator Int128.Divide(const A, B: Int128): Int128;
begin
end;

class operator Int128.IntDivide(const A, B: Int128): Int128;
begin
end;

class operator Int128.Multiply(const A, B: Int128): Int128;
begin
end;

class operator Int128.Modulus(const A, B: Int128): Int128;
begin
end;

end.

