unit UInt128;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

type

  { UInt128 }

  UInt128 = packed record
    procedure SetZero;
    procedure SetOne;
    procedure SetMinimum;
    procedure SetMaximum;
    function IsZero: Boolean;
    function IsOne: Boolean;
    function IsMinimum: Boolean;
    function IsMaximum: Boolean;
    class function Min(A, B: UInt128): UInt128; static;
    class function Max(A, B: UInt128): UInt128; static;
    class function Zero: UInt128; static;
    class function One: UInt128; static;
    class function Minimum: UInt128; static;
    class function Maximum: UInt128; static;
    class function Compare(A, B: UInt128): Int8; static;
    class procedure IntDivMod(A, B: UInt128; var Q, R: UInt128); static;
    class operator Inc(A: UInt128): UInt128;
    class operator Dec(A: UInt128): UInt128;
    class operator Implicit(A: ShortInt): UInt128;
    class operator Implicit(A: Byte): UInt128;
    class operator Implicit(A: UInt128): Byte;
    class operator Implicit(A: UInt128): LongWord;
    class operator Implicit(A: UInt128): ShortInt;
    class operator BitwiseXor(A, B: UInt128): UInt128;
    class operator BitwiseAnd(A, B: UInt128): UInt128;
    class operator BitwiseOr(A, B: UInt128): UInt128;
    class operator Add(A, B: UInt128): UInt128;
    class operator Subtract(A, B: UInt128): UInt128;
    class operator Equal(A, B: UInt128): Boolean;
    class operator LessThan(A, B: UInt128): Boolean;
    class operator LessThanOrEqual(A, B: UInt128): Boolean;
    class operator GreaterThan(A, B: UInt128): Boolean;
    class operator GreaterThanOrEqual(A, B: UInt128): Boolean;
    class operator Multiply(A, B: UInt128): UInt128;
    class operator IntDivide(A, B: UInt128): UInt128;
    class operator Modulus(A, B: UInt128): UInt128;
    class operator LeftShift(A, B: UInt128): UInt128;
    class operator RightShift(A, B: UInt128): UInt128;
    case Integer of
      0: (Bytes: array[0..15] of Byte);
      1: (Words: array[0..7] of Word);
      2: (LongWords: array[0..3] of LongWord);
      3: (QWords: array[0..1] of QWord);
  end;
  PUInt128 = ^UInt128;

  { Int128 }

  Int128 = packed record
    procedure SetZero;
    procedure SetOne;
    procedure SetMinusOne;
    procedure SetMinimum;
    procedure SetMaximum;
    function IsZero: Boolean;
    function IsOne: Boolean;
    function IsMinusOne: Boolean;
    function IsMinimum: Boolean;
    function IsMaximum: Boolean;
    function IsNegative: Boolean;
    function IsPositive: Boolean;
    function Abs: Int128;
    function Sign: Int8;
    class function Min(A, B: Int128): Int128; static;
    class function Max(A, B: Int128): Int128; static;
    class function Zero: Int128; static;
    class function One: Int128; static;
    class function MinusOne: Int128; static;
    class function Minimum: Int128; static;
    class function Maximum: Int128; static;
    class function Compare(A, B: Int128): Int8; static;
    class procedure IntDivMod(A, B: Int128; var Q, R: Int128); static;
    class operator Inc(A: Int128): Int128;
    class operator Dec(A: Int128): Int128;
    class operator Implicit(A: ShortInt): Int128;
    class operator Implicit(A: Byte): Int128;
    class operator Implicit(A: Int64): Int128;
    class operator Implicit(A: UInt128): Int128;
    class operator Implicit(A: Int128): ShortInt;
    class operator Implicit(A: Int128): Byte;
    class operator Implicit(A: Int128): LongWord;
    class operator Implicit(A: Int128): UInt128;
    class operator BitwiseXor(A, B: Int128): Int128;
    class operator BitwiseAnd(A, B: Int128): Int128;
    class operator BitwiseOr(A, B: Int128): Int128;
    class operator Add(A, B: Int128): Int128;
    class operator Subtract(A, B: Int128): Int128;
    class operator Equal(A, B: Int128): Boolean;
    class operator LessThan(A, B: Int128): Boolean;
    class operator LessThanOrEqual(A, B: Int128): Boolean;
    class operator GreaterThan(A, B: Int128): Boolean;
    class operator GreaterThanOrEqual(A, B: Int128): Boolean;
    class operator Negative(A: Int128): Int128;
    class operator Multiply(A, B: Int128): Int128;
    class operator IntDivide(A, B: Int128): Int128;
    class operator Modulus(A, B: Int128): Int128;
    class operator LeftShift(A, B: Int128): Int128;
    class operator RightShift(A, B: Int128): Int128;
    case Integer of
      0: (Bytes: array[0..15] of Byte);
      1: (ShortInts: array[0..15] of ShortInt);
      2: (Words: array[0..7] of Word);
      3: (SmallInts: array[0..7] of SmallInt);
      4: (LongWords: array[0..3] of LongWord);
      5: (LongInts: array[0..3] of LongInt);
      6: (QWords: array[0..1] of QWord);
      7: (Int64s: array[0..1] of Int64);
  end;
  PInt128 = ^Int128;

function IntToStr(Value: Int128): string; overload;
function IntToStr(Value: UInt128): string; overload;
function IntToHex(Value: Int128; Digits: integer): string; overload;
function IntToHex(Value: UInt128; Digits: integer): string; overload;


implementation


const
   HexDigits: array[0..15] of Char = '0123456789ABCDEF';

// Raise errors using Error function in System.pas
{$IFOPT Q+}
procedure RaiseOverflowError;
begin
  Error(reIntOverflow);
end;
{$ENDIF}

{$IFOPT R+}
procedure RaiseRangeError;
begin
  Error(reRangeError);
end;
{$ENDIF}
procedure RaiseDivByZeroError;
begin
  Error(reDivByZero);
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

function IntToHex(Value: UInt128; Digits: integer): string;
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

function IntToStr(Value: Int128): string;
begin
  Result := '';
  if Value < 0 then begin
    Value := -Value;
    while Value > 9 do begin
      Result := Chr(Ord('0') + (Value mod 10)) + Result;
      Value := Value div 10;
    end;
    Result := '-' + Chr(Ord('0') + (Value mod 10)) + Result;
  end else begin
    while Value > 9 do begin
      Result := Chr(Ord('0') + (Value mod 10)) + Result;
      Value := Value div 10;
    end;
    Result := Chr(Ord('0') + Value) + Result;
  end;
end;

function IntToStr(Value: UInt128): string;
begin
  Result := '';
  while Value < 9 do begin
    Result := Chr(Ord('0') + (Value mod 10)) + Result;
    Value := Value div 10;
  end;
  Result := Chr(Ord('0') + (Value mod 10)) + Result;
end;

{ UInt128 }

procedure UInt128.SetZero;
begin
  QWords[0] := 0;
  QWords[1] := 0;
end;

procedure UInt128.SetOne;
begin
  QWords[0] := 1;
  QWords[1] := 0;
end;

procedure UInt128.SetMinimum;
begin
  SetZero;
end;

procedure UInt128.SetMaximum;
begin
  LongWords[0] := $ffffffff;
  LongWords[1] := $ffffffff;
  LongWords[2] := $ffffffff;
  LongWords[3] := $ffffffff;
end;

function UInt128.IsZero: Boolean;
begin
  Result := Self = UInt128.Zero;
end;

function UInt128.IsOne: Boolean;
begin
  Result := Self = UInt128.One;
end;

function UInt128.IsMinimum: Boolean;
begin
  Result := Self = UInt128.Minimum;
end;

function UInt128.IsMaximum: Boolean;
begin
  Result := Self = UInt128.Maximum;
end;

class function UInt128.Min(A, B: UInt128): UInt128;
begin
  if A < B then Result := A else Result := B;
end;

class function UInt128.Max(A, B: UInt128): UInt128;
begin
  if A > B then Result := A else Result := B;
end;

class function UInt128.Zero: UInt128;
begin
  Result.SetZero;
end;

class function UInt128.One: UInt128;
begin
  Result.SetOne;
end;

class function UInt128.Minimum: UInt128;
begin
  Result.SetMinimum;
end;

class function UInt128.Maximum: UInt128;
begin
  Result.SetMaximum;
end;

class function UInt128.Compare(A, B: UInt128): Int8;
var
  C, D: LongWord;
begin
  C := A.LongWords[3];
  D := B.LongWords[3];
  if C = D then
    begin
      C := A.LongWords[2];
      D := B.LongWords[2];
      if C = D then
        begin
          C := A.LongWords[1];
          D := B.LongWords[1];
          if C = D then
            begin
              C := A.LongWords[0];
              D := B.LongWords[0];
            end;
        end;
    end;
  if C > D then
    Result := 1 else
  if C < D then
    Result := -1
  else
    Result := 0;
end;

class operator UInt128.Implicit(A: ShortInt): UInt128;
begin
  {$IFOPT R+}
  if A < 0 then
    RaiseRangeError;
  {$ENDIF}
  Result.SetZero;
  Result.Bytes[0] := A;
end;

class operator UInt128.Implicit(A: Byte): UInt128;
begin
  Result.SetZero;
  Result.Bytes[0] := A;
end;

class operator UInt128.Implicit(A: UInt128): Byte;
begin
  Result := A.Bytes[0];
end;

class operator UInt128.Implicit(A: UInt128): LongWord;
begin
  Result := A.LongWords[0];
end;

class operator UInt128.Implicit(A: UInt128): ShortInt;
begin
  {$IFOPT R+}
  if not (A <= High(ShortInt)) then
    RaiseRangeError;
  {$ENDIF}
  Result := A.Bytes[0];
end;

class operator UInt128.BitwiseXor(A, B: UInt128): UInt128;
begin
  Result.LongWords[0] := A.LongWords[0] xor B.LongWords[0];
  Result.LongWords[1] := A.LongWords[1] xor B.LongWords[1];
end;

class operator UInt128.BitwiseAnd(A, B: UInt128): UInt128;
begin
  Result.LongWords[0] := A.LongWords[0] and B.LongWords[0];
  Result.LongWords[1] := A.LongWords[1] and B.LongWords[1];
end;

class operator UInt128.BitwiseOr(A, B: UInt128): UInt128;
begin
  Result.LongWords[0] := A.LongWords[0] or B.LongWords[0];
  Result.LongWords[1] := A.LongWords[1] or B.LongWords[1];
end;

class operator UInt128.Add(A, B: UInt128): UInt128;
var
  C: LongWord;
  D: Integer;
begin
  C := LongWord(A.Words[0]) + B.Words[0];
  Result.Words[0] := Word(C and $FFFF);

  for D := 1 to 7 do begin
    C := C shr 16;
    Inc(C, A.Words[D]);
    Inc(C, B.Words[D]);
    Result.Words[D] := Word(C and $FFFF);
  end;

  {$IFOPT Q+}
  C := C shr 16;
  if C > 0 then RaiseOverflowError;
  {$ENDIF}
end;

class operator UInt128.Subtract(A, B: UInt128): UInt128;
var
  C, D: Integer;
begin
  C := A.Words[0];
  Dec(C, B.Words[0]);
  Result.Words[0] := Word(C);

  for D := 1 to 7 do
    begin
      if C < 0 then C := -1 else C := 0;
      Inc(C, A.Words[D]);
      Dec(C, B.Words[D]);
      Result.Words[D] := Word(C);
    end;

  {$IFOPT Q+}
  if C < 0 then RaiseOverflowError;
  {$ENDIF}
end;

class operator UInt128.Equal(A, B: UInt128): Boolean;
begin
  Result := (A.LongWords[0] = B.LongWords[0]) and
    (A.LongWords[1] = B.LongWords[1]);
end;

class operator UInt128.LessThan(A, B: UInt128): Boolean;
var
  G: Int8;
begin
  G := Compare(A, B);
  Result := G = -1;
end;

class operator UInt128.LessThanOrEqual(A, B: UInt128): Boolean;
begin
  Result := not (A > B);
end;

class operator UInt128.GreaterThan(A, B: UInt128): Boolean;
begin
  Result := Compare(A, B) = 1;
end;

class operator UInt128.GreaterThanOrEqual(A, B: UInt128): Boolean;
begin
  Result := not (A < B);
end;

class operator UInt128.Multiply(A, B: UInt128): UInt128;
var
  C : Int64;
begin
  C := LongWord(A.Words[0]) * B.Words[0];
  Result.Words[0] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[1]);
  Inc(C, LongWord(A.Words[1]) * B.Words[0]);
  Result.Words[1] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[2]);
  Inc(C, LongWord(A.Words[1]) * B.Words[1]);
  Inc(C, LongWord(A.Words[2]) * B.Words[0]);
  Result.Words[2] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[3]);
  Inc(C, LongWord(A.Words[1]) * B.Words[2]);
  Inc(C, LongWord(A.Words[2]) * B.Words[1]);
  Inc(C, LongWord(A.Words[3]) * B.Words[0]);
  Result.Words[3] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[4]);
  Inc(C, LongWord(A.Words[1]) * B.Words[3]);
  Inc(C, LongWord(A.Words[2]) * B.Words[2]);
  Inc(C, LongWord(A.Words[3]) * B.Words[1]);
  Inc(C, LongWord(A.Words[4]) * B.Words[0]);
  Result.Words[4] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[5]);
  Inc(C, LongWord(A.Words[1]) * B.Words[4]);
  Inc(C, LongWord(A.Words[2]) * B.Words[3]);
  Inc(C, LongWord(A.Words[3]) * B.Words[2]);
  Inc(C, LongWord(A.Words[4]) * B.Words[1]);
  Inc(C, LongWord(A.Words[5]) * B.Words[0]);
  Result.Words[5] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[6]);
  Inc(C, LongWord(A.Words[1]) * B.Words[5]);
  Inc(C, LongWord(A.Words[2]) * B.Words[4]);
  Inc(C, LongWord(A.Words[3]) * B.Words[3]);
  Inc(C, LongWord(A.Words[4]) * B.Words[2]);
  Inc(C, LongWord(A.Words[5]) * B.Words[1]);
  Inc(C, LongWord(A.Words[6]) * B.Words[0]);
  Result.Words[6] := Word(C);

  C := C shr 16;
  Inc(C, LongWord(A.Words[0]) * B.Words[7]);
  Inc(C, LongWord(A.Words[1]) * B.Words[6]);
  Inc(C, LongWord(A.Words[2]) * B.Words[5]);
  Inc(C, LongWord(A.Words[3]) * B.Words[4]);
  Inc(C, LongWord(A.Words[4]) * B.Words[3]);
  Inc(C, LongWord(A.Words[5]) * B.Words[2]);
  Inc(C, LongWord(A.Words[6]) * B.Words[1]);
  Inc(C, LongWord(A.Words[7]) * B.Words[0]);
  Result.Words[7] := Word(C);

  {$IFOPT Q+}
  C := C shr 16;
  Inc(C, LongWord(A.Words[1]) * B.Words[7]);
  Inc(C, LongWord(A.Words[2]) * B.Words[6]);
  Inc(C, LongWord(A.Words[3]) * B.Words[5]);
  Inc(C, LongWord(A.Words[4]) * B.Words[4]);
  Inc(C, LongWord(A.Words[5]) * B.Words[3]);
  Inc(C, LongWord(A.Words[6]) * B.Words[2]);
  Inc(C, LongWord(A.Words[7]) * B.Words[1]);
  if C > 0 then
    RaiseOverflowError;

  C := C shr 16;
  Inc(C, LongWord(A.Words[2]) * B.Words[7]);
  Inc(C, LongWord(A.Words[3]) * B.Words[6]);
  Inc(C, LongWord(A.Words[4]) * B.Words[5]);
  Inc(C, LongWord(A.Words[5]) * B.Words[4]);
  Inc(C, LongWord(A.Words[6]) * B.Words[3]);
  Inc(C, LongWord(A.Words[7]) * B.Words[2]);
  if C > 0 then
    RaiseOverflowError;

  C := C shr 16;
  Inc(C, LongWord(A.Words[3]) * B.Words[7]);
  Inc(C, LongWord(A.Words[4]) * B.Words[6]);
  Inc(C, LongWord(A.Words[5]) * B.Words[5]);
  Inc(C, LongWord(A.Words[6]) * B.Words[4]);
  Inc(C, LongWord(A.Words[7]) * B.Words[3]);
  if C > 0 then
    RaiseOverflowError;

  C := C shr 16;
  Inc(C, LongWord(A.Words[4]) * B.Words[7]);
  Inc(C, LongWord(A.Words[5]) * B.Words[6]);
  Inc(C, LongWord(A.Words[6]) * B.Words[5]);
  Inc(C, LongWord(A.Words[7]) * B.Words[4]);
  if C > 0 then
    RaiseOverflowError;

  C := C shr 16;
  Inc(C, LongWord(A.Words[5]) * B.Words[7]);
  Inc(C, LongWord(A.Words[6]) * B.Words[6]);
  Inc(C, LongWord(A.Words[7]) * B.Words[5]);
  if C > 0 then
    RaiseOverflowError;

  C := C shr 16;
  Inc(C, LongWord(A.Words[6]) * B.Words[7]);
  Inc(C, LongWord(A.Words[7]) * B.Words[6]);
  if C > 0 then
    RaiseOverflowError;

  C := C shr 16;
  Inc(C, LongWord(A.Words[7]) * B.Words[7]);
  if C > 0 then
    RaiseOverflowError;
  {$ENDIF}
end;

class operator UInt128.IntDivide(A, B: UInt128): UInt128;
var
  M: UInt128;
begin
  IntDivMod(A, B, Result, M);
end;

class procedure UInt128.IntDivMod(A, B: UInt128; var Q, R: UInt128);
var
  C: Integer;
  D: UInt128;
begin
  // Handle special cases
  if B.IsZero then begin // B = 0
    RaiseDivByZeroError;
  end else
  if B.IsOne then begin// B = 1
    Q := A;
    R.SetZero;
    Exit;
  end else
  if A.IsZero then begin // A = 0
    Q.SetZero;
    R.SetZero;
    Exit;
  end;
  C := UInt128.Compare(A, B);
  if C < 0 then begin // A < B
    R := A;
    Q.SetZero;
    Exit;
  end else
  if C = 0 then begin // A = B
    Q.SetOne;
    R.SetZero;
    Exit;
  end;
  // Divide using "restoring radix two" division
  D := A;
  R.SetZero; // remainder (128 bits)
  Q.SetZero; // quotient (128 bits)
  for C := 0 to 127 do begin
    // Shift high bit of dividend D into low bit of remainder R
    R := R shl 1;
    if D.LongWords[3] and $80000000 <> 0 then
      R.LongWords[0] := R.LongWords[0] or 1;
    D := D shl 1;
    // Shift quotient
    Q := Q shl 1;
    // Subtract divisor from remainder if large enough
    if UInt128.Compare(R, B) >= 0 then begin
      R := R - B;
      // Set result bit in quotient
      Q.LongWords[0] := Q.LongWords[0] or 1;
    end;
  end;
end;

class operator UInt128.Inc(A: UInt128): UInt128;
var
  C: LongWord;
  D: Integer;
begin
  C := A.Words[0];
  Inc(C);
  Result.Words[0] := Word(C and $FFFF);

  C := C shr 16;
  if C = 0 then Exit;
  Inc(C, A.Words[1]);
  Result.Words[1] := Word(C and $FFFF);

  for D := 2 to 7 do begin
    C := C shr 16;
    if C = 0 then Exit;
    Inc(C, A.Words[D]);
    Result.Words[D] := Word(C and $FFFF);
  end;

  {$IFOPT Q+}
  C := C shr 16;
  if C > 0 then RaiseOverflowError;
  {$ENDIF}
end;

class operator UInt128.Dec(A: UInt128): UInt128;
begin
  Result := A - 1;
end;

class operator UInt128.Modulus(A, B: UInt128): UInt128;
var
  D: UInt128;
begin
  IntDivMod(A, B, D, Result);
end;

class operator UInt128.LeftShift(A, B: UInt128): UInt128;
var
  C, D : Byte;
begin
  if B = 0 then begin
    Exit;
  end else
  if B = 1 then begin
    Result.LongWords[3] := (A.LongWords[3] shl 1) or (A.LongWords[2] shr 31);
    Result.LongWords[2] := (A.LongWords[2] shl 1) or (A.LongWords[1] shr 31);
    Result.LongWords[1] := (A.LongWords[1] shl 1) or (A.LongWords[0] shr 31);
    Result.LongWords[0] := (A.LongWords[0] shl 1);
  end else
  if B >= 128 then begin
    A.SetZero;
  end else
  if B < 32 then begin // 1 <= B <= 31
    C := 32 - B;
    Result.LongWords[3] := (A.LongWords[3] shl B) or (A.LongWords[2] shr C);
    Result.LongWords[2] := (A.LongWords[2] shl B) or (A.LongWords[1] shr C);
    Result.LongWords[1] := (A.LongWords[1] shl B) or (A.LongWords[0] shr C);
    Result.LongWords[0] := (A.LongWords[0] shl B);
  end else
  if B < 64 then begin // 32 <= B <= 63
    D := B - 32;
    C := 32 - D;
    Result.LongWords[3] := (A.LongWords[2] shl D) or (A.LongWords[1] shr C);
    Result.LongWords[2] := (A.LongWords[1] shl D) or (A.LongWords[0] shr C);
    Result.LongWords[1] := (A.LongWords[0] shl D);
    Result.LongWords[0] := 0;
  end else
  if B < 96 then begin // 64 <= B <= 95
    D := B - 64;
    C := 32 - D;
    Result.LongWords[3] := (A.LongWords[1] shl D) or (A.LongWords[0] shr C);
    Result.LongWords[2] := (A.LongWords[0] shl D);
    Result.LongWords[1] := 0;
    Result.LongWords[0] := 0;
  end else begin          // 96 <= B <= 127
    D := B - 96;
    Result.LongWords[3] := (A.LongWords[0] shl D);
    Result.LongWords[2] := 0;
    Result.LongWords[1] := 0;
    Result.LongWords[0] := 0;
  end;
end;

class operator UInt128.RightShift(A, B: UInt128): UInt128;
var
  C, D : Byte;
begin
  if B = 0 then begin
    Exit;
  end else
  if B = 1 then begin
    Result.LongWords[0] := (A.LongWords[0] shr 1) or (A.LongWords[1] shl 31);
    Result.LongWords[1] := (A.LongWords[1] shr 1) or (A.LongWords[2] shl 31);
    Result.LongWords[2] := (A.LongWords[2] shr 1) or (A.LongWords[3] shl 31);
    Result.LongWords[3] := (A.LongWords[3] shr 1);
  end else
  if B >= 128 then begin
    A.SetZero;
  end else
  if B < 32 then begin // 1 <= B <= 31
    C := 32 - B;
    Result.LongWords[0] := (A.LongWords[0] shr B) or (A.LongWords[1] shl C);
    Result.LongWords[1] := (A.LongWords[1] shr B) or (A.LongWords[2] shl C);
    Result.LongWords[2] := (A.LongWords[2] shr B) or (A.LongWords[3] shl C);
    Result.LongWords[3] := (A.LongWords[3] shr B);
  end else
  if B < 64 then begin // 32 <= B <= 63
    D := B - 32;
    C := 32 - D;
    Result.LongWords[0] := (A.LongWords[1] shr D) or (A.LongWords[2] shl C);
    Result.LongWords[1] := (A.LongWords[2] shr D) or (A.LongWords[3] shl C);
    Result.LongWords[2] := (A.LongWords[3] shr D);
    Result.LongWords[3] := 0;
  end else
  if B < 96 then begin // 64 <= B <= 95
    D := B - 64;
    C := 32 - D;
    Result.LongWords[0] := (A.LongWords[2] shr D) or (A.LongWords[3] shl C);
    Result.LongWords[1] := (A.LongWords[3] shr D);
    Result.LongWords[2] := 0;
    Result.LongWords[3] := 0;
  end else begin           // 96 <= B <= 127
    D := B - 96;
    Result.LongWords[0] := (A.LongWords[3] shr D);
    Result.LongWords[1] := 0;
    Result.LongWords[2] := 0;
    Result.LongWords[3] := 0;
  end;
end;

{ Int128 }

procedure Int128.SetZero;
begin
  QWords[0] := 0;
  QWords[1] := 0;
end;

procedure Int128.SetOne;
begin
  QWords[0] := 1;
  QWords[1] := 0;
end;

procedure Int128.SetMinusOne;
begin
  LongWords[0] := $ffffffff;
  LongWords[1] := $ffffffff;
  LongWords[2] := $ffffffff;
  LongWords[3] := $ffffffff;
end;

procedure Int128.SetMinimum;
begin
  LongWords[0] := 0;
  LongWords[1] := 0;
  LongWords[2] := 0;
  LongWords[3] := $80000000;
end;

procedure Int128.SetMaximum;
begin
  QWords[0] := 0;
  QWords[1] := $7fffffffffffffff;
end;

function Int128.IsNegative: Boolean;
begin
  Result := (LongWords[3] and $80000000) <> 0;
end;

function Int128.IsPositive: Boolean;
begin
  Result := (LongWords[3] and $80000000) = 0;
end;

function Int128.IsZero: Boolean;
begin
  Result := Self = Int128.Zero;
end;

function Int128.IsOne: Boolean;
begin
  Result := Self = Int128.One;
end;

function Int128.IsMinusOne: Boolean;
begin
  Result := Self = Int128.MinusOne;
end;

function Int128.IsMinimum: Boolean;
begin
  Result := Self = Minimum;
end;

function Int128.IsMaximum: Boolean;
begin
  Result := Self = Maximum;
end;

class function Int128.Zero: Int128;
begin
  Result.SetZero;
end;

class function Int128.One: Int128;
begin
  Result.SetOne;
end;

class function Int128.MinusOne: Int128;
begin
  Result.SetMinusOne;
end;

class function Int128.Minimum: Int128;
begin
  Result.SetMinimum;
end;

class function Int128.Maximum: Int128;
begin
  Result.SetMaximum;
end;

function Int128.Abs: Int128;
begin
  if Self < 0 then Result := -Self
    else Result := Self;
end;

function Int128.Sign: Int8;
begin
  if IsZero then Result := 0
  else if (LongWords[3] and $80000000) = 1 then Result := 1
  else Result := 1;
end;

class function Int128.Min(A, B: Int128): Int128;
begin
  if A < B then Result := A else Result := B;
end;

class function Int128.Max(A, B: Int128): Int128;
begin
  if A > B then Result := A else Result := B;
end;

class operator Int128.Implicit(A: ShortInt): Int128;
begin
  if A < 0 then begin
    Result.SetMinusOne;
    Result.ShortInts[0] := A;
  end else begin
    Result.SetZero;
    Result.ShortInts[0] := A;
  end;
end;

class operator Int128.Implicit(A: Byte): Int128;
begin
  Result.SetZero;
  Result.Bytes[0] := A;
end;

class operator Int128.Implicit(A: Int64): Int128;
begin
  Result.Int64s[0] := A;
  if A < 0 then Result.Int64s[1] := -1
    else Result.Int64s[1] := 0;
end;

class operator Int128.Implicit(A: Int128): Byte;
begin
  {$IFOPT R+}
  if not ((A <= High(Byte)) and (A >= Low(Byte))) then
    RaiseRangeError;
  {$ENDIF}
  Result := A.Bytes[0];
end;

class operator Int128.Implicit(A: Int128): LongWord;
begin
  Result := A.LongWords[0];
end;

class operator Int128.Implicit(A: Int128): UInt128;
begin
  {$IFOPT R+}
  if A.IsNegative then
    RaiseRangeError;
  {$ENDIF}
  Result.QWords[0] := A.QWords[0];
  Result.QWords[1] := A.QWords[1];
end;

class operator Int128.Implicit(A: UInt128): Int128;
begin
  {$IFOPT R+}
  if A.LongWords[3] and $80000000 <> 0 then
    RaiseRangeError;
  {$ENDIF}
  Result.LongWords[0] := A.LongWords[0];
  Result.LongWords[1] := A.LongWords[1];
  Result.LongWords[2] := A.LongWords[2];
  Result.LongWords[3] := A.LongWords[3];
end;

class operator Int128.Implicit(A: Int128): ShortInt;
begin
  {$IFOPT R+}
  if not ((A <= High(ShortInt)) and (A >= Low(ShortInt))) then
    RaiseRangeError;
  {$ENDIF}
  Result := A.ShortInts[0];
end;

class operator Int128.BitwiseXor(A, B: Int128): Int128;
begin
  Result.QWords[0] := A.QWords[0] xor B.QWords[0];
  Result.QWords[1] := A.QWords[1] xor B.QWords[1];
end;

class operator Int128.BitwiseAnd(A, B: Int128): Int128;
begin
  Result.QWords[0] := A.QWords[0] and B.QWords[0];
  Result.QWords[1] := A.QWords[1] and B.QWords[1];
end;

class operator Int128.BitwiseOr(A, B: Int128): Int128;
begin
  Result.QWords[0] := A.QWords[0] or B.QWords[0];
  Result.QWords[1] := A.QWords[1] or B.QWords[1];
end;

class function Int128.Compare(A, B: Int128): Int8;
var
  P, Q : Boolean;
begin
  P := A.LongWords[3] and $80000000 <> 0;
  Q := B.LongWords[3] and $80000000 <> 0;
  if P <> Q then
    if P then
      Result := -1
    else
      Result := 1
  else
    if A.LongWords[3] < B.LongWords[3] then
      Result := -1 else
    if A.LongWords[3] > B.LongWords[3] then
      Result := 1 else
    if A.LongWords[2] < B.LongWords[2] then
      Result := -1 else
    if A.LongWords[2] > B.LongWords[2] then
      Result := 1 else
    if A.LongWords[1] < B.LongWords[1] then
      Result := -1 else
    if A.LongWords[1] > B.LongWords[1] then
      Result := 1 else
    if A.LongWords[0] < B.LongWords[0] then
      Result := -1 else
    if A.LongWords[0] > B.LongWords[0] then
      Result := 1
    else
      Result := 0;
end;

class procedure Int128.IntDivMod(A, B: Int128; var Q, R: Int128);
var
  C, T: UInt128;
  D, E: UInt128;
begin
  C := A.Abs;
  D := B.Abs;
  UInt128.IntDivMod(C, D, T, E);
  if not (A.IsNegative xor B.IsNegative) then Q := T
    else Q := -Int128(T);
  R := E;
end;

class operator Int128.Inc(A: Int128): Int128;
begin
  Result := A + 1;
end;

class operator Int128.Dec(A: Int128): Int128;
begin
  Result := A - 1;
end;

class operator Int128.Add(A, B: Int128): Int128;
var
  {$IFOPT Q+}
  D, E : Boolean;
  {$ENDIF}
begin
  {$IFOPT Q+}
  D := A.LongWords[3] and $80000000 = 0;
  E := B.LongWords[3] and $80000000 = 0;
  {$ENDIF}

  Result.QWords[0] := A.QWords[0] + B.QWords[0];
  Result.QWords[1] := A.QWords[1] + B.QWords[1];
  if ((A.QWords[0] shr 63) = 1) and ((B.QWords[0] shr 63) = 1) then
    Result.QWords[1] := Result.QWords[1] + 1;

  {$IFOPT Q+}
  // Check overflow
  if A.LongWords[3] and $80000000 <> 0 then
    begin
      if D and not E then
        RaiseOverflowError;
    end
  else
    if not D and E then
      RaiseOverflowError;
  {$ENDIF}
end;

class operator Int128.Subtract(A, B: Int128): Int128;
var
  {$IFOPT Q+}
  D, E : Boolean;
  {$ENDIF}
begin
  {$IFOPT Q+}
  D := A.LongWords[3] and $80000000 = 0;
  E := B.LongWords[3] and $80000000 = 0;
  {$ENDIF}

  Result.QWords[0] := A.QWords[0] - B.QWords[0];
  Result.QWords[1] := A.QWords[1] - B.QWords[1];
  if ((A.QWords[0] shr 63) = 1) and ((B.QWords[0] shr 63) = 1) then
    Result.QWords[1] := Result.QWords[1] - 1;

  {$IFOPT Q+}
  // Check overflow
  if A.LongWords[3] and $80000000 <> 0 then
    begin
      if D and not E then
        RaiseOverflowError;
    end
  else
    if not D and E then
      RaiseOverflowError;
  {$ENDIF}
end;

class operator Int128.Equal(A, B: Int128): Boolean;
begin
  Result := (A.QWords[0] = B.QWords[0]) and (A.QWords[1] = B.QWords[1]);
end;

class operator Int128.LessThan(A, B: Int128): Boolean;
var
  G: Int8;
begin
  G := Compare(A, B);
  Result := G = -1;
end;

class operator Int128.LessThanOrEqual(A, B: Int128): Boolean;
begin
  Result := not (A > B);
end;

class operator Int128.GreaterThan(A, B: Int128): Boolean;
begin
  Result := Compare(A, B) = 1;
end;

class operator Int128.GreaterThanOrEqual(A, B: Int128): Boolean;
begin
  Result := not (A < B);
end;

class operator Int128.Negative(A: Int128): Int128;
begin
  Result := (A xor Int128.MinusOne) + 1;
end;

class operator Int128.Multiply(A, B: Int128): Int128;
begin
  Result := 0;
  while B.Abs > 0 do begin
    Result := Result + A;
    B := B - 1;
  end;
  if B.Sign = -1 then Result := -Result;
end;

class operator Int128.IntDivide(A, B: Int128): Int128;
var
  M: Int128;
begin
  IntDivMod(A, B, Result, M);
end;

class operator Int128.Modulus(A, B: Int128): Int128;
var
  D: Int128;
begin
  IntDivMod(A, B, D, Result);
end;

class operator Int128.LeftShift(A, B: Int128): Int128;
var
  C, D: Byte;
begin
  if B = 0 then begin
    Result := A;
  end else
  if B = 1 then begin
    Result.LongWords[3] := (A.LongWords[3] shl 1) or (A.LongWords[2] shr 31);
    Result.LongWords[2] := (A.LongWords[2] shl 1) or (A.LongWords[1] shr 31);
    Result.LongWords[1] := (A.LongWords[1] shl 1) or (A.LongWords[0] shr 31);
    Result.LongWords[0] := (A.LongWords[0] shl 1);
  end else
  if B >= 128 then begin
    Result.SetZero;
  end else
  if B < 32 then begin // 1 <= B <= 31
    C := 32 - B;
    Result.LongWords[3] := (A.LongWords[3] shl B) or (A.LongWords[2] shr C);
    Result.LongWords[2] := (A.LongWords[2] shl B) or (A.LongWords[1] shr C);
    Result.LongWords[1] := (A.LongWords[1] shl B) or (A.LongWords[0] shr C);
    Result.LongWords[0] := (A.LongWords[0] shl B);
  end else
  if B < 64 then begin // 32 <= B <= 63
    D := B - 32;
    C := 32 - D;
    Result.LongWords[3] := (A.LongWords[2] shl D) or (A.LongWords[1] shr C);
    Result.LongWords[2] := (A.LongWords[1] shl D) or (A.LongWords[0] shr C);
    Result.LongWords[1] := (A.LongWords[0] shl D);
    Result.LongWords[0] := 0;
  end else
  if B < 96 then begin // 64 <= B <= 95
    D := B - 64;
    C := 32 - D;
    Result.LongWords[3] := (A.LongWords[1] shl D) or (A.LongWords[0] shr C);
    Result.LongWords[2] := (A.LongWords[0] shl D);
    Result.LongWords[1] := 0;
    Result.LongWords[0] := 0;
  end else begin  // 96 <= B <= 127
    D := B - 96;
    Result.LongWords[3] := (A.LongWords[0] shl D);
    Result.LongWords[2] := 0;
    Result.LongWords[1] := 0;
    Result.LongWords[0] := 0;
  end;
end;

class operator Int128.RightShift(A, B: Int128): Int128;
var
  C, D: Byte;
begin
  if B = 0 then begin
    Result := A;
  end else
  if B = 1 then begin
    Result.LongWords[0] := (A.LongWords[0] shr 1) or (A.LongWords[1] shl 31);
    Result.LongWords[1] := (A.LongWords[1] shr 1) or (A.LongWords[2] shl 31);
    Result.LongWords[2] := (A.LongWords[2] shr 1) or (A.LongWords[3] shl 31);
    Result.LongWords[3] := (A.LongWords[3] shr 1);
  end else
  if B >= 128 then begin
    Result.SetZero;
  end else
  if B < 32 then begin // 1 <= B <= 31
    C := 32 - B;
    Result.LongWords[0] := (A.LongWords[0] shr Byte(B)) or (A.LongWords[1] shl C);
    Result.LongWords[1] := (A.LongWords[1] shr Byte(B)) or (A.LongWords[2] shl C);
    Result.LongWords[2] := (A.LongWords[2] shr Byte(B)) or (A.LongWords[3] shl C);
    Result.LongWords[3] := (A.LongWords[3] shr Byte(B));
  end else
  if B < 64 then begin // 32 <= B <= 63
    D := B - 32;
    C := 32 - D;
    Result.LongWords[0] := (A.LongWords[1] shr Byte(D)) or (A.LongWords[2] shl C);
    Result.LongWords[1] := (A.LongWords[2] shr Byte(D)) or (A.LongWords[3] shl C);
    Result.LongWords[2] := (A.LongWords[3] shr Byte(D));
    Result.LongWords[3] := 0;
  end else
  if B < 96 then begin // 64 <= B <= 95
    D := B - 64;
    C := 32 - D;
    Result.LongWords[0] := (A.LongWords[2] shr Byte(D)) or (A.LongWords[3] shl C);
    Result.LongWords[1] := (A.LongWords[3] shr Byte(D));
    Result.LongWords[2] := 0;
    Result.LongWords[3] := 0;
  end else begin           // 96 <= B <= 127
    D := B - 96;
    Result.LongWords[0] := (A.LongWords[3] shr Byte(D));
    Result.LongWords[1] := 0;
    Result.LongWords[2] := 0;
    Result.LongWords[3] := 0;
  end;
end;

end.

