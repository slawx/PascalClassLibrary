// 2010-03-30

unit UVarIntSerializer;

{$mode delphi}{$H+}

interface

uses
  Classes, DateUtils, UMemoryStreamEx, Math, Dialogs, SysUtils;

const
  BitAlignment = 8;
  RealBase = 2;

type
  { TVarIntSerializer }

  TVarIntSerializer = class(TMemoryStreamEx)
    // Base
    procedure WriteVarUInt(Value: QWord);
    function ReadVarUInt: QWord;
    procedure WriteVarIntStream(Stream: TMemoryStream);
    procedure ReadVarIntStream(Stream: TMemoryStream);

    // Advanced data types
    procedure WriteVarSInt(Value: Int64);
    function ReadVarSInt: Int64;
    procedure WriteVarFloat(Value: Double);
    function ReadVarFloat: Double;
    procedure WriteVarString(Value: string);
    function ReadVarString: string;

    function TestMask(Mask, BitIndex: Integer): Boolean;
    constructor Create;
  end;

implementation

{ TVarIntSerializer }

procedure TVarIntSerializer.WriteVarUInt(Value: QWord);
var
  Length: Byte;
  Data: Byte;
  I: Integer;
begin
  // Get bit length
  Length := 31;
  while (((Value shr Length) and 1) = 0) and (Length > 0) do
    Dec(Length);
  Inc(Length);
  Length := Ceil(Length / (BitAlignment - 1));

  // Copy data
  for I := Length downto 1 do begin
    Data := (Value shr (8 * (I - 1))) and $ff;
    //ShowMessage(IntToStr(Length) + ' ' + IntToHex(Data, 2));
    if I = Length then Data := Data and
      ((1 shl (BitAlignment - Length)) - 1)
      or (((1 shl (BitAlignment - Length + 1)) - 1) xor $ff);
    WriteByte(Data);
    //ShowMessage(IntToStr(Length) + ' ' + IntToHex(Data, 2));
  end;
end;

function TVarIntSerializer.ReadVarUInt: QWord;
var
  Data: Byte;
  Length: Integer;
  I: Integer;
begin
  Result := 0;
  Length := 1;
  I := 0;
  while I < Length do begin
    Data := ReadByte;
    if I = 0 then begin
      Length := 1;
      while ((Data shr (BitAlignment - Length)) = 1) and (Length < 9) do
        Inc(Length);
      if Length > 8 then raise Exception.Create('VarInt 64-bit read overflow');
      Data := Data and ((1 shl (BitAlignment - Length)) - 1);
    end;
    Result := Result or (Data shl ((Length - I - 1) * 8));
    Inc(I);
  end;
end;

procedure TVarIntSerializer.WriteVarFloat(Value: Double);
var
  Exponent: Integer;
begin
  // Normalize to integer number with base 10 exponent
  Exponent := 0;
  while Frac(Value) > 0 do begin
    Value := Value * RealBase;
    Dec(Exponent);
  end;
  while Frac(Value / RealBase) = 0 do begin
    Value := Value / RealBase;
    Inc(Exponent);
  end;
  WriteVarSInt(Trunc(Value));
  WriteVarSInt(Exponent);
end;

function TVarIntSerializer.ReadVarFloat: Double;
var
  Significant: Int64;
  Exponent: Integer;
begin
  Significant := ReadVarSInt;
  Exponent := ReadVarSInt;
  Result := Significant * IntPower(RealBase, Exponent);
end;

procedure TVarIntSerializer.WriteVarString(Value: string);
var
  Stream: TVarIntSerializer;
  I: Integer;
begin
  Stream := TVarIntSerializer.Create;
  for I := 1 to Length(Value) do
    Stream.WriteVarUInt(Integer(Value[I]));
  WriteVarIntStream(Stream);
  Stream.Destroy;
end;

function TVarIntSerializer.ReadVarString: string;
var
  Stream: TVarIntSerializer;
  Character: Integer;
begin
  Stream := TVarIntSerializer.Create;
  ReadVarIntStream(Stream);
  Stream.Position := 0;
  while Stream.Position < Stream.Size do begin
    Character := Stream.ReadVarUInt;
    Result := Result + Char(Character);
  end;
  Stream.Destroy;
end;

procedure TVarIntSerializer.WriteVarIntStream(Stream: TMemoryStream);
var
  Length: Byte; // Count of data bytes
  Data: Byte;
  I: Cardinal;
begin
  // Get bit length
  Stream.Position := 0;
  if Stream.Size < 8 then begin
    // Unary length
    Length := Stream.Size * 8;
    Data := 0;
    while (Length > 0) and
    (((Data shr (Length and 7)) and 1) = 0) do begin
      Data := Stream.ReadByte;
      Dec(Length); // set 7. bit in byte
      while (((Data shr (Length and 7)) and 1) = 0) and ((Length and 7) > 0) do
        Dec(Length);
    end;
    Inc(Length);
    Length := Ceil(Length / (BitAlignment - 1));
  end else Length := Stream.Size + 1; // Recursive length

  // Copy data
  Stream.Position := 0;
  for I := Length downto 1 do begin
    if I <= Stream.Size then Data := Stream.ReadByte
      else Data := 0;
    if I = Length then begin
      if Length < 8 then begin
        Data := Data and
        ((1 shl (BitAlignment - Length)) - 1)
        or (((1 shl (BitAlignment - Length + 1)) - 1) xor $ff);
      end else begin
        // Recursive length
        WriteByte($ff);
        WriteVarUInt(Length - 8);
        Continue;
      end;
    end;
    WriteByte(Data);
  end;
end;

procedure TVarIntSerializer.ReadVarIntStream(Stream: TMemoryStream);
var
  Data: Byte;
  Length: Cardinal;
  I: Cardinal;
begin
  Stream.Clear;
  Length := 1;
  I := 0;
  while I < Length do begin
    Data := ReadByte;
    if I = 0 then begin
      Length := 1;
      while (((Data shr (BitAlignment - Length)) and 1) = 1) and (Length < 9) do
        Inc(Length);
      if Length > 8 then begin
        // Read recursive length
        Length := ReadVarUInt + 8;
        Inc(I);
        Continue;
      end else Data := Data and ((1 shl (BitAlignment - Length)) - 1);
      Stream.Size := Length;
    end;
    Stream.WriteByte(Data);
    Inc(I);
  end;
  Stream.Position := 0;
end;

procedure TVarIntSerializer.WriteVarSInt(Value: Int64);
begin
  if Value < 0 then WriteVarUInt((Abs(Value) shl 1) - 1)
    else WriteVarUInt((Abs(Value) shl 1))
end;

function TVarIntSerializer.ReadVarSInt: Int64;
begin
  Result := ReadVarUInt;
  if (Result and 1) = 0 then Result := Result shr 1
    else Result := -((Result + 1) shr 1);
end;

function TVarIntSerializer.TestMask(Mask, BitIndex: Integer): Boolean;
begin
  Result := ((Mask shr BitIndex) and 1) = 1;
end;

constructor TVarIntSerializer.Create;
begin
  inherited Create;
  Endianness := enBig;
end;

end.
