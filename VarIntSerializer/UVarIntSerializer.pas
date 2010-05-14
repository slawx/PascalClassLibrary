// 2010-03-30

unit UVarIntSerializer;

{$mode Delphi}{$H+}

// One recursive VarInt size level supported
// Biggest UInt type is QWord (64-bit)

interface

uses
  Classes, DateUtils, UMemoryStreamEx, Math, Dialogs, SysUtils;

const
  BitAlignment = 8;
  RealBase = 2;

type
  { TVarIntSerializer }

  TVarIntSerializer = class(TMemoryStreamEx)
  private
    procedure TrimLeft;
    function GetUnaryLengthMask(Length: Integer): Byte;
    function DecodeUnaryLength(Data: Byte): Integer;
  public
    // Base
    procedure WriteVarUInt(Value: QWord);
    function ReadVarUInt: QWord;
    procedure WriteVarBlock(Stream: TStream);
    procedure ReadVarBlock(Stream: TStream);
    function GetVarSize: Integer;

    // Advanced data types
    procedure WriteVarSInt(Value: Int64);
    function ReadVarSInt: Int64;
    procedure WriteVarFloat(Value: Double);
    function ReadVarFloat: Double;
    procedure WriteVarString(Value: string);
    function ReadVarString: string;

    // Misc methods
    function TestMask(Mask, BitIndex: Integer): Boolean;
    procedure ReadItemByMaskIndex(Index: Integer; Data: TVarIntSerializer);
    procedure BlockEnclose;
    procedure BlockUnclose;
    constructor Create;
  end;

implementation

{ TVarIntSerializer }

procedure TVarIntSerializer.TrimLeft;
var
  Temp: TVarIntSerializer;
  Length: Integer;
  Data: Byte;
begin
  Temp := TVarIntSerializer.Create;
  Position := 0;
  Length := Size * 8;
  Data := 0;
  while (Length > 0) and
  (((Data shr (Length and 7)) and 1) = 0) do begin
    Data := ReadByte;
    Dec(Length); // set 7. bit in byte
    while (((Data shr (Length and 7)) and 1) = 0) and ((Length and 7) > 0) do
      Dec(Length);
  end;
  Inc(Length);
  Length := Ceil(Length / 8);
  Position := Size - Length;
  ReadStream(TStream(Temp), Length);
  Clear;
  Position := 0;
  WriteStream(Temp, Temp.Size);
end;

function TVarIntSerializer.GetUnaryLengthMask(Length: Integer): Byte;
begin
  Result := ((1 shl (BitAlignment - Length)) - 1) xor $ff;
end;

function TVarIntSerializer.DecodeUnaryLength(Data:Byte):Integer;
begin
  Result := 1;
  while (((Data shr (BitAlignment - Result)) and 1) = 1) and
    (Result < (BitAlignment + 1)) do Inc(Result);
end;

procedure TVarIntSerializer.WriteVarUInt(Value: QWord);
var
  Length: Byte;
  Data: Byte;
  I: Integer;
  LengthMask: Byte;
begin
  // Get bit length
  Length := SizeOf(QWord) * BitAlignment;
  while (((Value shr Length) and 1) = 0) and (Length > 0) do
    Dec(Length);
  Inc(Length);
  Length := Ceil(Length / (BitAlignment - 1));
  LengthMask := GetUnaryLengthMask(Length);

  // Copy data
  for I := Length downto 1 do begin
    Data := (Value shr (BitAlignment * (I - 1))) and $ff;
    if I = Length then Data := (Data and
      (LengthMask xor $ff)) or ((LengthMask shl 1) and $ff);
    WriteByte(Data);
  end;
end;

function TVarIntSerializer.ReadVarUInt: QWord;
var
  Data: Byte;
  Length: Integer;
  I: Integer;
  LengthMask: Byte;
begin
  Result := 0;
  Length := 1;
  I := 0;
  while I < Length do begin
    Data := ReadByte;
    if I = 0 then begin
      Length := DecodeUnaryLength(Data);
      if Length > (BitAlignment - 1) then raise Exception.Create('64-bit UInt read overflow');
      LengthMask := GetUnaryLengthMask(Length);
      Data := Data and (LengthMask xor $ff);
    end;
    Result := Result or (Data shl ((Length - I - 1) * BitAlignment));
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
  WriteVarBlock(Stream);
  Stream.Destroy;
end;

function TVarIntSerializer.ReadVarString: string;
var
  Stream: TVarIntSerializer;
  Character: Integer;
begin
  Stream := TVarIntSerializer.Create;
  ReadVarBlock(Stream);
  Stream.Position := 0;
  while Stream.Position < Stream.Size do begin
    Character := Stream.ReadVarUInt;
    Result := Result + Char(Character);
  end;
  Stream.Destroy;
end;

procedure TVarIntSerializer.WriteVarBlock(Stream: TStream);
var
  Length: Integer; // Count of data bytes
  Data: Byte;
  I: Integer;
  LengthMask: Byte;
begin
  Stream.Position := 0;
  Length := Stream.Size;

  // Copy data
  if Length = 0 then WriteByte(0)
  else begin
    if Stream.Size > 0 then Data := Stream.ReadByte
      else Data := 0;
    if (Length < BitAlignment) then begin
      LengthMask := GetUnaryLengthMask(Length);
      if ((Data and (LengthMask xor $ff)) <> Data) or (Data = 0) then begin
        // First data starts by zero or
        // first data byte not fit to length byte
        Inc(Length);
        if Length < 8 then begin
          LengthMask := GetUnaryLengthMask(Length);
          WriteByte((LengthMask shl 1) and $ff);
          WriteByte(Data);
        end;
      end else begin
        // First data byte fit to length byte
        WriteByte((Data and (LengthMask xor $ff)) or ((LengthMask shl 1) and $ff));
      end;
    end;
    if Length >= BitAlignment then begin
      // Recursive length
      WriteByte($ff);
      WriteVarUInt(Stream.Size);
      WriteByte(Data);
    end;

    // Copy rest of data
    for I := 1 to Stream.Size - 1 do begin
      if I < Stream.Size then Data := Stream.ReadByte
        else Data := 0;
      WriteByte(Data);
    end;
  end;
end;

procedure TVarIntSerializer.ReadVarBlock(Stream: TStream);
var
  Data: Byte;
  Length: Cardinal;
  I: Cardinal;
  LengthMask: Byte;
begin
  Stream.Size := 0;
  I := 0;
  Length := 1;
  while I < Length do begin
    Data := ReadByte;
    if I = 0 then begin
      if Data = $ff then begin
        // Read recursive length
        Length := ReadVarUInt;
        Stream.Size := Length;
        Data := ReadByte;
        Stream.WriteByte(Data);
      end else begin
        // Read unary length
        Length := DecodeUnaryLength(Data);
        Stream.Size := Length;
        LengthMask := GetUnaryLengthMask(Length);
        Data := Data and (LengthMask xor $ff);
        // Drop first byte if first data zero
        if Data <> 0 then Stream.WriteByte(Data)
          else begin
            Dec(Length);
            Stream.Size := Length;
            if Length > 0 then begin
              Data := ReadByte;
              Stream.WriteByte(Data);
            end;
          end;
      end;
    end else Stream.WriteByte(Data);
    Inc(I);
  end;
  Stream.Position := 0;
end;

function TVarIntSerializer.GetVarSize: Integer;
var
  Data: Byte;
  I: Cardinal;
  StoredPosition: Integer;
begin
  StoredPosition := Position;
  Result := 1; // Byte block length
  Data := ReadByte;
  if Data = $ff then Result := ReadVarUInt + 2
  else begin
    Result := DecodeUnaryLength(Data);
  end;
  Position := StoredPosition;
end;

procedure TVarIntSerializer.WriteVarSInt(Value: Int64);
begin
  if Value < 0 then WriteVarUInt(((-Value) shl 1) - 1)
    else WriteVarUInt((Value shl 1))
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

procedure TVarIntSerializer.ReadItemByMaskIndex(Index:Integer;Data:
  TVarIntSerializer);
var
  Mask: Integer;
  I: Integer;
begin
  Position := 0;
  Mask := ReadVarUInt;
  I := 0;
  while (Position < Size) and (I < Index) do begin
    if TestMask(Mask, I) then Position := Position + GetVarSize;
    Inc(I);
  end;
  if TestMask(Mask, Index) then
    ReadStream(TStream(Data), GetVarSize);
end;

procedure TVarIntSerializer.BlockEnclose;
var
  Temp: TVarIntSerializer;
begin
  Temp := TVarIntSerializer.Create;
  Temp.WriteStream(Self, Size);
  Clear;
  WriteVarBlock(Temp);
  Temp.Destroy;
end;

procedure TVarIntSerializer.BlockUnclose;
var
  Temp: TVarIntSerializer;
begin
  Temp := TVarIntSerializer.Create;
  ReadVarBlock(Temp);
  Clear;
  WriteStream(Temp, Temp.Size);
  Temp.Destroy;
  Position := 0;
end;

constructor TVarIntSerializer.Create;
begin
  inherited Create;
  Endianness := enBig;
end;

end.
