// 2010-03-30

unit UVarBlockSerializer;

{$mode Delphi}{$H+}

// One recursive VarInt size level supported
// Biggest UInt type is QWord (64-bit)

interface

uses
  Classes, DateUtils, UStreamHelper, Math, SysUtils, USubStream,
  Contnrs, SpecializedList;

const
  BitAlignment = 8;
  RealBase = 2;

type

  { TVarBlockSerializer }

  TVarBlockSerializer = class
  private
    FStream: TStream;
    procedure SetStream(const AValue: TStream);
    procedure TrimLeft;
    function GetUnaryLengthMask(Length: Integer): Byte;
    function DecodeUnaryLength(Data: Byte): Integer;
  public
    OwnsStream: Boolean;

    // Base
    procedure WriteVarUInt(Value: QWord);
    function ReadVarUInt: QWord;
    procedure WriteVarBlock(Block: TVarBlockSerializer);
    procedure ReadVarBlock(Block: TVarBlockSerializer);
    procedure WriteVarStream(AStream: TStream);
    procedure ReadVarStream(AStream: TStream);
    function GetVarSize: Integer;

    // Advanced data types
    procedure WriteVarSInt(Value: Int64);
    function ReadVarSInt: Int64;
    procedure WriteVarFloat(Value: Double);
    function ReadVarFloat: Double;
    procedure WriteVarString(Value: string);
    function ReadVarString: string;

    // Misc methods
    function TestMask(Mask: QWord; BitIndex: Byte): Boolean;
    function BuildMask(Bits: array of Integer): Integer;
    procedure ReadItemByMaskIndex(Index: Integer; Data: TVarBlockSerializer);
    procedure ReadItemRefByMaskIndex(Index: Integer; Data: TSubStream);
    procedure BlockEnclose;
    procedure BlockUnclose;
    constructor Create;
    destructor Destroy; override;
    property Stream: TStream read FStream write SetStream;
  end;

  { TVarBlockIndexed }

  TVarBlockIndexed = class
  private
  public
    Items: TObjectList; // TObjectList<TVarBlockSerializer>
    Enclose: Boolean;
    procedure CheckItem(Index: Integer);

    // Base
    procedure WriteVarUInt(Index: Integer; Value: QWord);
    function ReadVarUInt(Index: Integer): QWord;
    procedure WriteVarBlock(Index: Integer; Block: TVarBlockSerializer);
    procedure ReadVarBlock(Index: Integer; Block: TVarBlockSerializer);
    procedure WriteVarStream(Index: Integer; Stream: TStream);
    procedure ReadVarStream(Index: Integer; Stream: TStream);
    procedure WriteVarIndexedBlock(Index: Integer; Block: TVarBlockIndexed);
    procedure ReadVarIndexedBlock(Index: Integer; Block: TVarBlockIndexed);

    // Advanced data types
    procedure WriteVarSInt(Index: Integer; Value: Int64);
    function ReadVarSInt(Index: Integer): Int64;
    procedure WriteVarFloat(Index: Integer; Value: Double);
    function ReadVarFloat(Index: Integer): Double;
    procedure WriteVarString(Index: Integer; Value: string);
    function ReadVarString(Index: Integer): string;
    procedure WriteVarIntegerArray(Index: Integer; List: TListInteger);
    procedure ReadVarIntegerArray(Index: Integer; List: TListInteger);

    procedure Clear;
    function TestIndex(Index: Integer): Boolean;
    procedure WriteToVarBlock(Stream: TVarBlockSerializer);
    procedure ReadFromVarBlock(Stream: TVarBlockSerializer);
    procedure WriteToStream(Stream: TStream);
    procedure ReadFromStream(Stream: TStream);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

resourcestring
  SMaskedValueReadError = 'Error reading masked variable length block.';
  SUInt64Overflow = '64-bit UInt read overflow.';

{ TVarBlockSerializer }

procedure TVarBlockSerializer.TrimLeft;
var
  Temp: TVarBlockSerializer;
  Length: Integer;
  Data: Byte;
  StreamHelper: TStreamHelper;
begin
  try
    Temp := TVarBlockSerializer.Create;
    Stream.Position := 0;
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
    Length := Ceil(Length / 8);
    Stream.Position := Stream.Size - Length;
    StreamHelper := TStreamHelper.Create(Stream);
    StreamHelper.ReadStream(Temp.Stream, Length);
    Temp.Stream.Size := 0;
    Stream.Position := 0;
    StreamHelper.WriteStream(Temp.Stream, Temp.Stream.Size);
  finally
    StreamHelper.Free;
    Temp.Free;
  end;
end;

procedure TVarBlockSerializer.SetStream(const AValue: TStream);
begin
  if OwnsStream and Assigned(FStream) then
    FStream.Free;
  OwnsStream := False;
  FStream := AValue;
end;

function TVarBlockSerializer.GetUnaryLengthMask(Length: Integer): Byte;
begin
  Result := ((1 shl (BitAlignment - Length)) - 1) xor $ff;
end;

function TVarBlockSerializer.DecodeUnaryLength(Data:Byte):Integer;
begin
  Result := 1;
  while (((Data shr (BitAlignment - Result)) and 1) = 1) and
    (Result < (BitAlignment + 1)) do Inc(Result);
end;

procedure TVarBlockSerializer.WriteVarUInt(Value: QWord);
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
    Stream.WriteByte(Data);
  end;
end;

function TVarBlockSerializer.ReadVarUInt: QWord;
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
    Data := Stream.ReadByte;
    if I = 0 then begin
      Length := DecodeUnaryLength(Data);
      if Length > (BitAlignment) then
        raise Exception.Create(SUInt64Overflow);
      LengthMask := GetUnaryLengthMask(Length);
      Data := Data and (LengthMask xor $ff);
    end;
    Result := Result or (QWord(Data) shl ((Length - I - 1) * BitAlignment));
    Inc(I);
  end;
end;

procedure TVarBlockSerializer.WriteVarBlock(Block: TVarBlockSerializer);
begin
  WriteVarStream(Block.Stream);
end;

procedure TVarBlockSerializer.ReadVarBlock(Block: TVarBlockSerializer);
begin
  ReadVarStream(Block.Stream);
end;

procedure TVarBlockSerializer.WriteVarFloat(Value: Double);
var
  Exponent: Integer;
  Block: TVarBlockSerializer;
begin
  try
    Block := TVarBlockSerializer.Create;

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
    Block.WriteVarSInt(Trunc(Value));
    Block.WriteVarSInt(Exponent);
    WriteVarBlock(Block);
  finally
    Block.Free;
  end;
end;

function TVarBlockSerializer.ReadVarFloat: Double;
var
  Significant: Int64;
  Exponent: Integer;
  Block: TVarBlockSerializer;
begin
  try
    Block := TVarBlockSerializer.Create;
    ReadVarBlock(Block);
    Significant := Block.ReadVarSInt;
    Exponent := Block.ReadVarSInt;
    Result := Significant * IntPower(RealBase, Exponent);
  finally
    Block.Free;
  end;
end;

procedure TVarBlockSerializer.WriteVarString(Value: string);
var
  Stream: TVarBlockSerializer;
  I: Integer;
begin
  try
    Stream := TVarBlockSerializer.Create;
    for I := 1 to Length(Value) do
      Stream.WriteVarUInt(Integer(Value[I]));
    WriteVarBlock(Stream);
  finally
    Stream.Free;
  end;
end;

function TVarBlockSerializer.ReadVarString: string;
var
  Block: TVarBlockSerializer;
  Character: Integer;
begin
  try
    Block := TVarBlockSerializer.Create;
    ReadVarBlock(Block);
    Block.Stream.Position := 0;
    while Block.Stream.Position < Block.Stream.Size do begin
      Character := Block.ReadVarUInt;
      Result := Result + Char(Character);
    end;
  finally
    Block.Free;
  end;
end;

procedure TVarBlockSerializer.WriteVarStream(AStream: TStream);
var
  Length: Integer; // Count of data bytes
  Data: Byte;
  I: Integer;
  LengthMask: Byte;
begin
  AStream.Position := 0;
  Length := AStream.Size;

  // Copy data
  if Length = 0 then Stream.WriteByte(0)
  else begin
    if AStream.Size > 0 then Data := AStream.ReadByte
      else Data := 0;
    if (Length < BitAlignment) then begin
      LengthMask := GetUnaryLengthMask(Length);
      if ((Data and (LengthMask xor $ff)) <> Data) or (Data = 0) then begin
        // First data starts by zero or
        // first data byte not fit to length byte
        Inc(Length);
        if Length < 8 then begin
          LengthMask := GetUnaryLengthMask(Length);
          Stream.WriteByte((LengthMask shl 1) and $ff);
          Stream.WriteByte(Data);
        end;
      end else begin
        // First data byte fit to length byte
        Stream.WriteByte((Data and (LengthMask xor $ff)) or ((LengthMask shl 1) and $ff));
      end;
    end;
    if Length >= BitAlignment then begin
      // Recursive length
      Stream.WriteByte($ff);
      WriteVarUInt(AStream.Size);
      Stream.WriteByte(Data);
    end;

    // Copy rest of data
    for I := 1 to AStream.Size - 1 do begin
      if I < AStream.Size then Data := AStream.ReadByte
        else Data := 0;
      Stream.WriteByte(Data);
    end;
  end;
end;

procedure TVarBlockSerializer.ReadVarStream(AStream: TStream);
var
  Data: Byte;
  Length: Cardinal;
  I: Cardinal;
  LengthMask: Byte;
begin
  AStream.Size := 0;
  I := 0;
  Length := 1;
  while I < Length do begin
    Data := Stream.ReadByte;
    if I = 0 then begin
      if Data = $ff then begin
        // Read recursive length
        Length := ReadVarUInt;
        AStream.Size := Length;
        Data := Stream.ReadByte;
        AStream.WriteByte(Data);
      end else begin
        // Read unary length
        Length := DecodeUnaryLength(Data);
        AStream.Size := Length;
        LengthMask := GetUnaryLengthMask(Length);
        Data := Data and (LengthMask xor $ff);
        // Drop first byte if first data zero
        if Data <> 0 then AStream.WriteByte(Data)
          else begin
            Dec(Length);
            AStream.Size := Length;
            if Length > 0 then begin
              Data := Stream.ReadByte;
              AStream.WriteByte(Data);
            end;
          end;
      end;
    end else AStream.WriteByte(Data);
    Inc(I);
  end;
  AStream.Position := 0;
end;

function TVarBlockSerializer.GetVarSize: Integer;
var
  Data: Byte;
  StoredPosition: Integer;
begin
  StoredPosition := Stream.Position;
  Result := 1; // Byte block length
  Data := Stream.ReadByte;
  if Data = $ff then Result := ReadVarUInt + 2
  else begin
    Result := DecodeUnaryLength(Data);
  end;
  Stream.Position := StoredPosition;
end;

procedure TVarBlockSerializer.WriteVarSInt(Value: Int64);
begin
  if Value < 0 then WriteVarUInt(((-Value) shl 1) - 1)
    else WriteVarUInt((Value shl 1))
end;

function TVarBlockSerializer.ReadVarSInt: Int64;
begin
  Result := ReadVarUInt;
  if (Result and 1) = 0 then Result := Result shr 1
    else Result := -((Result + 1) shr 1);
end;

function TVarBlockSerializer.TestMask(Mask: QWord; BitIndex: Byte): Boolean;
begin
  Result := ((Mask shr BitIndex) and 1) = 1;
end;

function TVarBlockSerializer.BuildMask(Bits:array of Integer):Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(Bits) do
    Result := Result or (1 shl Bits[I]);
end;

procedure TVarBlockSerializer.ReadItemByMaskIndex(Index:Integer; Data:
  TVarBlockSerializer);
var
  Mask: Integer;
  I: Integer;
  StreamHelper: TStreamHelper;
begin
  try
    StreamHelper := TStreamHelper.Create(Stream);
    try
      Stream.Position := 0;
      Data.Stream.Size := 0;
      Mask := ReadVarUInt;
      I := 0;
      while (Stream.Position < Stream.Size) and (I < Index) do begin
        if TestMask(Mask, I) then Stream.Position := Stream.Position + GetVarSize;
        Inc(I);
      end;
      if TestMask(Mask, Index) then
        StreamHelper.ReadStream(Data.Stream, GetVarSize);
    except
      raise Exception.Create(SMaskedValueReadError);
    end;
  finally
    StreamHelper.Free;
    Data.Stream.Position := 0;
  end;
end;

procedure TVarBlockSerializer.ReadItemRefByMaskIndex(Index:Integer;Data:TSubStream
  );
var
  Mask: Integer;
  I: Integer;
begin
  try
    Stream.Position := 0;
    Data.Size := 0;
    Mask := ReadVarUInt;
    I := 0;
    while (Stream.Position < Stream.Size) and (I < Index) do begin
      if TestMask(Mask, I) then Stream.Position := Stream.Position + GetVarSize;
      Inc(I);
    end;
    if TestMask(Mask, Index) then begin
      if Stream is TSubStream then begin
        // Recalculate substream
        Data.Source := TSubStream(Stream).Source;
        Data.SourcePosition := TSubStream(Stream).SourcePosition + Stream.Position;
      end else begin
        Data.Source := Self.Stream;
        Data.SourcePosition := Stream.Position;
      end;
      Data.Size := GetVarSize;
    end;
    Data.Position := 0;
  except
    raise Exception.Create(SMaskedValueReadError);
  end;
end;

procedure TVarBlockSerializer.BlockEnclose;
var
  Temp: TVarBlockSerializer;
  StreamHelper: TStreamHelper;
begin
  try
    Temp := TVarBlockSerializer.Create;
    StreamHelper := TStreamHelper.Create(Temp.Stream);
    StreamHelper.WriteStream(Stream, Stream.Size);
    Stream.Size := 0;
    WriteVarBlock(Temp);
  finally
    StreamHelper.Free;
    Temp.Free;
  end;
end;

procedure TVarBlockSerializer.BlockUnclose;
var
  Temp: TVarBlockSerializer;
  StreamHelper: TStreamHelper;
begin
  try
    Temp := TVarBlockSerializer.Create;
    StreamHelper := TStreamHelper.Create(Stream);
    ReadVarBlock(Temp);
    Stream.Size := 0;
    StreamHelper.WriteStream(Temp.Stream, Temp.Stream.Size);
  finally
    Stream.Position := 0;
    StreamHelper.Free;
    Temp.Free;
  end;
end;

constructor TVarBlockSerializer.Create;
begin
  inherited Create;
  Stream := TStreamHelper.Create;
  OwnsStream := True;
  TStreamHelper(Stream).Endianness := enBig;
end;

destructor TVarBlockSerializer.Destroy;
begin
  if OwnsStream then begin
    Stream.Free;
  end;
  inherited Destroy;
end;

{ TVarBlockIndexed }

procedure TVarBlockIndexed.CheckItem(Index:Integer);
begin
  if Items.Count > Index then begin
    if not Assigned(Items[Index]) then
      Items[Index] := TVarBlockSerializer.Create;
    TVarBlockSerializer(Items[Index]).Stream.Size := 0;
  end else begin
    Items.Count := Index + 1;
    Items[Index] := TVarBlockSerializer.Create;
  end;
end;

procedure TVarBlockIndexed.WriteVarUInt(Index:Integer;Value:QWord);
begin
  CheckItem(Index);
  TVarBlockSerializer(Items[Index]).WriteVarUInt(Value);
end;

function TVarBlockIndexed.ReadVarUInt(Index:Integer):QWord;
begin
  TVarBlockSerializer(Items[Index]).Stream.Position := 0;
  Result := TVarBlockSerializer(Items[Index]).ReadVarUInt;
end;

procedure TVarBlockIndexed.WriteVarBlock(Index: Integer; Block: TVarBlockSerializer);
begin
  CheckItem(Index);
  TVarBlockSerializer(Items[Index]).WriteVarBlock(Block);
end;

procedure TVarBlockIndexed.ReadVarBlock(Index: Integer; Block: TVarBlockSerializer);
begin
  TVarBlockSerializer(Items[Index]).Stream.Position := 0;
  TVarBlockSerializer(Items[Index]).ReadVarBlock(Block);
end;

procedure TVarBlockIndexed.WriteVarStream(Index: Integer; Stream: TStream);
begin
  CheckItem(Index);
  TVarBlockSerializer(Items[Index]).WriteVarStream(Stream);
end;

procedure TVarBlockIndexed.ReadVarStream(Index: Integer; Stream: TStream);
begin
  TVarBlockSerializer(Items[Index]).Stream.Position := 0;
  TVarBlockSerializer(Items[Index]).ReadVarStream(Stream);
end;

procedure TVarBlockIndexed.WriteVarIndexedBlock(Index: Integer;
  Block: TVarBlockIndexed);
var
  Temp: TStreamHelper;
begin
  try
    Temp := TStreamHelper.Create;
    Block.Enclose := False;
    Block.WriteToStream(Temp);
    WriteVarStream(Index, Temp);
  finally
    Temp.Free;
  end;
end;

procedure TVarBlockIndexed.ReadVarIndexedBlock(Index: Integer;
  Block: TVarBlockIndexed);
var
  Temp: TStreamHelper;
begin
  try
    Temp := TStreamHelper.Create;
    Block.Enclose := False;
    ReadVarStream(Index, Temp);
    Block.ReadFromStream(Temp);
  finally
    Temp.Free;
  end;
end;

procedure TVarBlockIndexed.WriteVarSInt(Index: Integer; Value:Int64);
begin
  CheckItem(Index);
  TVarBlockSerializer(Items[Index]).WriteVarSInt(Value);
end;

function TVarBlockIndexed.ReadVarSInt(Index: Integer): Int64;
begin
  TVarBlockSerializer(Items[Index]).Stream.Position := 0;
  Result := TVarBlockSerializer(Items[Index]).ReadVarSInt;
end;

procedure TVarBlockIndexed.WriteVarFloat(Index: Integer; Value:Double);
begin
  CheckItem(Index);
  TVarBlockSerializer(Items[Index]).WriteVarFloat(Value);
end;

function TVarBlockIndexed.ReadVarFloat(Index: Integer):Double;
begin
  TVarBlockSerializer(Items[Index]).Stream.Position := 0;
  Result := TVarBlockSerializer(Items[Index]).ReadVarFloat;
end;

procedure TVarBlockIndexed.WriteVarString(Index: Integer; Value:string);
begin
  CheckItem(Index);
  TVarBlockSerializer(Items[Index]).WriteVarString(Value);
end;

function TVarBlockIndexed.ReadVarString(Index: Integer):string;
begin
  TVarBlockSerializer(Items[Index]).Stream.Position := 0;
  Result := TVarBlockSerializer(Items[Index]).ReadVarString;
end;

procedure TVarBlockIndexed.WriteVarIntegerArray(Index: Integer;
  List: TListInteger);
var
  I: Integer;
  Temp: TVarBlockSerializer;
begin
  try
    Temp := TVarBlockSerializer.Create;
    for I := 0 to List.Count - 1 do
      Temp.WriteVarUInt(Integer(List[I]));
    WriteVarBlock(Index, Temp);
  finally
    Temp.Free;
  end;
end;

procedure TVarBlockIndexed.ReadVarIntegerArray(Index: Integer;
  List: TListInteger);
var
  Temp: TVarBlockSerializer;
begin
  try
    Temp := TVarBlockSerializer.Create;
    List.Clear;
    ReadVarBlock(Index, Temp);
    while Temp.Stream.Position < Temp.Stream.Size do begin
      List.Add(Temp.ReadVarUInt);
    end;
  finally
    Temp.Free;
  end;
end;

procedure TVarBlockIndexed.Clear;
begin
  Items.Clear;
end;

function TVarBlockIndexed.TestIndex(Index:Integer):Boolean;
begin
  if (Index >= 0) and (Index < Items.Count) then
    Result := Assigned(Items[Index])
    else Result := False
end;

procedure TVarBlockIndexed.WriteToVarBlock(Stream: TVarBlockSerializer);
var
  Mask: Integer;
  I: Integer;
  StreamHelper: TStreamHelper;
begin
  try
    StreamHelper := TStreamHelper.Create(Stream.Stream);
    Stream.Stream.Size := 0;
    Mask := 0;
    for I := 0 to Items.Count - 1 do
      if Assigned(Items[I]) then Mask := Mask or (1 shl I);
    Stream.WriteVarUInt(Mask);
    for I := 0 to Items.Count - 1 do
      if Assigned(Items[I]) then StreamHelper.WriteStream(TVarBlockSerializer(Items[I]).Stream,
        TVarBlockSerializer(Items[I]).Stream.Size);
    if Enclose then Stream.BlockEnclose;
  finally
    StreamHelper.Free;
  end;
end;

procedure TVarBlockIndexed.ReadFromVarBlock(Stream: TVarBlockSerializer);
var
  Mask: Integer;
  I: Integer;
begin
  if Enclose then Stream.BlockUnclose;
  Stream.Stream.Position := 0;
  Mask := Stream.ReadVarUInt;
  I := 0;
  while Mask <> 0 do begin
    if Stream.TestMask(Mask, I) then begin
      if Items.Count <= I then Items.Count := I + 1;
      Items[I] := TVarBlockSerializer.Create;
      Stream.ReadItemByMaskIndex(I, TVarBlockSerializer(Items[I]));
      Mask := Mask xor (1 shl I); // Clear bit on current index
    end;
    Inc(I);
  end;
end;

procedure TVarBlockIndexed.WriteToStream(Stream: TStream);
var
  Temp: TVarBlockSerializer;
  StreamHelper: TStreamHelper;
begin
  try
    Temp := TVarBlockSerializer.Create;
    StreamHelper := TStreamHelper.Create(Stream);
    WriteToVarBlock(Temp);
    StreamHelper.WriteStream(Temp.Stream, Temp.Stream.Size);
  finally
    StreamHelper.Free;
    Temp.Free;
  end;
end;

procedure TVarBlockIndexed.ReadFromStream(Stream: TStream);
var
  VarBlock: TVarBlockSerializer;
begin
  try
    VarBlock := TVarBlockSerializer.Create;
    VarBlock.Stream := Stream;
    ReadFromVarBlock(VarBlock);
  finally
    VarBlock.Free;
  end;
end;

constructor TVarBlockIndexed.Create;
begin
  Items := TObjectList.Create;
  Enclose := True;
end;

destructor TVarBlockIndexed.Destroy;
begin
  Items.Free;
  inherited Destroy;
end;

end.
