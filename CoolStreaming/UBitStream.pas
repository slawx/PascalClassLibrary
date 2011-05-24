unit UBitStream;

// Date: 2010-08-17

{$mode delphi}

interface

uses
  Classes, SysUtils, RtlConsts, Math;

type
  TBytes = array[0..MaxInt - 1] of Byte;

  { TBitStream }

  TBitStream = class
  private
    function GetBit(Index: Integer):Boolean; virtual;
    function GetPosition: LongInt; virtual;
    function GetSize: LongInt; virtual;
    procedure SetBit(Index: Integer;const AValue: Boolean); virtual;
    procedure SetPosition(const AValue: LongInt); virtual;
    procedure SetSize(const AValue: LongInt); virtual;
  public
    function Seek(Offset: LongInt; Origin: TSeekOrigin): LongInt; virtual;
    function Read(var Buffer; Count: Longint): Longint; virtual;
    function CopyFrom(Source: TBitStream; Count: LongInt): LongInt;
    function Write(const Buffer; Count: Longint): Longint; virtual;
    function EqualTo(Source: TBitStream): Boolean;
    function GetString: string;
    procedure SetString(const AValue: string);
    procedure ReadBuffer(var Buffer; Count: Longint);
    procedure WriteBuffer(const Buffer; Count: Longint);
    function ReadBit: Boolean;
    procedure WriteBit(AValue: Boolean);
    function ReadNumber(Count: Byte): QWord;
    procedure WriteNumber(AValue: QWord; Count: Byte);
    property Position: LongInt read GetPosition write SetPosition;
    property Size: LongInt read GetSize write SetSize;
    property Bit[Index: Integer]: Boolean read GetBit write SetBit;
    property AsString: string read GetString write SetString;
  end;

  { TMemoryBitStream }

  TMemoryBitStream = class(TBitStream)
  private
    FStream: TMemoryStream;
    FPosition: LongInt;
    FSize: LongInt;
    function GetPosition: LongInt; override;
    function GetSize: LongInt; override;
    procedure SetPosition(const AValue: LongInt); override;
    procedure SetSize(const AValue: LongInt); override;
    function WriteToByte(var Data: Byte; NewData, Pos, Count: Byte): Byte;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: LongInt; Origin: TSeekOrigin): LongInt; override;
    constructor Create;
    destructor Destroy; override;
    property Stream: TMemoryStream read FStream;
  end;


implementation

{ TBitStream }

function TBitStream.GetBit(Index: Integer):Boolean;
begin
  Seek(Index, soBeginning);
  Read(Result, 1);
end;

function TBitStream.GetPosition:LongInt;
begin
  Result := Seek(0, soCurrent);
end;

function TBitStream.GetSize: LongInt;
var
  P: LongInt;
begin
  P := Seek(0, soCurrent);
  GetSize := Seek(0, soEnd);
  Seek(P, soBeginning);
end;

procedure TBitStream.SetBit(Index: Integer;const AValue: Boolean);
begin
  Seek(Index, soBeginning);
  Write(AValue, 1);
end;

procedure TBitStream.SetPosition(const AValue:LongInt);
begin
  Seek(AValue, soBeginning);
end;

procedure TBitStream.SetSize(const AValue:LongInt);
begin
end;

function TBitStream.Seek(Offset:LongInt;Origin:TSeekOrigin):LongInt;
begin
  Result := 0;
end;

function TBitStream.Read(var Buffer; Count:Longint):Longint;
begin
  Result := 0;
end;

function TBitStream.CopyFrom(Source: TBitStream; Count: LongInt): LongInt;
var
  BlockSize: LongInt;
  Buffer: array[0..1023] of Byte;
begin
  Result := 0;
  while Count > 0 do begin
    if Count > (SizeOf(Buffer) * 8) then BlockSize := SizeOf(Buffer) * 8
      else BlockSize := Count;
    BlockSize := Source.Read(Buffer, BlockSize);
    BlockSize := Write(Buffer, BlockSize);
    if BlockSize = 0 then Break;
    Dec(Count, BlockSize);
    Result := Result + BlockSize;
  end;
end;

function TBitStream.Write(const Buffer; Count:Longint):Longint;
begin
  Result := 0;
end;

function TBitStream.EqualTo(Source: TBitStream): Boolean;
var
  I: Integer;
begin
  if Size = Source.Size then begin
    I := 0;
    Result := True;
    Position := 0;
    Source.Position := 0;
    while (I < Size) and (ReadBit = Source.ReadBit) do Inc(I);
    if I < Size then Result := False;
  end else Result := False;
end;

procedure TBitStream.ReadBuffer(var Buffer; Count:Longint);
begin
  if Read(Buffer, Count) < Count then
    raise EReadError.Create(SReadError);
end;

procedure TBitStream.WriteBuffer(const Buffer; Count:Longint);
begin
  if Write(Buffer, Count) < Count then
    raise EWriteError.Create(SWriteError);
end;

function TBitStream.ReadBit:Boolean;
begin
  Read(Result, 1);
  Result := Boolean(Integer(Result) and 1);
end;

procedure TBitStream.WriteBit(AValue:Boolean);
begin
  Write(AValue, 1);
end;

function TBitStream.ReadNumber(Count: Byte): QWord;
begin
  Read(Result, Count);
  Result := Result and ((QWord(1) shl Count) - 1);
end;

procedure TBitStream.WriteNumber(AValue: QWord; Count: Byte);
begin
  Write(AValue, Count);
end;

function TBitStream.GetString: string;
var
  I: Integer;
begin
  Result := '';
  Position := 0;
  for I := 0 to Size - 1 do
    Result := Result + IntToStr(Integer(ReadBit));
end;

procedure TBitStream.SetString(const AValue: string);
var
  I: Integer;
begin
  Size := 0;
  for I := 1 to Length(AValue) do
    WriteBit(Boolean(StrToInt(AValue[I])));
  Position := 0;
end;

{ TMemoryBitStream }

function TMemoryBitStream.GetPosition:LongInt;
begin
  Result := FPosition;
end;

function TMemoryBitStream.GetSize:LongInt;
begin
  Result := FSize;
end;

procedure TMemoryBitStream.SetPosition(const AValue:LongInt);
begin
  Seek(AValue, soBeginning);
end;

procedure TMemoryBitStream.SetSize(const AValue: LongInt);
begin
  FSize := AValue;
  Stream.Size := Ceil(AValue / 8);
  if FPosition > FSize then FPosition := FSize;
end;

function TMemoryBitStream.WriteToByte(var Data: Byte; NewData,Pos,Count:Byte):Byte;
begin
  Data := Byte(Data and not (((1 shl Count) - 1) shl Pos) // Make zero space for new data
     or ((NewData and ((1 shl Count) - 1)) shl Pos));  // Write new data
  Result := Count;
  if Result > (8 - Pos) then Result := 8 - Pos;
end;

function TMemoryBitStream.Read(var Buffer;Count:Longint):Longint;
var
  ByteCount: LongInt;
  I: LongInt;
  PosInByte: Byte;
  Data: Byte;
begin
  if (Count < 0) or (Count > (Size - Position)) then
    raise EReadError.Create(SReadError);

  Result := 0;
  if (FSize > 0) and (FPosition < FSize) and (FPosition >= 0) then begin
    if (FPosition + Count) > FSize then Count := FSize - FPosition;
    ByteCount := Ceil(Count / 8);
    PosInByte := FPosition mod 8;
    Stream.Position := Trunc(FPosition / 8);
    Data := Stream.ReadByte; // Read first byte
    for I := 0 to ByteCount - 1 do begin
      TBytes(Buffer)[I] := (Data shr PosInByte) and ((1 shl (8 - PosInByte)) - 1);
      if (I < ByteCount) and (Stream.Position < Stream.Size) then
        Data := Stream.ReadByte else Data := 0;
      if PosInByte > 0 then
        TBytes(Buffer)[I] := TBytes(Buffer)[I] or
          ((Integer(Data) and ((1 shl PosInByte) - 1)) shl (8 - PosInByte));
      //if (I = (ByteCount - 1)) and (PosInByte > 0) then
      //  TBytes(Buffer)[I] := TBytes(Buffer)[I] and ((1 shl (Count mod 8)) - 1);
    end;
    Inc(FPosition, Count);
    Result := Count;
  end;
end;

function TMemoryBitStream.Write(const Buffer;Count:Longint):Longint;
var
  ByteCount: LongInt;
  BitCount: LongInt;
  I: LongInt;
  BytePos: Byte;
  Data: Byte;

function Min(Value1, Value2: Integer): Integer;
begin
  if Value1 < Value2 then Result := Value1
    else Result := Value2;
end;

begin
  if Count < 0 then
    raise EWriteError.Create(SWriteError);

  BitCount := Count;
  ByteCount := Ceil(Count / 8);
  BytePos := FPosition mod 8;
  Stream.Position := Trunc(FPosition / 8);
  if Stream.Position < Stream.Size then begin
    Data := Stream.ReadByte;
    Stream.Position := Stream.Position - 1;
  end else Data := 0;
  for I := 0 to ByteCount - 1 do begin
    Dec(BitCount, WriteToByte(Data, TBytes(Buffer)[I], BytePos, Min(8 - BytePos, BitCount)));
    Stream.WriteByte(Data);
    Data := 0;
    if (BitCount > 0) and (BytePos > 0) then begin
      if (I = (ByteCount - 1)) and (Stream.Position < Stream.Size) then begin
        Data := Stream.ReadByte;
        Stream.Position := Stream.Position - 1;
      end;
      Dec(BitCount, WriteToByte(Data, TBytes(Buffer)[I] shr (8 - BytePos), 0, Min(BytePos, BitCount)));
      if I = (ByteCount - 1) then
        Stream.WriteByte(Data);
    end;
  end;
  Inc(FPosition, Count);
  if FSize < FPosition then FSize := FPosition;
  Result := Count;
end;

function TMemoryBitStream.Seek(Offset:LongInt;Origin:TSeekOrigin):LongInt;
begin
  case Origin of
    soBeginning: FPosition := Offset;
    soEnd: FPosition := FSize + Offset;
    soCurrent: FPosition := FPosition + Offset;
  end;
  //if FPosition > FSize then FPosition := FSize;
  Result := FPosition;
end;

constructor TMemoryBitStream.Create;
begin
  FStream := TMemoryStream.Create;
  FPosition := 0;
  FSize := 0;
end;

destructor TMemoryBitStream.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

end.

