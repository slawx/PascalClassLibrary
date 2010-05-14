unit UMemoryStreamEx;

{$mode delphi}{$H+}

interface

uses
  Classes, DateUtils;

type
  TEndianness = (enBig, enLittle);
  
  { TMemoryStreamEx }

  TMemoryStreamEx = class(TMemoryStream)
  private
    FEndianness: TEndianness;
    SwapData: Boolean;
    procedure SetEndianness(const AValue: TEndianness);
  public
    procedure WriteByte(Data: Byte);
    procedure WriteWord(Data: Word);
    procedure WriteCardinal(Data: Cardinal);
    procedure WriteInt64(Data: Int64);
    procedure WriteShortString(Data: ShortString);
    procedure WriteAnsiString(Data: string);
    procedure WriteUnixTime(Data: TDateTime);
    procedure WriteStream(Stream: TStream; Count: Integer);
    procedure WriteDouble(Value: Double);
    procedure WriteSingle(Value: Single);
    function ReadByte: Byte;
    function ReadWord: Word;
    function ReadCardinal: Cardinal;
    function ReadInt64: Int64;
    function ReadShortString: string;
    function ReadAnsiString: string;
    function ReadUnixTime: TDateTime;
    function ReadDouble: Double;
    function ReadSingle: Single;
    procedure ReadStream(var Stream: TStream; Count: Integer);
    constructor Create;
    property Endianness: TEndianness read FEndianness write SetEndianness;
  end;

implementation

{ TMemoryStreamEx }

procedure TMemoryStreamEx.WriteAnsiString(Data: string);
var
  StringLength: Longint;
begin
  StringLength := Length(Data);
  Write(StringLength, SizeOf(StringLength));
  Write(Data[1], StringLength);
end;

function TMemoryStreamEx.ReadAnsiString: string;
var
  StringLength: Longint;
begin
  ReadBuffer(StringLength, SizeOf(StringLength));
  SetLength(Result, StringLength);
  if StringLength > 0 then begin
    ReadBuffer(Result[1], StringLength);
  end;
end;

function TMemoryStreamEx.ReadByte: Byte;
begin
  ReadBuffer(Result, SizeOf(Byte));
end;

function TMemoryStreamEx.ReadCardinal: Cardinal;
begin
  ReadBuffer(Result, SizeOf(Cardinal));
  if SwapData then Result := Swap(Result);
end;

function TMemoryStreamEx.ReadInt64: Int64;
begin
  ReadBuffer(Result, SizeOf(Int64));
  if SwapData then Result := Swap(Result);
end;

function TMemoryStreamEx.ReadShortString: string;
var
  Count: Byte;
begin
  ReadBuffer(Count, 1);
  SetLength(Result, Count);
  ReadBuffer(Result[1], Count);
end;

procedure TMemoryStreamEx.ReadStream(var Stream: TStream; Count: Integer);
var
  Buffer: array of Byte;
begin
  if Count > 0 then begin
    SetLength(Buffer, Count);
    ReadBuffer(Buffer[0], Count);
    Stream.Size := Count;
    Stream.Position := 0;
    Stream.Write(Buffer[0], Count);
  end;
end;

constructor TMemoryStreamEx.Create;
begin
  Endianness := enLittle;
end;

function TMemoryStreamEx.ReadUnixTime: TDateTime;
begin
  Result := UnixToDateTime(ReadCardinal);
end;

function TMemoryStreamEx.ReadDouble: Double;
begin
  ReadBuffer(Result, SizeOf(Double));
end;

function TMemoryStreamEx.ReadSingle: Single;
begin
  ReadBuffer(Result, SizeOf(Single));
end;

function TMemoryStreamEx.ReadWord: Word;
begin
  ReadBuffer(Result, SizeOf(Word));
  if SwapData then Result := Swap(Result);
end;

procedure TMemoryStreamEx.SetEndianness(const AValue: TEndianness);
begin
  FEndianness := AValue;
  {$ifdef FPC_LITTLE_ENDIAN}
  SwapData := FEndianness = enBig;
  {$elseifdef FPC_BIG_ENDIAN}
  SwapData := FEndianness = enLittle;
  {$endif}
end;

procedure TMemoryStreamEx.WriteByte(Data: Byte);
begin
  Write(Data, SizeOf(Byte));
end;

procedure TMemoryStreamEx.WriteCardinal(Data: Cardinal);
begin
  if SwapData then Data := Swap(Data);
  Write(Data, SizeOf(Cardinal));
end;

procedure TMemoryStreamEx.WriteInt64(Data: Int64);
begin
  if SwapData then Data := Swap(Data);
  Write(Data, SizeOf(Int64));
end;

procedure TMemoryStreamEx.WriteShortString(Data: ShortString);
begin
  WriteByte(Length(Data));
  Write(Data[1], Length(Data));
end;

procedure TMemoryStreamEx.WriteStream(Stream: TStream; Count: Integer);
var
  Buffer: array of Byte;
begin
  if Count > Stream.Size then Count := Stream.Size; // Limit max. stream size
  Stream.Position := 0;
  if Count > 0 then begin
    SetLength(Buffer, Count);
    Stream.Read(Buffer[0], Count);
    Write(Buffer[0], Count);
  end;
end;

procedure TMemoryStreamEx.WriteDouble(Value: Double);
begin
  Write(Value, SizeOf(Double));
end;

procedure TMemoryStreamEx.WriteSingle(Value: Single);
begin
  Write(Value, SizeOf(Single));
end;

procedure TMemoryStreamEx.WriteUnixTime(Data: TDateTime);
var
  DataUnix: Int64;
begin
  DataUnix := DateTimeToUnix(Data);
  WriteCardinal(DataUnix);
end;

procedure TMemoryStreamEx.WriteWord(Data: Word);
begin
  if SwapData then Data := Swap(Data);
  Write(Data, SizeOf(Word));
end;

end.
