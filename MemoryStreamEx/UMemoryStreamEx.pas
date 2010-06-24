unit UMemoryStreamEx;

{$mode delphi}{$H+}

interface

uses
  Classes, DateUtils, syncobjs;

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
    procedure WriteString(Data: string);
    procedure WriteShortString(Data: ShortString);
    procedure WriteAnsiString(Data: string);
    procedure WriteUnixTime(Data: TDateTime);
    procedure WriteDouble(Value: Double);
    procedure WriteSingle(Value: Single);
    procedure WriteStream(Stream: TStream; Count: Integer);
    procedure WriteStreamPart(Stream: TStream; Count: Integer);
    function ReadByte: Byte;
    function ReadWord: Word;
    function ReadCardinal: Cardinal;
    function ReadInt64: Int64;
    function ReadString: string;
    function ReadShortString: string;
    function ReadAnsiString: string;
    function ReadStringTerminated(Terminator: string = #0): string;
    function ReadUnixTime: TDateTime;
    function ReadDouble: Double;
    function ReadSingle: Single;
    procedure ReadStream(var Stream: TStream; Count: Integer);
    procedure ReadStreamPart(var Stream: TStream; Count: Integer);
    function Sum: Byte;
    procedure FillByte(Data: Byte; Count: Integer);
    constructor Create;
    property Endianness: TEndianness read FEndianness write SetEndianness;
  end;

  { TThreadMemoryStreamEx }

  TThreadMemoryStreamEx = class(TMemoryStreamEx)
    Lock: TCriticalSection;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TMemoryStreamEx }

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

function TMemoryStreamEx.ReadStringTerminated(Terminator: string = #0): string;
var
  Data: Char;
  I: Integer;
  OldPosition: Integer;
begin
  OldPosition := Position;
  Result := '';
  I := 1;
  repeat
    if Position >= Size then Break;
    Data := Chr(ReadByte);
    if Data <> Terminator[I] then begin
      Result := Result + Data;
      I := 1;
    end else Inc(I);
  until I > Length(Terminator);
  if not (I > Length(Terminator)) then begin
    Result := '';
    Position := OldPosition;
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

function TMemoryStreamEx.ReadString:string;
begin
  SetLength(Result, Size - Position);
  if (Size - Position) > 0 then
    Read(Result[1], Size - Position)
    else Result := '';
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

procedure TMemoryStreamEx.ReadStreamPart(var Stream:TStream;Count:Integer);
var
  Buffer: array of Byte;
begin
  if Count > 0 then begin
    SetLength(Buffer, Count);
    ReadBuffer(Buffer[0], Count);
    if Stream.Size < (Stream.Position + Count) then
      Stream.Size := Stream.Position + Count;
    Stream.Write(Buffer[0], Count);
  end;
end;

procedure TMemoryStreamEx.WriteStreamPart(Stream:TStream;Count:Integer);
var
  Buffer: array of Byte;
begin
  if Count > Stream.Size then Count := Stream.Size; // Limit max. stream size
  if Count > 0 then begin
    SetLength(Buffer, Count);
    Stream.Read(Buffer[0], Count);
    Write(Buffer[0], Count);
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

function TMemoryStreamEx.Sum: Byte;
begin
  Result := 0;
  Position := 0;
  while Position < Size do
    Result := (Result + ReadByte) and $ff;
end;

procedure TMemoryStreamEx.FillByte(Data:Byte;Count:Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    WriteByte(Data);
end;

function TMemoryStreamEx.ReadWord: Word;
begin
  ReadBuffer(Result, SizeOf(Word));
  if SwapData then Result := Swap(Result);
end;

procedure TMemoryStreamEx.SetEndianness(const AValue: TEndianness);
begin
  FEndianness := AValue;
  {$if defined(FPC_LITTLE_ENDIAN)}
  SwapData := FEndianness = enBig;
  {$elseif defined(FPC_BIG_ENDIAN)}
  SwapData := FEndianness = enLittle;
  {$endif}
end;

procedure TMemoryStreamEx.WriteAnsiString(Data: string);
var
  StringLength: Longint;
begin
  StringLength := Length(Data);
  Write(StringLength, SizeOf(StringLength));
  Write(Data[1], StringLength);
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

procedure TMemoryStreamEx.WriteString(Data:string);
begin
  if Length(Data) > 0 then
    Write(Data[1], Length(Data));
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

{ TThreadMemoryStreamEx }

constructor TThreadMemoryStreamEx.Create;
begin
  inherited Create;
  Lock := TCriticalSection.Create;
end;

destructor TThreadMemoryStreamEx.Destroy;
begin
  Lock.Destroy;
  inherited Destroy;
end;

end.
