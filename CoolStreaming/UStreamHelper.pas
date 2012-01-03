unit UStreamHelper;

{$mode delphi}{$H+}

interface

uses
  Classes, DateUtils, syncobjs;

type
  TEndianness = (enBig, enLittle);
  
  { TStreamHelper }

  TStreamHelper = class(TStream)
  private
    FOwnStream: Boolean;
    FStream: TStream;
    FEndianness: TEndianness;
    SwapData: Boolean;
    function GetItem(Index: Integer): Byte;
    procedure SetEndianness(const AValue: TEndianness);
    procedure SetItem(Index: Integer; const AValue: Byte);
  public
    procedure Assign(Source: TStreamHelper);
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
    procedure WriteStream(AStream: TStream; Count: Integer);
    procedure WriteStreamPart(AStream: TStream; Count: Integer);
    function ReadByte: Byte;
    function ReadWord: Word;
    function ReadCardinal: Cardinal;
    function ReadInt64: Int64;
    function ReadString(Length: Integer): string;
    function ReadShortString: string;
    function ReadAnsiString: string;
    function ReadStringTerminated(Terminator: string = #0): string;
    function ReadUnixTime: TDateTime;
    function ReadDouble: Double;
    function ReadSingle: Single;
    procedure ReadStream(AStream: TStream; Count: Integer);
    procedure ReadStreamPart(AStream: TStream; Count: Integer);
    function Sum: Byte;
    procedure FillByte(Data: Byte; Count: Integer);
    constructor Create; overload;
    constructor Create(AStream: TStream); overload;
    procedure Clear;
    destructor Destroy; override;
    function GetSize: Int64; override;
    procedure SetSize(NewSize: Longint); override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    property Endianness: TEndianness read FEndianness write SetEndianness;
    property Stream: TStream read FStream write FStream;
    property Items[Index: Integer]: Byte read GetItem write SetItem; default;
  end;

implementation

{ TStreamHelper }

function TStreamHelper.ReadAnsiString: string;
var
  StringLength: Longint;
begin
  FStream.ReadBuffer(StringLength, SizeOf(StringLength));
  Result := ReadString(StringLength);
end;

function TStreamHelper.ReadStringTerminated(Terminator: string = #0): string;
var
  Data: Char;
  I: Integer;
  OldPosition: Integer;
begin
  OldPosition := FStream.Position;
  Result := '';
  I := 1;
  repeat
    if FStream.Position >= FStream.Size then Break;
    Data := Chr(ReadByte);
    if Data <> Terminator[I] then begin
      Result := Result + Data;
      I := 1;
    end else Inc(I);
  until I > Length(Terminator);
  if not (I > Length(Terminator)) then begin
    Result := '';
    FStream.Position := OldPosition;
  end;
end;

function TStreamHelper.ReadByte: Byte;
begin
  FStream.ReadBuffer(Result, SizeOf(Byte));
end;

function TStreamHelper.ReadCardinal: Cardinal;
begin
  FStream.ReadBuffer(Result, SizeOf(Cardinal));
  if SwapData then Result := Swap(Result);
end;

function TStreamHelper.ReadInt64: Int64;
begin
  FStream.ReadBuffer(Result, SizeOf(Int64));
  if SwapData then Result := Swap(Result);
end;

function TStreamHelper.ReadString(Length: Integer): string;
begin
  if Length > 0 then begin
    SetLength(Result, Length);
    FStream.Read(Result[1], Length);
  end else Result := '';
end;

function TStreamHelper.ReadShortString: string;
var
  Count: Byte;
begin
  FStream.ReadBuffer(Count, 1);
  Result := ReadString(Count);
end;

procedure TStreamHelper.ReadStream(AStream: TStream; Count: Integer);
var
  Buffer: array of Byte;
begin
  if Count > 0 then begin
    SetLength(Buffer, Count);
    FStream.ReadBuffer(Buffer[0], Count);
    AStream.Size := Count;
    AStream.Position := 0;
    AStream.Write(Buffer[0], Count);
  end;
end;

procedure TStreamHelper.ReadStreamPart(AStream: TStream; Count: Integer);
var
  Buffer: array of Byte;
begin
  if Count > 0 then begin
    SetLength(Buffer, Count);
    FStream.ReadBuffer(Buffer[0], Count);
    if AStream.Size < (AStream.Position + Count) then
      AStream.Size := AStream.Position + Count;
    AStream.Write(Buffer[0], Count);
  end;
end;

procedure TStreamHelper.WriteStreamPart(AStream: TStream; Count: Integer);
var
  Buffer: array of Byte;
begin
  if Count > AStream.Size then Count := AStream.Size; // Limit max. stream size
  if Count > 0 then begin
    SetLength(Buffer, Count);
    AStream.Read(Buffer[0], Count);
    FStream.Write(Buffer[0], Count);
  end;
end;

constructor TStreamHelper.Create;
begin
  inherited;
  Endianness := enLittle;
  FStream := TMemoryStream.Create;
  FOwnStream := True;
end;

constructor TStreamHelper.Create(AStream: TStream);
begin
  inherited Create;
  Endianness := enLittle;
  FStream := AStream;
  FOwnStream := False;
end;

procedure TStreamHelper.Clear;
begin
  Stream.Size := 0;
end;

destructor TStreamHelper.Destroy;
begin
  if FOwnStream then FStream.Free;
  inherited Destroy;
end;

function TStreamHelper.GetSize: Int64;
begin
  Result := Stream.Size;
end;

procedure TStreamHelper.SetSize(NewSize: Longint);
begin
  Stream.Size := NewSize;
end;

function TStreamHelper.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := Stream.Seek(Offset, Origin);
end;

function TStreamHelper.Read(var Buffer; Count: Longint): Longint;
begin
  Result := Stream.Read(Buffer, Count);
end;

function TStreamHelper.Write(const Buffer; Count: Longint): Longint;
begin
  Result := Stream.Write(Buffer, Count);
end;

function TStreamHelper.ReadUnixTime: TDateTime;
begin
  Result := UnixToDateTime(ReadCardinal);
end;

function TStreamHelper.ReadDouble: Double;
begin
  FStream.ReadBuffer(Result, SizeOf(Double));
end;

function TStreamHelper.ReadSingle: Single;
begin
  FStream.ReadBuffer(Result, SizeOf(Single));
end;

function TStreamHelper.Sum: Byte;
begin
  Result := 0;
  FStream.Position := 0;
  while FStream.Position < FStream.Size do
    Result := (Result + FStream.ReadByte) and $ff;
end;

procedure TStreamHelper.FillByte(Data:Byte;Count:Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    WriteByte(Data);
end;

function TStreamHelper.ReadWord: Word;
begin
  FStream.ReadBuffer(Result, SizeOf(Word));
  if SwapData then Result := Swap(Result);
end;

procedure TStreamHelper.SetEndianness(const AValue: TEndianness);
begin
  FEndianness := AValue;
  {$if defined(FPC_LITTLE_ENDIAN)}
  SwapData := FEndianness = enBig;
  {$elseif defined(FPC_BIG_ENDIAN)}
  SwapData := FEndianness = enLittle;
  {$endif}
end;

function TStreamHelper.GetItem(Index: Integer): Byte;
begin
  Position := Index;
  Result := ReadByte;
end;

procedure TStreamHelper.SetItem(Index: Integer; const AValue: Byte);
begin
 Position := Index;
 WriteByte(AValue);
end;

procedure TStreamHelper.Assign(Source: TStreamHelper);
var
  OldPosition: Integer;
begin
  OldPosition := Source.Position;
  Clear;
  WriteStream(Source, Source.Size);
  Source.Position := OldPosition;
  Position := OldPosition;
end;

procedure TStreamHelper.WriteAnsiString(Data: string);
var
  StringLength: Longint;
begin
  StringLength := Length(Data);
  Write(StringLength, SizeOf(StringLength));
  Write(Data[1], StringLength);
end;

procedure TStreamHelper.WriteByte(Data: Byte);
begin
  FStream.Write(Data, SizeOf(Byte));
end;

procedure TStreamHelper.WriteCardinal(Data: Cardinal);
begin
  if SwapData then Data := Swap(Data);
  Write(Data, SizeOf(Cardinal));
end;

procedure TStreamHelper.WriteInt64(Data: Int64);
begin
  if SwapData then Data := Swap(Data);
  Write(Data, SizeOf(Int64));
end;

procedure TStreamHelper.WriteString(Data:string);
begin
  if Length(Data) > 0 then
    Write(Data[1], Length(Data));
end;

procedure TStreamHelper.WriteShortString(Data: ShortString);
begin
  WriteByte(Length(Data));
  Write(Data[1], Length(Data));
end;

procedure TStreamHelper.WriteStream(AStream: TStream; Count: Integer);
var
  Buffer: array of Byte;
begin
  if Count > AStream.Size then Count := AStream.Size; // Limit max. stream size
  AStream.Position := 0;
  if Count > 0 then begin
    SetLength(Buffer, Count);
    AStream.Read(Buffer[0], Count);
    FStream.Write(Buffer[0], Count);
  end;
end;

procedure TStreamHelper.WriteDouble(Value: Double);
begin
  Write(Value, SizeOf(Double));
end;

procedure TStreamHelper.WriteSingle(Value: Single);
begin
  Write(Value, SizeOf(Single));
end;

procedure TStreamHelper.WriteUnixTime(Data: TDateTime);
var
  DataUnix: Int64;
begin
  DataUnix := DateTimeToUnix(Data);
  WriteCardinal(DataUnix);
end;

procedure TStreamHelper.WriteWord(Data: Word);
begin
  if SwapData then Data := Swap(Data);
  Write(Data, SizeOf(Word));
end;


end.
