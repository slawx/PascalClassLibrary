unit UBinarySerializer;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, SpecializedList, SpecializedStream;

type
  TEndianness = (enBig, enLittle);

  { TBinarySerializer }

  TBinarySerializer = class
  private
    FGrow: Boolean;
    FList: TListByte;
    FEndianness: TEndianness;
    SwapData: Boolean;
    procedure SetList(const AValue: TListByte);
    procedure SetEndianness(const AValue: TEndianness);
    procedure ReverseByteOrder(var Buffer; Count: Integer);
  public
    Position: Integer;
    OwnsList: Boolean;
    procedure Write(var Buffer; Count: Integer); //inline;
    procedure Read(var Buffer; Count: Integer); //inline;

    procedure Assign(Source: TBinarySerializer);
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
    procedure WriteStream(AStream: TStreamByte; Count: Integer);
    procedure WriteStreamPart(AStream: TStreamByte; Count: Integer);
    procedure WriteList(List: TListByte; StartIndex, Count: Integer);
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
    procedure ReadList(List: TListByte; StartIndex, Count: Integer);
    constructor Create; overload;
    procedure Clear;
    destructor Destroy; override;
    property Endianness: TEndianness read FEndianness write SetEndianness;
    property List: TListByte read FList write SetList;
    property Grow: Boolean read FGrow write FGrow;
  end;


implementation

{ TBinarySerializer }

function TBinarySerializer.ReadAnsiString: string;
var
  StringLength: Longint;
begin
  StringLength := 0;
  Read(StringLength, SizeOf(StringLength));
  Result := ReadString(StringLength);
end;

function TBinarySerializer.ReadStringTerminated(Terminator: string = #0): string;
var
  Data: Char;
  I: Integer;
  OldPosition: Integer;
begin
  OldPosition := Position;
  Result := '';
  I := 1;
  repeat
    if Position >= FList.Count then Break;
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

function TBinarySerializer.ReadByte: Byte;
begin
  Result := 0;
  Read(Result, SizeOf(Byte));
end;

function TBinarySerializer.ReadCardinal: Cardinal;
begin
  Result := 0;
  Read(Result, SizeOf(Cardinal));
  if SwapData then Result := SwapEndian(Result);
end;

function TBinarySerializer.ReadInt64: Int64;
begin
  Result := 0;
  Read(Result, SizeOf(Int64));
  if SwapData then Result := SwapEndian(Result);
end;

function TBinarySerializer.ReadString(Length: Integer): string;
begin
  if Length > 0 then begin
    SetLength(Result, Length);
    Read(Result[1], Length);
  end else Result := '';
end;

function TBinarySerializer.ReadShortString: string;
var
  Count: Byte;
begin
  Count := 0;
  Read(Count, 1);
  Result := ReadString(Count);
end;

procedure TBinarySerializer.ReadStream(AStream: TStream; Count: Integer);
var
  Buffer: array of Byte;
begin
  if Count > 0 then begin
    SetLength(Buffer, Count);
    Read(Buffer[0], Count);
    AStream.Size := Count;
    AStream.Position := 0;
    AStream.Write(Buffer[0], Count);
  end;
end;

procedure TBinarySerializer.ReadStreamPart(AStream: TStream; Count: Integer);
var
  Buffer: array of Byte;
begin
  if Count > 0 then begin
    SetLength(Buffer, Count);
    Read(Buffer[0], Count);
    if AStream.Size < (AStream.Position + Count) then
      AStream.Size := AStream.Position + Count;
    Write(Buffer[0], Count);
  end;
end;

procedure TBinarySerializer.ReadList(List: TListByte; StartIndex, Count: Integer
  );
var
  Buffer: array of Byte;
begin
  if Count > (List.Count - StartIndex) then Count := (List.Count - StartIndex); // Limit max. stream size
  if Count > 0 then begin
    SetLength(Buffer, Count);
    Read(Pointer(Buffer)^, Count);
    List.ReplaceBuffer(StartIndex, Pointer(Buffer)^, Count);
  end;
end;

procedure TBinarySerializer.WriteStreamPart(AStream: TStreamByte; Count: Integer);
var
  Buffer: array of Byte;
begin
  if Count > AStream.Size then Count := AStream.Size; // Limit max. stream size
  if Count > 0 then begin
    SetLength(Buffer, Count);
    AStream.ReadBuffer(Pointer(Buffer)^, Count);
    Write(Pointer(Buffer)^, Count);
  end;
end;

procedure TBinarySerializer.WriteList(List: TListByte; StartIndex, Count: Integer);
var
  Buffer: array of Byte;
begin
//  if Count > (List.Count - StartIndex) then Count := (List.Count - StartIndex); // Limit max. stream size
  if Count > 0 then begin
    SetLength(Buffer, Count);
    List.GetBuffer(StartIndex, PByte(Buffer)^, Count);
    Write(Pointer(Buffer)^, Count);
  end;
end;

constructor TBinarySerializer.Create;
begin
  inherited;
  Endianness := enLittle;
  FList := nil;
  FGrow := True;
end;

procedure TBinarySerializer.Clear;
begin
  FList.Count := 0;
  Position := 0;
end;

destructor TBinarySerializer.Destroy;
begin
  if OwnsList then FList.Free;
  inherited Destroy;
end;

function TBinarySerializer.ReadUnixTime: TDateTime;
begin
  Result := UnixToDateTime(ReadCardinal);
end;

function TBinarySerializer.ReadDouble: Double;
begin
  Result := 0;
  Read(Result, SizeOf(Double));
end;

function TBinarySerializer.ReadSingle: Single;
begin
  Result := 0;
  Read(Result, SizeOf(Single));
end;

function TBinarySerializer.ReadWord: Word;
begin
  Result := 0;
  Read(Result, SizeOf(Word));
  if SwapData then Result := SwapEndian(Result);
end;

procedure TBinarySerializer.SetList(const AValue: TListByte);
begin
  if OwnsList then FList.Free;
  FList := AValue;
end;

procedure TBinarySerializer.SetEndianness(const AValue: TEndianness);
begin
  FEndianness := AValue;
  {$if defined(FPC_LITTLE_ENDIAN)}
  SwapData := FEndianness = enBig;
  {$elseif defined(FPC_BIG_ENDIAN)}
  SwapData := FEndianness = enLittle;
  {$endif}
end;

procedure TBinarySerializer.ReverseByteOrder(var Buffer; Count: Integer);
var
  I: Integer;
  Temp: Byte;
type
  TBytes = array of Byte;
begin
  I := 0;
  while I < (Count div 2) do begin
    Temp := TBytes(Buffer)[Count - 1 - I];
    TBytes(Buffer)[Count - 1 - I] := TBytes(Buffer)[I];
    TBytes(Buffer)[I] := Temp;
    I := I + 1;
  end;
end;

procedure TBinarySerializer.Write(var Buffer; Count: Integer);
var
  NewCount: Integer;
begin
  if FGrow then begin
    NewCount := Position + Count;
    if FList.Count < NewCount then
      FList.Count := NewCount;
  end;
  FList.ReplaceBuffer(Position, Buffer, Count);
  Inc(Position, Count);
end;

procedure TBinarySerializer.Read(var Buffer; Count: Integer);
begin
  FList.GetBuffer(Position, Buffer, Count);
  Inc(Position, Count);
end;

procedure TBinarySerializer.Assign(Source: TBinarySerializer);
begin
  FList := Source.FList;
end;

procedure TBinarySerializer.WriteAnsiString(Data: string);
var
  StringLength: Longint;
begin
  StringLength := Length(Data);
  Write(StringLength, SizeOf(StringLength));
  Write(Data[1], StringLength);
end;

procedure TBinarySerializer.WriteByte(Data: Byte);
begin
  Write(Data, SizeOf(Byte));
end;

procedure TBinarySerializer.WriteCardinal(Data: Cardinal);
begin
  if SwapData then Data := SwapEndian(Data);
  Write(Data, SizeOf(Cardinal));
end;

procedure TBinarySerializer.WriteInt64(Data: Int64);
begin
  if SwapData then Data := SwapEndian(Data);
  Write(Data, SizeOf(Int64));
end;

procedure TBinarySerializer.WriteString(Data:string);
begin
  if Length(Data) > 0 then
    Write(Data[1], Length(Data));
end;

procedure TBinarySerializer.WriteShortString(Data: ShortString);
begin
  WriteByte(Length(Data));
  Write(Data[1], Length(Data));
end;

procedure TBinarySerializer.WriteStream(AStream: TStreamByte; Count: Integer);
var
  Buffer: array of Byte;
begin
  if Count > AStream.Size then Count := AStream.Size; // Limit max. stream size
  AStream.Position := 0;
  if Count > 0 then begin
    SetLength(Buffer, Count);
    AStream.ReadBuffer(Pointer(Buffer)^, Count);
    Write(Pointer(Buffer)^, Count);
  end;
end;

procedure TBinarySerializer.WriteDouble(Value: Double);
begin
  Write(Value, SizeOf(Double));
end;

procedure TBinarySerializer.WriteSingle(Value: Single);
begin
  Write(Value, SizeOf(Single));
end;

procedure TBinarySerializer.WriteUnixTime(Data: TDateTime);
var
  DataUnix: Int64;
begin
  DataUnix := DateTimeToUnix(Data);
  WriteCardinal(DataUnix);
end;

procedure TBinarySerializer.WriteWord(Data: Word);
begin
  if SwapData then Data := SwapEndian(Data);
  Write(Data, SizeOf(Word));
end;

end.

