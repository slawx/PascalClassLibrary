unit UBitStream;

// Date: 2010-06-14

{$mode delphi}

interface

uses
  Classes, SysUtils, RtlConsts;

type
  TBytes = array[0..MaxInt - 1] of Byte;

  { TBitStream }

  TBitStream = class
  private
    function GetPosition: LongInt; virtual;
    function GetSize: LongInt; virtual;
    procedure SetPosition(const AValue: LongInt); virtual;
    procedure SetSize(const AValue: LongInt); virtual;
  public
    function Seek(Offset: LongInt; Origin: TSeekOrigin): LongInt; virtual;
    function Read(var Buffer; Count: Longint): Longint; virtual;
    function CopyFrom(Source: TBitStream; Count: LongInt): LongInt;
    function Write(const Buffer; Count: Longint): Longint; virtual;
    procedure ReadBuffer(var Buffer; Count: Longint);
    procedure WriteBuffer(const Buffer; Count: Longint);
    property Position: LongInt read GetPosition write SetPosition;
    property Size: LongInt read GetSize write SetSize;
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

function TBitStream.GetPosition:LongInt;
begin
  Result := Seek(0, soCurrent);
end;

function TBitStream.GetSize: LongInt;
var
  p: LongInt;
begin
  p := Seek(0, soCurrent);
  GetSize := Seek(0, soEnd);
  Seek(p, soBeginning);
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

end;

function TBitStream.Read(var Buffer; Count:Longint):Longint;
begin
  Result := 0;
end;

function TBitStream.CopyFrom(Source:TBitStream;Count:LongInt):LongInt;
var
  I: LongInt;
  Buffer: array[0..1023] of Byte;
begin
  Result := 0;
  while Count > 0 do begin
    if Count > (SizeOf(Buffer) * 8) then I := SizeOf(Buffer) * 8
      else I := Count;
    I := Source.Read(Buffer, I);
    I := Write(Buffer, I);
    if I = 0 then Break;
    Dec(Count, I);
    Result := Result + I;
  end;
end;

function TBitStream.Write(const Buffer; Count:Longint):Longint;
begin
  Result := 0;
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

procedure TMemoryBitStream.SetSize(const AValue:LongInt);
begin
  FSize := AValue;
  Stream.Size := Trunc(AValue / 8) + 1;
end;

function TMemoryBitStream.Read(var Buffer;Count:Longint):Longint;
var
  ByteCount: LongInt;
  I: LongInt;
  BytePos: Byte;
  Data: Byte;
begin
  if (FPosition + Count) > FSize then Count := FSize - FPosition;
  ByteCount := Trunc(Count / 8) + 1;
  BytePos := FPosition mod 8;
  Stream.Position := Trunc(FPosition / 8);
  Data := Stream.ReadByte;
  for I := 0 to ByteCount - 1 do begin
    TBytes(Buffer)[I] := (Data shr BytePos) and ((1 shl (8 - BytePos)) - 1);
    if I <> (ByteCount - 1) then
      Data := Stream.ReadByte;
    if BytePos > 0 then
      TBytes(Buffer)[I] := TBytes(Buffer)[I] or (Data and ((1 shl BytePos) - 1)) shl (8 - BytePos);
    if I = (ByteCount - 1) then
      TBytes(Buffer)[I] := TBytes(Buffer)[I] and ((1 shl (Count mod 8)) - 1);
  end;
  Inc(FPosition, Count);
  Result := Count;
end;

function TMemoryBitStream.Write(const Buffer;Count:Longint):Longint;
var
  ByteCount: LongInt;
  I: LongInt;
  BytePos: Byte;
  Data: Byte;
begin
  ByteCount := Trunc(Count / 8) + 1;
  BytePos := FPosition mod 8;
  Stream.Position := Trunc(FPosition / 8);
  if Stream.Position < Stream.Size then begin
    Data := Stream.ReadByte;
    Stream.Position := Stream.Position - 1;
  end else Data := 0;
  for I := 0 to ByteCount - 1 do begin
    Data := (Data and ((1 shl BytePos) - 1)) or
      ((TBytes(Buffer)[I] and ((1 shl (8 - BytePos)) - 1)) shl BytePos);
    if I = (ByteCount - 1) then
      Data := Data and ((1 shl (Count mod 8)) - 1);
    Stream.WriteByte(Data);
    Data := (TBytes(Buffer)[I] shr (8 - BytePos)) and ((1 shl BytePos) - 1);
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
  if FPosition > FSize then FPosition := FSize;
  Result := FPosition;
end;

constructor TMemoryBitStream.Create;
begin
  FStream := TMemoryStream.Create;
end;

destructor TMemoryBitStream.Destroy;
begin
  FStream.Destroy;
  inherited Destroy;
end;

end.

