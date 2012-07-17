unit UMemory;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TMemory }

  TMemory = class
  private
    FData: PByte;
    function GetItem(Index: Integer): Byte;
    procedure SetItem(Index: Integer; AValue: Byte);
    function GetSize: Integer; virtual;
    procedure SetSize(AValue: Integer); virtual;
  public
    procedure Fill(Value: Byte = 0);
    procedure Assign(Source: TMemory);
    constructor Create;
    destructor Destroy; override;
    property Data: PByte read FData;
    property Size: Integer read GetSize write SetSize;
    property Items[Index: Integer]: Byte read GetItem write SetItem; default;
  end;

  { TPositionMemory }

  TPositionMemory = class(TMemory)
  private
    FPosition: Integer;
  protected
    procedure SetSize(AValue: Integer); override;
  public
    procedure WriteByte(Value: Byte);
    function ReadByte: Byte;
    property Position: Integer read FPosition write FPosition;
  end;

  { TMemoryRec }

  TMemoryRec = record
  private
    FData: PByte;
    function GetItem(Index: Integer): Byte; inline;
    procedure SetItem(Index: Integer; AValue: Byte); inline;
    function GetSize: Integer; inline;
    procedure SetSize(AValue: Integer); inline;
  public
    procedure Fill(Value: Byte = 0); inline;
    procedure Assign(Source: TMemoryRec); inline;
    property Data: PByte read FData;
    property Size: Integer read GetSize write SetSize;
    property Items[Index: Integer]: Byte read GetItem write SetItem; default;
  end;

  { TBitMemoryRec }

  TBitMemoryRec = record
  private
    FMemory: TMemoryRec;
    FSize: Cardinal;
    function GetItem(Index: Cardinal): Boolean; inline;
    function GetSize: Cardinal; inline;
    procedure SetItem(Index: Cardinal; AValue: Boolean); inline;
    procedure SetSize(AValue: Cardinal); inline;
  public
    procedure WriteItems(Addr: Cardinal; Items: TBitMemoryRec);
    procedure Fill(Value: Boolean = False);
    procedure Assign(Source: TBitMemoryRec); inline;
    property Memory: TMemoryRec read FMemory;
    property Size: Cardinal read GetSize write SetSize;
    property Items[Index: Cardinal]: Boolean read GetItem write SetItem; default;
  end;


implementation

{ TBitMemoryRec }

function TBitMemoryRec.GetItem(Index: Cardinal): Boolean;
begin
  Result := Boolean((FMemory[Index shr 3] shr (Index and 7)) and 1);
end;

function TBitMemoryRec.GetSize: Cardinal;
begin
  Result := FSize;
end;

procedure TBitMemoryRec.SetItem(Index: Cardinal; AValue: Boolean);
begin
  FMemory[Index shr 3] := (FMemory[Index shr 3] and ($ff xor (1 shl (Index and 7)))) or
    (Byte(AValue) shl (Index and 7));
end;

procedure TBitMemoryRec.SetSize(AValue: Cardinal);
begin
  FSize := AValue;
  FMemory.Size := (AValue - 1) shr 3 + 1;
end;

procedure TBitMemoryRec.WriteItems(Addr: Cardinal; Items: TBitMemoryRec);
begin

end;

procedure TBitMemoryRec.Fill(Value: Boolean);
begin
  FMemory.Fill($ff * Byte(Value));
end;

procedure TBitMemoryRec.Assign(Source: TBitMemoryRec);
begin
  Size := Source.Size;
  FMemory.Assign(Source.Memory);
end;

{ TMemoryRec }

function TMemoryRec.GetItem(Index: Integer): Byte;
begin
  Result := PByte(FData + Index)^;
end;

procedure TMemoryRec.SetItem(Index: Integer; AValue: Byte);
begin
  PByte(FData + Index)^ := AValue;
end;

function TMemoryRec.GetSize: Integer;
begin
  Result := MemSize(FData);
end;

procedure TMemoryRec.SetSize(AValue: Integer);
begin
  FData := ReAllocMem(FData, AValue);
end;

procedure TMemoryRec.Fill(Value: Byte);
begin
  FillChar(FData^, Size, Value);
end;

procedure TMemoryRec.Assign(Source: TMemoryRec);
begin
  Size := Source.Size;
  Move(Source.Data^, FData^, Size);
end;

{ TPositionMemory }

procedure TPositionMemory.SetSize(AValue: Integer);
begin
  inherited SetSize(AValue);
  if FPosition > Size then FPosition := Size;
end;

procedure TPositionMemory.WriteByte(Value: Byte);
begin
  if FPosition >= Size then Size := FPosition + 1;
  Items[FPosition] := Value;
  Inc(FPosition);
end;

function TPositionMemory.ReadByte: Byte;
begin
  if FPosition >= Size then Size := FPosition + 1;
  Result := Items[FPosition];
  Inc(FPosition);
end;

{ TMemory }

procedure TMemory.SetSize(AValue: Integer);
begin
  FData := ReAllocMem(FData, AValue);
end;

function TMemory.GetItem(Index: Integer): Byte;
begin
  Result := PByte(FData + Index)^;
end;

procedure TMemory.SetItem(Index: Integer; AValue: Byte);
begin
  PByte(FData + Index)^ := AValue;
end;

function TMemory.GetSize: Integer;
begin
  Result := MemSize(FData);
end;

procedure TMemory.Fill(Value: Byte);
begin
  FillChar(FData^, Size, Value);
end;

procedure TMemory.Assign(Source: TMemory);
begin
  Size := Source.Size;
  Move(Source.Data^, FData^, Size);
end;

constructor TMemory.Create;
begin
  FData := nil;
  Size := 0;
end;

destructor TMemory.Destroy;
begin
  Size := 0;
  inherited Destroy;
end;

end.
