unit UBitStream;

{$mode delphi}{$H+}

interface

uses
  Classes, DateUtils, syncobjs, UObjectTypeBase, UObjectBoolean;

type
  { TMemoryBitStream }

  TMemoryBitStream = class(TInterfacedObject, IAssignable, IComparable)
  private
    FMemoryStream: TMemoryStream;
    FPositionOffset: Byte;
    FSizeOffset: Byte;
    function GetBit(Bit: Longint): TBoolean;
    function GetPosition:Int64;
    function GetSize:Int64;
    procedure SetBit(Bit: Longint;const AValue: TBoolean);
    procedure SetPosition(const AValue: Int64);
    procedure SetSize(const AValue: Int64);
  public
    procedure ReadBits(Bits: TMemoryBitStream; Count: Integer);
    procedure WriteBits(Bits: TMemoryBitStream);

    procedure AndBy(Bits: TMemoryBitStream);
    procedure OrBy(Bits: TMemoryBitStream);
    procedure XorBy(Bits: TMemoryBitStream);
    procedure NotBy(Bits: TMemoryBitStream);

    function Lookup(Bit: TBoolean): Integer;
    procedure Reverse;
    procedure Clear;
    procedure Assign(Source: TInterfacedObject);
    function EqualTo(Operand: IComparable): TBoolean;

    constructor Create;
    destructor Destroy; override;

    property Position: Int64 read GetPosition write SetPosition;
    property Size: Int64 read GetSize write SetSize;
    property Bits[Bit: Longint]: TBoolean read GetBit write SetBit; default;
  end;
 
implementation


{ TMemoryBitStream }

function TMemoryBitStream.GetBit(Bit: Longint): TBoolean;
begin

end;

function TMemoryBitStream.GetPosition:Int64;
begin
  Result := FMemoryStream.Position + FPositionOffset;
end;

function TMemoryBitStream.GetSize:Int64;
begin

end;

procedure TMemoryBitStream.SetBit(Bit: Longint;const AValue: TBoolean);
begin
end;

procedure TMemoryBitStream.SetPosition(const AValue: Int64);
begin
  FMemoryStream.Position := AValue shr 3;
  FPositionOffset := AValue and 7;
end;

procedure TMemoryBitStream.SetSize(const AValue: Int64);
begin
  FSizeOffset := AValue and 7;
  if FSizeOffset = 0 then FMemoryStream.Size := (AValue shr 3)
    else FMemoryStream.Size := (AValue shr 3) + 1;
end;

procedure TMemoryBitStream.ReadBits(Bits:TMemoryBitStream;Count:Integer);
begin

end;

procedure TMemoryBitStream.WriteBits(Bits:TMemoryBitStream);
begin

end;

procedure TMemoryBitStream.AndBy(Bits:TMemoryBitStream);
begin

end;

procedure TMemoryBitStream.OrBy(Bits:TMemoryBitStream);
begin

end;

procedure TMemoryBitStream.XorBy(Bits:TMemoryBitStream);
begin

end;

procedure TMemoryBitStream.NotBy(Bits:TMemoryBitStream);
begin

end;

function TMemoryBitStream.Lookup(Bit:TBoolean):Integer;
begin
  while (Position < Size) and (Bits[Position] <> Bit) do
    Position := Position + 1;
  if Position < Size then Result := Position
    else Result := -1;
end;

procedure TMemoryBitStream.Reverse;
begin

end;

procedure TMemoryBitStream.Clear;
begin
  Size := 0;
  Position := 0;
end;

procedure TMemoryBitStream.Assign(Source:TInterfacedObject);
begin
  if Assigned(Source) and (Source is TMemoryBitStream) then begin
    FMemoryStream.LoadFromStream(TMemoryBitStream(Source).FMemoryStream);
    FPositionOffset := TMemoryBitStream(Source).FPositionOffset;
    FSizeOffset := TMemoryBitStream(Source).FSizeOffset;
  end;
end;

function TMemoryBitStream.EqualTo(Operand:IComparable):TBoolean;
begin

end;

constructor TMemoryBitStream.Create;
begin
  FMemoryStream := TMemoryStream.Create;
end;

destructor TMemoryBitStream.Destroy;
begin
  FMemoryStream.Destroy;
  inherited Destroy;
end;

end.
