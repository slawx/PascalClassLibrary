// http://code.google.com/intl/cs/apis/protocolbuffers/docs/overview.html
unit UProtocolBuffers;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TPBItemMode = (imRequired, imOptional, imRepeated);
  TPBItemType = (itVariant, it64bit, itLengthDelimited, itStartGroup,
    itEndGroup, it32bit);

  TPBEnumeration = class
  end;

  { TPBItem }
  TPBItem = class
    Name: string;
    Tag: Integer;
    ItemType: TPBItemType;
    ItemMode: TPBItemMode;
    procedure SaveVariantToStream(Stream: TStream; Value: Integer);
    function LoadVariantFromStream(Stream: TStream): Integer;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
  end;

  TPBMessageItem = class;

  { TPBStringItem }
  TPBStringItem = class(TPBItem)
    Value: string;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    constructor Create;
  end;

  { TPBIntegerItem }
  TPBIntegerItem = class(TPBItem)
    Value: Integer;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    constructor Create;
  end;

  { TPBMessageItem }
  TPBMessageItem = class(TPBItem)
    Items: TList; // TList<TPBItem>;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    constructor Create;
    destructor Destroy; override;
  end;

  { TProtocolBuffer }
  TProtocolBuffer = class
    BaseMessage: TPBMessageItem;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromProto(Source: TStringList);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  UMemoryStreamEx;

{ TProtocolBuffer }

procedure TProtocolBuffer.LoadFromStream(Stream: TStream);
begin
  BaseMessage.LoadFromStream(Stream);
end;

procedure TProtocolBuffer.SaveToStream(Stream: TStream);
begin
  BaseMessage.SaveToStream(Stream);
end;

procedure TProtocolBuffer.LoadFromProto(Source: TStringList);
begin

end;

constructor TProtocolBuffer.Create;
begin
  BaseMessage := TPBMessageItem.Create;
end;

destructor TProtocolBuffer.Destroy;
begin
  if Assigned(BaseMessage) then BaseMessage.Free;
  inherited Destroy;
end;

{ TPBMessageItem }

procedure TPBMessageItem.SaveToStream(Stream: TStream);
var
  I: Integer;
begin
  inherited SaveToStream(Stream);
  for I := 0 to Items.Count - 1 do
    TPBItem(Items[I]).SaveToStream(Stream);
end;

procedure TPBMessageItem.LoadFromStream(Stream: TStream);
var
  I: Integer;
begin
  inherited LoadFromStream(Stream);
  for I := 0 to Items.Count - 1 do
    TPBItem(Items[I]).LoadFromStream(Stream);
end;

constructor TPBMessageItem.Create;
begin
  ItemType := itLengthDelimited;
  Items := TList.Create;
end;

destructor TPBMessageItem.Destroy;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    TPBItem(Items[I]).Free;
  Items.Free;
  inherited Destroy;
end;

{ TPBItem }

procedure TPBItem.SaveVariantToStream(Stream: TStream; Value: Integer);
var
  ByteIndex: Byte;
  Data: Byte;
begin
  with TMemoryStreamEx(Stream) do begin
    Data := Value and $7f;
    ByteIndex := 1;
    while Value > (1 shl (ByteIndex * 7)) do begin
      WriteByte(Data or $80);
      Data := (Value shr (ByteIndex * 7)) and $7f;
      Inc(ByteIndex);
    end;
    WriteByte(Data);
  end
end;

procedure TPBItem.SaveToStream(Stream: TStream);
var
  ByteIndex: Byte;
  Data: Byte;
begin
  with TMemoryStreamEx(Stream) do begin
    Data := ((Tag and $f) shl 3) or (Integer(ItemType) and $7);
    ByteIndex := 0;
    while Tag > (1 shl (ByteIndex * 7 + 4)) do begin
      WriteByte(Data or $80);
      Data := (Tag shr (ByteIndex * 7 + 4)) and $7f;
      Inc(ByteIndex);
    end;
    WriteByte(Data);
  end
end;

procedure TPBItem.LoadFromStream(Stream: TStream);
var
  Data: Byte;
  ByteIndex: Byte;
begin
  Data := TMemoryStreamEx(Stream).ReadByte;
  ItemType := TPBItemType(Data and 3);
  Tag := (Data shr 3) and $f;
  ByteIndex := 0;
  while Data > $7f do begin
    Data := TMemoryStreamEx(Stream).ReadByte;
    Tag := Tag or ((Data and $7f) shl (ByteIndex * 7 + 4));
    Inc(ByteIndex);
  end;
end;

function TPBItem.LoadVariantFromStream(Stream: TStream): Integer;
var
  Data: Byte;
  ByteIndex: Byte;
begin
  Data := TMemoryStreamEx(Stream).ReadByte;
  Result := Data and $7f;
  ByteIndex := 1;
  while Data > $7f do begin
    Data := TMemoryStreamEx(Stream).ReadByte;
    Result := Result or ((Data and $7f) shl (ByteIndex * 7));
    Inc(ByteIndex);
  end;
end;

{ TPBIntegerItem }

procedure TPBIntegerItem.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  SaveVariantToStream(Stream, Value);
end;

procedure TPBIntegerItem.LoadFromStream(Stream: TStream);
begin
  inherited LoadFromStream(Stream);
  LoadVariantFromStream(Stream);
end;

constructor TPBIntegerItem.Create;
begin
  ItemType := itVariant;
end;

{ TPBStringItem }

procedure TPBStringItem.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  SaveVariantToStream(Stream, Length(Value));
  TMemoryStreamEx(Stream).Write(Value[1], Length(Value));
end;

procedure TPBStringItem.LoadFromStream(Stream: TStream);
begin
  inherited LoadFromStream(Stream);
  SetLength(Value, LoadVariantFromStream(Stream));
  TMemoryStreamEx(Stream).Read(Value[1], Length(Value));
end;

constructor TPBStringItem.Create;
begin
  ItemType := itLengthDelimited;
end;

end.

