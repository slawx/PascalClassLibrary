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
    procedure SaveLengthDelimitedToStream(Stream: TStream; Block: TStream);
    procedure LoadLengthDelimitedFromStream(Stream: TStream; Block: TStream);
    procedure SaveHeadToStream(Stream: TStream);
    procedure LoadHeadFromStream(Stream: TStream);
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
    GenerateHead: Boolean;
    Items: TList; // TList<TPBItem>;
    function SearchItemByTag(Tag: Integer): TPBItem;
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
  BaseMessage.GenerateHead := False;
  BaseMessage.LoadFromStream(Stream);
end;

procedure TProtocolBuffer.SaveToStream(Stream: TStream);
begin
  BaseMessage.GenerateHead := False;
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

function TPBMessageItem.SearchItemByTag(Tag: Integer): TPBItem;
var
  I: Integer;
begin
  I := 0;
  while (I < Items.Count) and (TPBItem(Items[I]).Tag <> Tag) do Inc(I);
  if I < Items.Count then Result := Items[I]
    else Result := nil;
end;

procedure TPBMessageItem.SaveToStream(Stream: TStream);
var
  I: Integer;
  TempStream: TMemoryStream;
begin
  inherited;
  // Generate message content to temporary stream
  TempStream := TMemoryStream.Create;
  for I := 0 to Items.Count - 1 do
    TPBItem(Items[I]).SaveToStream(TempStream);
  // if head is used than write lenght-delimited head type with block byte length
  if GenerateHead then begin
    SaveHeadToStream(Stream);
    SaveVariantToStream(Stream, TempStream.Size);
  end;
  TempStream.Position := 0;
  TempStream.SaveToStream(Stream);
  TempStream.Free;
end;

procedure TPBMessageItem.LoadFromStream(Stream: TStream);
var
  I: Integer;
  TempItem: TPBItem;
  SearchItem: TPBItem;
  EndIndex: Integer;
  TempStream: TMemoryStream;
begin
  inherited;
  TempStream := TMemoryStream.Create;

  if GenerateHead then begin
    I := LoadVariantFromStream(Stream);
    EndIndex := Stream.Position + I;
  end else EndIndex := Stream.Size;

  TempItem := TPBItem.Create;
  while Stream.Position < EndIndex do begin
    TempItem.LoadHeadFromStream(Stream);
    SearchItem := SearchItemByTag(TempItem.Tag);
    if Assigned(SearchItem) then begin
      if SearchItem.ItemType <> TempItem.ItemType then
        raise Exception.Create('Bad type for item "' + SearchItem.Name +
          '" with tag ' + IntToStr(SearchItem.Tag));
      if SearchItem is TPBIntegerItem then
        TPBIntegerItem(SearchItem).LoadFromStream(Stream)
      else if SearchItem is TPBStringItem then
        TPBStringItem(SearchItem).LoadFromStream(Stream)
      else if SearchItem is TPBMessageItem then
        TPBMessageItem(SearchItem).LoadFromStream(Stream);
    end else begin
      if TempItem.ItemType = itVariant then
        TempItem.LoadVariantFromStream(Stream)
      else if TempItem.ItemType = itLengthDelimited then
        TempItem.LoadLengthDelimitedFromStream(Stream, TempStream);
    end;
  end;
  TempStream.Free;
end;

constructor TPBMessageItem.Create;
begin
  ItemType := itLengthDelimited;
  Items := TList.Create;
  GenerateHead := True;
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

procedure TPBItem.SaveHeadToStream(Stream: TStream);
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

procedure TPBItem.LoadHeadFromStream(Stream: TStream);
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

procedure TPBItem.SaveToStream(Stream: TStream);
begin

end;

procedure TPBItem.LoadFromStream(Stream: TStream);
begin

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

procedure TPBItem.SaveLengthDelimitedToStream(Stream: TStream; Block: TStream);
begin
  SaveVariantToStream(Stream, Block.Size);
  Block.Position := 0;
  TMemoryStreamEx(Block).ReadStream(Stream, Block.Size);
end;

procedure TPBItem.LoadLengthDelimitedFromStream(Stream: TStream; Block: TStream
  );
var
  Size: Integer;
begin
  Size := LoadVariantFromStream(Stream);
  TMemoryStreamEx(Stream).ReadStream(Block, Size);
end;

{ TPBIntegerItem }

procedure TPBIntegerItem.SaveToStream(Stream: TStream);
begin
  inherited;
  SaveHeadToStream(Stream);
  SaveVariantToStream(Stream, Value);
end;

procedure TPBIntegerItem.LoadFromStream(Stream: TStream);
begin
  inherited;
  Value := LoadVariantFromStream(Stream);
end;

constructor TPBIntegerItem.Create;
begin
  ItemType := itVariant;
end;

{ TPBStringItem }

procedure TPBStringItem.SaveToStream(Stream: TStream);
begin
  inherited;
  SaveHeadToStream(Stream);
  SaveVariantToStream(Stream, Length(Value));
  TMemoryStreamEx(Stream).Write(Value[1], Length(Value));
end;

procedure TPBStringItem.LoadFromStream(Stream: TStream);
begin
  inherited;
  SetLength(Value, LoadVariantFromStream(Stream));
  TMemoryStreamEx(Stream).Read(Value[1], Length(Value));
end;

constructor TPBStringItem.Create;
begin
  ItemType := itLengthDelimited;
end;

end.

