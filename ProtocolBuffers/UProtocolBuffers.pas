// Specification: http://code.google.com/intl/cs/apis/protocolbuffers/docs/overview.html
unit UProtocolBuffers;

{$mode delphi}

interface

uses
  Classes, SysUtils, Dialogs;

type
  TPBItemMode = (imRequired, imOptional, imRepeated);
  TPBWireType = (wtVariant, wt64bit, wtLengthDelimited, wtStartGroup,
    wtEndGroup, wt32bit);
  TPBItemType = (itInteger, itString, itMessage, itFloat, itDouble, itBlock);

  TPBEnumeration = class
  end;

  { TPBDefinition }
  TPBDefinition = class
  private
    function GetWireType: TPBWireType;
  public
    Name: string;
    Tag: Integer;
    ItemType: TPBItemType;
    ItemMode: TPBItemMode;
    Items: TList; // TList<TPBDefinition>
    DefaultString: string;
    DefaultInteger: Integer;
    constructor Create;
    destructor Destroy; override;
    function SearchItemByTag(Tag: Integer): Integer;
    property WireType: TPBWireType read GetWireType;
  end;

  { TPBItemHead }
  TPBItemHead = record
    Tag: Integer;
    WireType: TPBWireType;
  end;

  { TPBItem }
  TPBItem = class
    procedure SaveVariantToStream(Stream: TStream; Value: Integer);
    function LoadVariantFromStream(Stream: TStream): Integer;
    procedure SaveLengthDelimitedToStream(Stream: TStream; Block: TStream);
    procedure LoadLengthDelimitedFromStream(Stream: TStream; Block: TStream);
    procedure SaveHeadToStream(Stream: TStream; Definition: TPBDefinition);
    function LoadHeadFromStream(Stream: TStream; Definition: TPBDefinition): TPBItemHead;
    procedure SaveToStream(Stream: TStream; Definition: TPBDefinition); virtual;
    procedure LoadFromStream(Stream: TStream; Definition: TPBDefinition); virtual;
    procedure Clear(Definition: TPBDefinition); virtual;
    procedure Assign(Source: TPBItem); virtual;
  end;

  TPBMessageItem = class;

  { TPBStringItem }
  TPBStringItem = class(TPBItem)
    Value: string;
    procedure SaveToStream(Stream: TStream; Definition: TPBDefinition); override;
    procedure LoadFromStream(Stream: TStream; Definition: TPBDefinition); override;
    constructor Create;
    procedure Assign(Source: TPBItem); override;
  end;

  { TPBIntegerItem }
  TPBIntegerItem = class(TPBItem)
    Value: Integer;
    procedure SaveToStream(Stream: TStream; Definition: TPBDefinition); override;
    procedure LoadFromStream(Stream: TStream; Definition: TPBDefinition); override;
    constructor Create;
    procedure Assign(Source: TPBItem); override;
  end;

  { TPBMessageItem }
  TPBMessageItem = class(TPBItem)
    GenerateHead: Boolean;
    Items: TList; // TList<TList<TPBItem>>;
    procedure Clear(Definition: TPBDefinition); override;
    procedure SaveToStream(Stream: TStream; Definition: TPBDefinition); override;
    procedure LoadFromStream(Stream: TStream; Definition: TPBDefinition); override;
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPBItem); override;
  end;

  { TPBRepeatedItem }

  TPBRepeatedItem = class(TPBItem)
    Items: TList;
    procedure Clear(Definition: TPBDefinition); override;
    procedure SaveToStream(Stream: TStream; Definition: TPBDefinition); override;
    procedure LoadFromStream(Stream: TStream; Definition: TPBDefinition); override;
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPBItem); override;
  end;

  { TProtocolBuffer }
  TProtocolBuffer = class
    Definition: TPBDefinition;
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
  BaseMessage.LoadFromStream(Stream, Definition);
end;

procedure TProtocolBuffer.SaveToStream(Stream: TStream);
begin
  BaseMessage.GenerateHead := False;
  BaseMessage.SaveToStream(Stream, Definition);
end;

procedure TProtocolBuffer.LoadFromProto(Source: TStringList);
begin

end;

constructor TProtocolBuffer.Create;
begin
  BaseMessage := TPBMessageItem.Create;
  Definition := TPBDefinition.Create;
  Definition.ItemType := itMessage;
end;

destructor TProtocolBuffer.Destroy;
begin
  Definition.Destroy;
  BaseMessage.Free;
  inherited Destroy;
end;

{ TPBMessageItem }

procedure TPBMessageItem.Clear(Definition: TPBDefinition);
var
  I: Integer;
  Q: Integer;
begin
  for I := 0 to Items.Count - 1 do
    TPBItem(Items[I]).Clear(Definition);
  Items.Clear;
  Items.Count := Definition.Items.Count;
  for I := 0 to Items.Count - 1 do begin
    if TPBDefinition(Definition.Items[I]).ItemMode = imRepeated then
      Items[I] := TPBRepeatedItem.Create
    else
    if TPBDefinition(Definition.Items[I]).ItemType = itInteger then begin
      Items[I] := TPBIntegerItem.Create;
      TPBIntegerItem(Items[I]).Value := TPBDefinition(Definition.Items[I]).DefaultInteger;
    end else
    if TPBDefinition(Definition.Items[I]).ItemType = itString then begin
      Items[I] := TPBStringItem.Create;
      TPBStringItem(Items[I]).Value := TPBDefinition(Definition.Items[I]).DefaultString;
    end else
    if TPBDefinition(Definition.Items[I]).ItemType = itMessage then begin
      Items[I] := TPBMessageItem.Create;
      TPBMessageItem(Items[I]).Clear(Definition.Items[I]);
    end;
  end;
end;

procedure TPBMessageItem.SaveToStream(Stream: TStream; Definition: TPBDefinition);
var
  I: Integer;
  Q: Integer;
  TempStream: TMemoryStream;
begin
  inherited;
  // Generate message content to temporary stream
  TempStream := TMemoryStream.Create;
  if Items.Count <> Definition.Items.Count then
    raise Exception.Create('Definition and value items count mismatch.');
  for I := 0 to Definition.Items.Count - 1 do
    TPBItem(Items[I]).SaveToStream(TempStream, Definition.Items[I]);

  // If head is used than write lenght-delimited head type with block byte length
  if GenerateHead then begin
    SaveHeadToStream(Stream, Definition);
    SaveVariantToStream(Stream, TempStream.Size);
  end;
  TempStream.Position := 0;
  TempStream.SaveToStream(Stream);
  TempStream.Free;
end;

procedure TPBMessageItem.LoadFromStream(Stream: TStream; Definition: TPBDefinition);
var
  I: Integer;
  TempItem: TPBItem;
  ItemIndex: Integer;
  EndIndex: Integer;
  TempStream: TMemoryStream;
  ItemHead: TPBItemHead;
  NewItem: TPBItem;
begin
  inherited;
  TempStream := TMemoryStream.Create;

  if GenerateHead then begin
    I := LoadVariantFromStream(Stream);
    EndIndex := Stream.Position + I;
  end else EndIndex := Stream.Size;

  TempItem := TPBItem.Create;
  Clear(Definition);
  while Stream.Position < EndIndex do begin
    ItemHead := TempItem.LoadHeadFromStream(Stream, Definition);
    ItemIndex := Definition.SearchItemByTag(ItemHead.Tag);
    if ItemIndex <> -1 then
      with TPBDefinition(Definition.Items[ItemIndex]) do begin
        if WireType <> ItemHead.WireType then
          raise Exception.Create('Bad type for item "' + TPBDefinition(Definition.Items[ItemIndex]).Name +
            '" with tag ' + IntToStr(ItemHead.Tag));
        if ItemType = itInteger then begin
          NewItem := TPBIntegerItem.Create;
          TPBIntegerItem(NewItem).LoadFromStream(Stream, Definition.Items[ItemIndex]);
        end else
        if TPBDefinition(Definition.Items[ItemIndex]).ItemType = itString then begin
          NewItem := TPBStringItem.Create;
          TPBStringItem(NewItem).LoadFromStream(Stream, Definition.Items[ItemIndex])
        end else
        if TPBDefinition(Definition.Items[ItemIndex]).ItemType = itMessage then begin
          NewItem := TPBMessageItem.Create;
          TPBMessageItem(NewItem).LoadFromStream(Stream, Definition.Items[ItemIndex]);
        end;

        if ItemMode = imRepeated then begin
          TPBRepeatedItem(Self.Items[ItemIndex]).Items.Add(NewItem);
        end else begin
          TPBItem(Self.Items[ItemIndex]).Assign(NewItem);
          NewItem.Free;
        end;
    end else begin
      // Skip item data
      if ItemHead.WireType = wtVariant then
        TempItem.LoadVariantFromStream(Stream)
      else if ItemHead.WireType = wtLengthDelimited then
        TempItem.LoadLengthDelimitedFromStream(Stream, TempStream);
    end;
  end;
  TempStream.Free;
end;

constructor TPBMessageItem.Create;
begin
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

procedure TPBMessageItem.Assign(Source: TPBItem);
var
  I: Integer;
begin
  if Source is TPBMessageItem then begin
    GenerateHead := TPBMessageItem(Source).GenerateHead;
    for I := 0 to Items.Count - 1 do
      TPBItem(Items[I]).Assign(TPBMessageItem(Source).Items[I]);
  end;
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

procedure TPBItem.SaveHeadToStream(Stream: TStream; Definition: TPBDefinition);
var
  ByteIndex: Byte;
  Data: Byte;
begin
  with TMemoryStreamEx(Stream) do begin
    Data := ((Definition.Tag and $f) shl 3) or (Integer(Definition.WireType) and $7);
    ByteIndex := 0;
    while Definition.Tag > (1 shl (ByteIndex * 7 + 4)) do begin
      WriteByte(Data or $80);
      Data := (Definition.Tag shr (ByteIndex * 7 + 4)) and $7f;
      Inc(ByteIndex);
    end;
    WriteByte(Data);
  end
end;

function TPBItem.LoadHeadFromStream(Stream: TStream; Definition: TPBDefinition): TPBItemHead;
var
  Data: Byte;
  ByteIndex: Byte;
begin
  Data := TMemoryStreamEx(Stream).ReadByte;
  Result.WireType := TPBWireType(Data and 3);
  Result.Tag := (Data shr 3) and $f;
  ByteIndex := 0;
  while Data > $7f do begin
    Data := TMemoryStreamEx(Stream).ReadByte;
    Result.Tag := Result.Tag or ((Data and $7f) shl (ByteIndex * 7 + 4));
    Inc(ByteIndex);
  end;
end;

procedure TPBItem.SaveToStream(Stream: TStream; Definition: TPBDefinition);
begin

end;

procedure TPBItem.LoadFromStream(Stream: TStream; Definition: TPBDefinition);
begin

end;

procedure TPBItem.Clear(Definition: TPBDefinition);
begin

end;

procedure TPBItem.Assign(Source: TPBItem);
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

procedure TPBIntegerItem.SaveToStream(Stream: TStream; Definition: TPBDefinition);
begin
  inherited;
  SaveHeadToStream(Stream, Definition);
  SaveVariantToStream(Stream, Value);
end;

procedure TPBIntegerItem.LoadFromStream(Stream: TStream; Definition: TPBDefinition);
begin
  inherited;
  Value := LoadVariantFromStream(Stream);
end;

constructor TPBIntegerItem.Create;
begin
end;

procedure TPBIntegerItem.Assign(Source: TPBItem);
begin
  if Source is TPBIntegerItem then
    Value := TPBIntegerItem(Source).Value;
end;

{ TPBStringItem }

procedure TPBStringItem.SaveToStream(Stream: TStream; Definition: TPBDefinition);
begin
  inherited;
  SaveHeadToStream(Stream, Definition);
  SaveVariantToStream(Stream, Length(Value));
  if Length(Value) > 0 then
    TMemoryStreamEx(Stream).Write(Value[1], Length(Value));
end;

procedure TPBStringItem.LoadFromStream(Stream: TStream; Definition: TPBDefinition);
begin
  inherited;
  SetLength(Value, LoadVariantFromStream(Stream));
  if Length(Value) > 0 then
    TMemoryStreamEx(Stream).Read(Value[1], Length(Value));
end;

constructor TPBStringItem.Create;
begin
end;

procedure TPBStringItem.Assign(Source: TPBItem);
begin
  if Source is TPBStringItem then
    Value := TPBStringItem(Source).Value;
  inherited Assign(Source);
end;

{ TPBDefinition }

function TPBDefinition.SearchItemByTag(Tag: Integer): Integer;
var
  I: Integer;
begin
  I := 0;
  while (I < Items.Count) and (TPBDefinition(Items[I]).Tag <> Tag) do Inc(I);
  if I < Items.Count then Result := I
    else Result := -1;
end;

function TPBDefinition.GetWireType: TPBWireType;
begin
  case ItemType of
    itInteger: Result := wtVariant;
    itFloat: Result := wt64bit;
    itDouble: Result := wt32bit;
    itString: Result := wtLengthDelimited;
    itMessage: Result := wtLengthDelimited;
  end;
end;

constructor TPBDefinition.Create;
begin
  Items := TList.Create;
end;

destructor TPBDefinition.Destroy;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    TPBDefinition(Items[I]).Destroy;
  Items.Free;
  inherited Destroy;
end;

{ TPBRepeatedItem }

procedure TPBRepeatedItem.Clear(Definition: TPBDefinition);
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do begin
    TPBItem(Items[I]).Free;
    if Definition.ItemType = itInteger then begin
      Items[I] := TPBIntegerItem.Create;
      TPBIntegerItem(Items[I]).Value := Definition.DefaultInteger;
    end else
    if Definition.ItemType = itString then begin
      Items[I] := TPBStringItem.Create;
      TPBStringItem(Items[I]).Value := Definition.DefaultString;
    end else
    if Definition.ItemType = itMessage then begin
      Items[I] := TPBMessageItem.Create;
      TPBMessageItem(Items[I]).Clear(Definition);
    end;
  end;
  inherited;
end;

procedure TPBRepeatedItem.SaveToStream(Stream: TStream;
  Definition: TPBDefinition);
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do begin
    TPBItem(Items[I]).SaveToStream(Stream, Definition);
  end;
end;

procedure TPBRepeatedItem.LoadFromStream(Stream: TStream;
  Definition: TPBDefinition);
begin
  inherited LoadFromStream(Stream, Definition);
end;

constructor TPBRepeatedItem.Create;
begin
  Items := TList.Create;
end;

destructor TPBRepeatedItem.Destroy;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    TPBItem(Items[I]).Free;
  Items.Free;
  inherited Destroy;
end;

procedure TPBRepeatedItem.Assign(Source: TPBItem);
var
  I: Integer;
begin
  if Source is TPBRepeatedItem then begin
    for I := 0 to Items.Count - 1 do
      TPBItem(Items[I]).Assign(TPBRepeatedItem(Source).Items[I]);
  end;
  inherited Assign(Source);
end;

end.

