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
    procedure SaveToStream(Stream: TStream); virtual;
  end;

  TPBMessage = class;

  TPBStringItem = class(TPBItem)
    Value: string;
  end;

  TPBIntegerItem = class(TPBItem)
    Value: Integer;
  end;

  TPBMessageItem = class(TPBItem)
    Tag: Integer;
    Name: string;
    ItemType: TPBItemType;
  end;

  { TPBMessage }

  TPBMessage = class(TPBItem)
    Items: TList; // TList<TPBItem>;
    procedure SaveToStream(Stream: TStream); override;
    constructor Create;
  end;

  { TProtocolBuffer }

  TProtocolBuffer = class
    BaseMessage: TPBMessage;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    destructor Destroy; override;
  end;

implementation

uses
  UMemoryStreamEx;

{ TProtocolBuffer }

procedure TProtocolBuffer.LoadFromStream(Stream: TStream);
begin

end;

procedure TProtocolBuffer.SaveToStream(Stream: TStream);
begin
  BaseMessage.SaveToStream(Stream);
end;

destructor TProtocolBuffer.Destroy;
begin
  if Assigned(BaseMessage) then BaseMessage.Free;
  inherited Destroy;
end;

{ TPBMessage }

procedure TPBMessage.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
end;

constructor TPBMessage.Create;
begin
  ItemType := itLengthDelimited;
end;

{ TPBItem }

procedure TPBItem.SaveToStream(Stream: TStream);
var
  ByteIndex: Byte;
  Data: Byte;
begin
  with TMemoryStreamEx(Stream) do begin
    Data := ((Tag and $f) shl 3) or (Integer(ItemType) and $7);
    ByteIndex := 0;
    while Tag > (1 shl (ByteIndex * 8 + 4)) do begin
      WriteByte(Data or $80);
      Data := (Tag shr (ByteIndex * 8 + 4)) and $7f;
      Inc(ByteIndex);
    end;
    WriteByte(Data);
  end
end;

end.

