unit UProtocolBuffers;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils; 

type
  TPBItemType = (itRequired, itOptional, itRepeated);

  TPBEnumeration = class
  end;

  TPBItem = class
    Name: string;
    Tag: Integer;
    ItemType: TPBItemType;
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

  TPBMessage = class(TPBItem)
    Items: TList; // TList<TPBItem>;
  end;

  TProtocolBuffer = class
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    PMMessage: TPBMessage;
  end;

implementation

{ TProtocolBuffer }

procedure TProtocolBuffer.LoadFromStream(Stream: TStream);
begin

end;

procedure TProtocolBuffer.SaveToStream(Stream: TStream);
begin

end;

end.

