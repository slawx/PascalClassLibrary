unit SpecializedList;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
{$MACRO ON}

// TListInteger<Integer, Integer>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Integer}
{$DEFINE TGList := TListInteger}
{$DEFINE TGListSortCompare := TListIntegerSortCompare}
{$DEFINE TGListToStringConverter := TListIntegerToStringConverter}
{$DEFINE TGListFromStringConverter := TListIntegerFromStringConverter}
{$DEFINE TGListItemArray := TListIntegerItemArray}
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

// TListBoolean<Integer, Boolean>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Boolean}
{$DEFINE TGList := TListBoolean}
{$DEFINE TGListSortCompare := TListBooleanSortCompare}
{$DEFINE TGListToStringConverter := TListBooleanToStringConverter}
{$DEFINE TGListFromStringConverter := TListBooleanFromStringConverter}
{$DEFINE TGListItemArray := TListBooleanItemArray}
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

// TListSmallInt<Integer, SmallInt>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := SmallInt}
{$DEFINE TGList := TListSmallInt}
{$DEFINE TGListSortCompare := TListSmallIntSortCompare}
{$DEFINE TGListToStringConverter := TListSmallIntToStringConverter}
{$DEFINE TGListFromStringConverter := TListSmallIntFromStringConverter}
{$DEFINE TGListItemArray := TListSmallIntItemArray}
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

// TListDouble<Integer, Double>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Double}
{$DEFINE TGList := TListDouble}
{$DEFINE TGListSortCompare := TListDoubleSortCompare}
{$DEFINE TGListToStringConverter := TListDoubleToStringConverter}
{$DEFINE TGListFromStringConverter := TListDoubleFromStringConverter}
{$DEFINE TGListItemArray := TListDoubleItemArray}
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericList.inc'}

// TListPointer<Integer, Pointer>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Pointer}
{$DEFINE TGList := TListPointer}
{$DEFINE TGListSortCompare := TListPointerSortCompare}
{$DEFINE TGListToStringConverter := TListPointerToStringConverter}
{$DEFINE TGListFromStringConverter := TListPointerFromStringConverter}
{$DEFINE TGListItemArray := TListPointerItemArray}
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

// TListString<Integer, string>
{$DEFINE TGListStringIndex := Integer}
{$DEFINE TGListStringItem := string}
{$DEFINE TGListString := TListString}
{$DEFINE TGListStringSortCompare := TListStringSortCompare}
{$DEFINE TGListStringToStringConverter := TListStringToStringConverter}
{$DEFINE TGListStringFromStringConverter := TListStringFromStringConverter}
{$DEFINE TGListItemArray := TListStringItemArray}
{$DEFINE INTERFACE}
{$I 'GenericListString.inc'}

// TListByte<Integer, Byte>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Byte}
{$DEFINE TGList := TListByteBase}
{$DEFINE TGListSortCompare := TListByteSortCompare}
{$DEFINE TGListToStringConverter := TListByteToStringConverter}
{$DEFINE TGListFromStringConverter := TListByteFromStringConverter}
{$DEFINE TGListItemArray := TListByteItemArray}
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

TListByte = class(TListByteBase)
  procedure WriteToStream(Stream: TStream);
  procedure WriteToStreamPart(Stream: TStream; ItemIndex, ItemCount: TGListIndex);
  procedure ReplaceStream(Stream: TStream);
  procedure ReplaceStreamPart(Stream: TStream; ItemIndex, ItemCount: TGListIndex);
  procedure AddStream(Stream: TStream);
  procedure AddStreamPart(Stream: TStream; ItemCount: TGListIndex);
  procedure WriteBuffer(var Buffer; Count: Integer);
  procedure ReadBuffer(var Buffer; Count: Integer);
end;

// TListChar<Integer, Char>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Char}
{$DEFINE TGList := TListCharBase}
{$DEFINE TGListSortCompare := TListSortCompareChar}
{$DEFINE TGListToStringConverter := TListToStringConverterChar}
{$DEFINE TGListFromStringConverter := TListFromStringConverterChar}
{$DEFINE TGListItemArray := TListStringItemArray}
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

// TListObject<Integer, TObject>
{$DEFINE TGListObjectIndex := Integer}
{$DEFINE TGListObjectItem := TObject}
{$DEFINE TGListObjectList := TListObjectList}
{$DEFINE TGListObject := TListObject}
{$DEFINE TGListObjectSortCompare := TListObjectSortCompare}
{$DEFINE TGListObjectToStringConverter := TListObjectToStringConverter}
{$DEFINE TGListObjectFromStringConverter := TListObjectFromStringConverter}
{$DEFINE TGListItemArray := TListObjectItemArray}
{$DEFINE INTERFACE}
{$I 'GenericListObject.inc'}


{ TListChar }

// TListByte<Integer, Char>
TListChar = class(TListCharBase)
  procedure UpperCase;
  procedure LowerCase;
  procedure Trim;
  procedure TrimLeft;
  procedure TrimRight;
end;

// TListMethodBase<Integer, TMethod>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := TMethod}
{$DEFINE TGList := TListMethodBase}
{$DEFINE TGListSortCompare := TListMethodSortCompare}
{$DEFINE TGListToStringConverter := TListMethodToStringConverter}
{$DEFINE TGListFromStringConverter := TListMethodFromStringConverter}
{$DEFINE TGListItemArray := TListMethodItemArray}
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

// TListMethod<Integer, TMethod>
TListMethod = class(TListMethodBase)
  procedure CallAll;
end;

// TListNotifyEventBase<Integer, TNotifyEvent>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := TNotifyEvent}
{$DEFINE TGList := TListNotifyEventBase}
{$DEFINE TGListSortCompare := TListNotifyEventSortCompare}
{$DEFINE TGListToStringConverter := TListNotifyEventToStringConverter}
{$DEFINE TGListFromStringConverter := TListNotifyEventFromStringConverter}
{$DEFINE TGListItemArray := TListNotifyEventItemArray}
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

// TListNotifyEvent<Integer, TNotifyEvent>
TListNotifyEvent = class(TListNotifyEventBase)
  procedure CallAll(Sender: TObject);
end;


TBaseEvent = procedure of object;

// TListSimpleEventBase<Integer, TBaseEvent>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := TBaseEvent}
{$DEFINE TGList := TListSimpleEventBase}
{$DEFINE TGListSortCompare := TListSimpleEventSortCompare}
{$DEFINE TGListToStringConverter := TListSimpleEventToStringConverter}
{$DEFINE TGListFromStringConverter := TListSimpleEventFromStringConverter}
{$DEFINE TGListItemArray := TListSimpleEventItemArray}
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

// TListSimpleEvent<Integer, TSimpleEvent>
TListSimpleEvent = class(TListSimpleEventBase)
  procedure CallAll;
end;


function StrToStr(Value: string): string;

implementation

{$DEFINE IMPLEMENTATION_USES}
{$I 'GenericList.inc'}

// TListInteger<Integer, Integer>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Integer}
{$DEFINE TGList := TListInteger}
{$DEFINE TGListSortCompare := TListIntegerSortCompare}
{$DEFINE TGListToStringConverter := TListIntegerToStringConverter}
{$DEFINE TGListFromStringConverter := TListIntegerFromStringConverter}
{$DEFINE TGListItemArray := TListIntegerItemArray}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

// TListSmallInt<Integer, SmallInt>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := SmallInt}
{$DEFINE TGList := TListSmallInt}
{$DEFINE TGListSortCompare := TListSmallIntSortCompare}
{$DEFINE TGListToStringConverter := TListSmallIntToStringConverter}
{$DEFINE TGListFromStringConverter := TListSmallIntFromStringConverter}
{$DEFINE TGListItemArray := TListSmallIntItemArray}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

// TListBoolean<Integer, Boolean>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Boolean}
{$DEFINE TGList := TListBoolean}
{$DEFINE TGListSortCompare := TListBooleanSortCompare}
{$DEFINE TGListToStringConverter := TListBooleanToStringConverter}
{$DEFINE TGListFromStringConverter := TListBooleanFromStringConverter}
{$DEFINE TGListItemArray := TListBooleanItemArray}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

// TListDouble<Integer, Double>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Double}
{$DEFINE TGList := TListDouble}
{$DEFINE TGListSortCompare := TListDoubleSortCompare}
{$DEFINE TGListToStringConverter := TListDoubleToStringConverter}
{$DEFINE TGListFromStringConverter := TListDoubleFromStringConverter}
{$DEFINE TGListItemArray := TListDoubleItemArray}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

// TListPointer<Integer, Pointer>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Pointer}
{$DEFINE TGList := TListPointer}
{$DEFINE TGListSortCompare := TListPointerSortCompare}
{$DEFINE TGListToStringConverter := TListPointerToStringConverter}
{$DEFINE TGListFromStringConverter := TListPointerFromStringConverter}
{$DEFINE TGListItemArray := TListPointerItemArray}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

// TListString<Integer, string>
{$DEFINE TGListStringIndex := Integer}
{$DEFINE TGListStringItem := string}
{$DEFINE TGListString := TListString}
{$DEFINE TGListStringSortCompare := TListStringSortCompare}
{$DEFINE TGListStringToStringConverter := TListStringToStringConverter}
{$DEFINE TGListStringFromStringConverter := TListStringFromStringConverter}
{$DEFINE TGListItemArray := TListStringItemArray}
{$DEFINE IMPLEMENTATION}
{$I 'GenericListString.inc'}

// TListByte<Integer, Byte>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Byte}
{$DEFINE TGList := TListByteBase}
{$DEFINE TGListSortCompare := TListByteSortCompare}
{$DEFINE TGListToStringConverter := TListByteToStringConverter}
{$DEFINE TGListFromStringConverter := TListByteFromStringConverter}
{$DEFINE TGListItemArray := TListByteItemArray}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

// TListByte<Integer, Char>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Char}
{$DEFINE TGList := TListCharBase}
{$DEFINE TGListSortCompare := TListSortCompareChar}
{$DEFINE TGListToStringConverter := TListToStringConverterChar}
{$DEFINE TGListFromStringConverter := TListFromStringConverterChar}
{$DEFINE TGListItemArray := TListStringItemArray}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

// TListObject<Integer, TObject>
{$DEFINE TGListObjectIndex := Integer}
{$DEFINE TGListObjectItem := TObject}
{$DEFINE TGListObjectList := TListObjectList}
{$DEFINE TGListObject := TListObject}
{$DEFINE TGListObjectSortCompare := TListObjectSortCompare}
{$DEFINE TGListObjectToStringConverter := TListObjectToStringConverter}
{$DEFINE TGListObjectFromStringConverter := TListObjectFromStringConverter}
{$DEFINE TGListItemArray := TListObjectItemArray}
{$DEFINE IMPLEMENTATION}
{$I 'GenericListObject.inc'}

// TListMethod<Integer, TMethod>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := TMethod}
{$DEFINE TGList := TListMethodBase}
{$DEFINE TGListSortCompare := TListMethodSortCompare}
{$DEFINE TGListToStringConverter := TListMethodToStringConverter}
{$DEFINE TGListFromStringConverter := TListMethodFromStringConverter}
{$DEFINE TGListItemArray := TListMethodItemArray}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

// TListNotifyEventBase<Integer, TNotifyEvent>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := TNotifyEvent}
{$DEFINE TGList := TListNotifyEventBase}
{$DEFINE TGListSortCompare := TListNotifyEventSortCompare}
{$DEFINE TGListToStringConverter := TListNotifyEventToStringConverter}
{$DEFINE TGListFromStringConverter := TListNotifyEventFromStringConverter}
{$DEFINE TGListItemArray := TListNotifyEventItemArray}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

// TListSimpleEventBase<Integer, TBaseEvent>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := TBaseEvent}
{$DEFINE TGList := TListSimpleEventBase}
{$DEFINE TGListSortCompare := TListSimpleEventSortCompare}
{$DEFINE TGListToStringConverter := TListSimpleEventToStringConverter}
{$DEFINE TGListFromStringConverter := TListSimpleEventFromStringConverter}
{$DEFINE TGListItemArray := TListSimpleEventItemArray}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}



function StrToStr(Value: string): string;
begin
  Result := Value;
end;

{ TListSimpleEvent }

procedure TListSimpleEvent.CallAll;
var
  I: TGListIndex;
begin
  I := 0;
  while (I < Count) do begin
    TBaseEvent(Items[I])();
    I := I + 1;
  end;
end;


{ TListChar }

procedure TListChar.UpperCase;
var
  I: TGListIndex;
begin
  for I := 0 to Count - 1 do
    if (FItems[I] in ['a'..'z']) then
      FItems[I] := Char(Byte(FItems[I]) - 32);
end;

procedure TListChar.LowerCase;
var
  I: TGListIndex;
begin
  for I := 0 to Count - 1 do
    if (FItems[I] in ['A'..'Z']) then
      FItems[I] := Char(Byte(FItems[I]) + 32);
end;

procedure TListChar.Trim;
begin
  TrimLeft;
  TrimRight;
end;

procedure TListChar.TrimLeft;
var
  I: TGListIndex;
begin
  I := 0;
  while (I < Count) and (FItems[I] = ' ') do
    I := I + 1;
  if I < Count then
    DeleteItems(0, I);
end;

procedure TListChar.TrimRight;
var
  I: TGListIndex;
begin
  I := Count - 1;
  while (I >= 0) and (FItems[I] = ' ') do
    I := I - 1;
  if I >= 0 then
    DeleteItems(I + 1, Count - I - 1);
end;

procedure TListMethod.CallAll;
var
  I: TGListIndex;
begin
  I := 0;
  while (I < Count) do begin
    Items[I];
    I := I + 1;
  end;
end;

procedure TListNotifyEvent.CallAll(Sender: TObject);
var
  I: TGListIndex;
begin
  I := 0;
  while (I < Count) do begin
    TNotifyEvent(Items[I])(Sender);
    I := I + 1;
  end;
end;

{ TListByte }

procedure TListByte.WriteToStream(Stream: TStream);
var
  I: Integer;
begin
  Stream.Position := 0;
  I := 0;
  while I < Count do begin
    Stream.WriteByte(Items[I]);
    I := I + 1;
  end;
end;

procedure TListByte.WriteToStreamPart(Stream: TStream; ItemIndex, ItemCount: Integer);
var
  I: Integer;
begin
  I := ItemIndex;
  while I < ItemCount do begin
    Stream.WriteByte(Items[I]);
    I := I + 1;
  end;
end;

procedure TListByte.ReplaceStream(Stream: TStream);
var
  I: Integer;
begin
  Stream.Position := 0;
  I := 0;
  while I < Count do begin
    Items[I] := Stream.ReadByte;
    I := I + 1;
  end;
end;

procedure TListByte.ReplaceStreamPart(Stream: TStream; ItemIndex,
  ItemCount: Integer);
var
  I: Integer;
begin
  I := ItemIndex;
  while I < ItemCount do begin
    Items[I] := Stream.ReadByte;
    I := I + 1;
  end;
end;

procedure TListByte.AddStream(Stream: TStream);
var
  I: Integer;
begin
  Stream.Position := 0;
  I := Count;
  Count := Count + Stream.Size;
  while I < Count do begin
    Items[I] := Stream.ReadByte;
    I := I + 1;
  end;
end;

procedure TListByte.AddStreamPart(Stream: TStream; ItemCount: Integer);
var
  I: Integer;
begin
  I := Count;
  Count := Count + ItemCount;
  while I < Count do begin
    Items[I] := Stream.ReadByte;
    I := I + 1;
  end;
end;

procedure TListByte.WriteBuffer(var Buffer; Count: Integer);
begin

end;

procedure TListByte.ReadBuffer(var Buffer; Count: Integer);
begin

end;

end.
