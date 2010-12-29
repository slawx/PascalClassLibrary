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
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

// TListSmallInt<Integer, SmallInt>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := SmallInt}
{$DEFINE TGList := TListSmallInt}
{$DEFINE TGListSortCompare := TListSmallIntSortCompare}
{$DEFINE TGListToStringConverter := TListSmallIntToStringConverter}
{$DEFINE TGListFromStringConverter := TListSmallIntFromStringConverter}
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

// TListDouble<Integer, Double>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Double}
{$DEFINE TGList := TListDouble}
{$DEFINE TGListSortCompare := TListDoubleSortCompare}
{$DEFINE TGListToStringConverter := TListDoubleToStringConverter}
{$DEFINE TGListFromStringConverter := TListDoubleFromStringConverter}
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericList.inc'}

// TListPointer<Integer, Pointer>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Pointer}
{$DEFINE TGList := TListPointer}
{$DEFINE TGListSortCompare := TListPointerSortCompare}
{$DEFINE TGListToStringConverter := TListPointerToStringConverter}
{$DEFINE TGListFromStringConverter := TListPointerFromStringConverter}
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

// TListString<Integer, string>
{$DEFINE TGListStringIndex := Integer}
{$DEFINE TGListStringItem := string}
{$DEFINE TGListString := TListString}
{$DEFINE TGListStringSortCompare := TListStringSortCompare}
{$DEFINE TGListStringToStringConverter := TListStringToStringConverter}
{$DEFINE TGListStringFromStringConverter := TListStringFromStringConverter}
{$DEFINE INTERFACE}
{$I 'GenericListString.inc'}

// TListByte<Integer, Byte>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Byte}
{$DEFINE TGList := TListByte}
{$DEFINE TGListSortCompare := TListByteSortCompare}
{$DEFINE TGListToStringConverter := TListByteToStringConverter}
{$DEFINE TGListFromStringConverter := TListByteFromStringConverter}
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

// TListChar<Integer, Char>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Char}
{$DEFINE TGList := TListCharBase}
{$DEFINE TGListSortCompare := TListSortCompareChar}
{$DEFINE TGListToStringConverter := TListToStringConverterChar}
{$DEFINE TGListFromStringConverter := TListFromStringConverterChar}
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
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

// TListMethod<Integer, TMethod>
TListMethod = class(TListMethodBase)
  procedure CallAll;
  procedure CallNotifyEvents(Sender: TObject);
end;

function StrToStr(Value: string): string;

implementation

{$DEFINE IMPLEMENTATION_USES}
{$I 'GenericList.inc'}

// TListInteger<Integer, Integer>
{$DEFINE TGListIndex:=Integer}
{$DEFINE TGListItem:=Integer}
{$DEFINE TGList:=TListInteger}
{$DEFINE TGListSortCompare:=TListIntegerSortCompare}
{$DEFINE TGListToStringConverter:=TListIntegerToStringConverter}
{$DEFINE TGListFromStringConverter:=TListIntegerFromStringConverter}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

// TListSmallInt<Integer, SmallInt>
{$DEFINE TGListIndex:=Integer}
{$DEFINE TGListItem:=SmallInt}
{$DEFINE TGList:=TListSmallInt}
{$DEFINE TGListSortCompare:=TListSmallIntSortCompare}
{$DEFINE TGListToStringConverter:=TListSmallIntToStringConverter}
{$DEFINE TGListFromStringConverter:=TListSmallIntFromStringConverter}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

// TListDouble<Integer, Double>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Double}
{$DEFINE TGList := TListDouble}
{$DEFINE TGListSortCompare := TListDoubleSortCompare}
{$DEFINE TGListToStringConverter := TListDoubleToStringConverter}
{$DEFINE TGListFromStringConverter := TListDoubleFromStringConverter}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

// TListPointer<Integer, Pointer>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Pointer}
{$DEFINE TGList := TListPointer}
{$DEFINE TGListSortCompare := TListPointerSortCompare}
{$DEFINE TGListToStringConverter := TListPointerToStringConverter}
{$DEFINE TGListFromStringConverter := TListPointerFromStringConverter}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

// TListString<Integer, string>
{$DEFINE TGListStringIndex := Integer}
{$DEFINE TGListStringItem := string}
{$DEFINE TGListString := TListString}
{$DEFINE TGListStringSortCompare := TListStringSortCompare}
{$DEFINE TGListStringToStringConverter := TListStringToStringConverter}
{$DEFINE TGListStringFromStringConverter := TListStringFromStringConverter}
{$DEFINE IMPLEMENTATION}
{$I 'GenericListString.inc'}

// TListByte<Integer, Byte>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Byte}
{$DEFINE TGList := TListByte}
{$DEFINE TGListSortCompare := TListByteSortCompare}
{$DEFINE TGListToStringConverter := TListByteToStringConverter}
{$DEFINE TGListFromStringConverter := TListByteFromStringConverter}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

// TListByte<Integer, Char>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Char}
{$DEFINE TGList := TListCharBase}
{$DEFINE TGListSortCompare := TListSortCompareChar}
{$DEFINE TGListToStringConverter := TListToStringConverterChar}
{$DEFINE TGListFromStringConverter := TListFromStringConverterChar}
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
{$DEFINE IMPLEMENTATION}
{$I 'GenericListObject.inc'}

// TListMethod<Integer, TMethod>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := TMethod}
{$DEFINE TGList := TListMethodBase}
{$DEFINE TGListSortCompare := TListMethodSortCompare}
{$DEFINE TGListToStringConverter := TListMethodToStringConverter}
{$DEFINE TGListFromStringConverter := TListMethodFromStringConverter}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}


function StrToStr(Value: string): string;
begin
  Result := Value;
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

procedure TListMethod.CallNotifyEvents(Sender: TObject);
var
  I: TGListIndex;
begin
  I := 0;
  while (I < Count) do begin
    TNotifyEvent(Items[I])(Sender);
    I := I + 1;
  end;
end;

end.
