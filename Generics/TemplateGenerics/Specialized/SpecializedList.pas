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
{$DEFINE TGListStringConverter := TListIntegerStringConverter}
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

// TListSmallInt<Integer, SmallInt>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := SmallInt}
{$DEFINE TGList := TListSmallInt}
{$DEFINE TGListSortCompare := TListSmallIntSortCompare}
{$DEFINE TGListStringConverter := TListSmallIntStringConverter}
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

// TListDouble<Integer, Double>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Double}
{$DEFINE TGList := TListDouble}
{$DEFINE TGListSortCompare := TListDoubleSortCompare}
{$DEFINE TGListStringConverter := TListDoubleStringConverter}
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericList.inc'}

// TListPointer<Integer, Pointer>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Pointer}
{$DEFINE TGList := TListPointer}
{$DEFINE TGListSortCompare := TListPointerSortCompare}
{$DEFINE TGListStringConverter := TListPointerStringConverter}
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

// TListString<Integer, string>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := string}
{$DEFINE TGList := TListString}
{$DEFINE TGListSortCompare := TListStringSortCompare}
{$DEFINE TGListStringConverter := TListStringStringConverter}
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

// TListByte<Integer, Byte>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Byte}
{$DEFINE TGList := TListByte}
{$DEFINE TGListSortCompare := TListByteSortCompare}
{$DEFINE TGListStringConverter := TListByteStringConverter}
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

// TListChar<Integer, Char>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Char}
{$DEFINE TGList := TListCharBase}
{$DEFINE TGListSortCompare := TListSortCompareChar}
{$DEFINE TGListStringConverter := TListStringConverterChar}
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

{ TListChar }

// TListByte<Integer, Char>
TListChar = class(TListCharBase)
  procedure UpperCase;
  procedure LowerCase;
  procedure Trim;
  procedure TrimLeft;
  procedure TrimRight;
end;

implementation

{$DEFINE IMPLEMENTATION_USES}
{$I 'GenericList.inc'}

// TListInteger<Integer, Integer>
{$DEFINE TGListIndex:=Integer}
{$DEFINE TGListItem:=Integer}
{$DEFINE TGList:=TListInteger}
{$DEFINE TGListSortCompare:=TListIntegerSortCompare}
{$DEFINE TGListStringConverter:=TListIntegerStringConverter}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

// TListSmallInt<Integer, SmallInt>
{$DEFINE TGListIndex:=Integer}
{$DEFINE TGListItem:=SmallInt}
{$DEFINE TGList:=TListSmallInt}
{$DEFINE TGListSortCompare:=TListSmallIntSortCompare}
{$DEFINE TGListStringConverter:=TListSmallIntStringConverter}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

// TListDouble<Integer, Double>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Double}
{$DEFINE TGList := TListDouble}
{$DEFINE TGListSortCompare := TListDoubleSortCompare}
{$DEFINE TGListStringConverter := TListDoubleStringConverter}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

// TListPointer<Integer, Pointer>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Pointer}
{$DEFINE TGList := TListPointer}
{$DEFINE TGListSortCompare := TListPointerSortCompare}
{$DEFINE TGListStringConverter := TListPointerStringConverter}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

// TListString<Integer, string>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := string}
{$DEFINE TGList := TListString}
{$DEFINE TGListSortCompare := TListStringSortCompare}
{$DEFINE TGListStringConverter := TListStringStringConverter}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

// TListByte<Integer, Byte>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Byte}
{$DEFINE TGList := TListByte}
{$DEFINE TGListSortCompare := TListByteSortCompare}
{$DEFINE TGListStringConverter := TListByteStringConverter}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

// TListByte<Integer, Char>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Char}
{$DEFINE TGList := TListCharBase}
{$DEFINE TGListSortCompare := TListSortCompareChar}
{$DEFINE TGListStringConverter := TListStringConverterChar}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

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

end.
