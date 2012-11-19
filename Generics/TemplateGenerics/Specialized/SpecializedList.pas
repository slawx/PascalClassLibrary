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
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

// TListBoolean<Integer, Boolean>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Boolean}
{$DEFINE TGList := TListBoolean}
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

// TListSmallInt<Integer, SmallInt>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := SmallInt}
{$DEFINE TGList := TListSmallInt}
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

// TListDouble<Integer, Double>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Double}
{$DEFINE TGList := TListDouble}
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericList.inc'}

// TListPointer<Integer, Pointer>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Pointer}
{$DEFINE TGList := TListPointer}
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

// TListString<Integer, string>
{$DEFINE TGListStringIndex := Integer}
{$DEFINE TGListStringItem := string}
{$DEFINE TGListString := TListString}
{$DEFINE INTERFACE}
{$I 'GenericListString.inc'}

// TListByte<Integer, Byte>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Byte}
{$DEFINE TGList := TListByteBase}
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

TListByte = class(TListByteBase)
  procedure WriteToStream(Stream: TStream);
  procedure WriteToStreamPart(Stream: TStream; ItemIndex, ItemCount: TGListIndex);
  procedure ReplaceStream(Stream: TStream);
  procedure ReplaceStreamPart(Stream: TStream; ItemIndex, ItemCount: TGListIndex);
  procedure AddStream(Stream: TStream);
  procedure AddStreamPart(Stream: TStream; ItemCount: TGListIndex);
end;

// TListChar<Integer, Char>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Char}
{$DEFINE TGList := TListCharBase}
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

// TListObject<Integer, TObject>
{$DEFINE TGListObjectIndex := Integer}
{$DEFINE TGListObjectItem := TObject}
{$DEFINE TGListObjectList := TListObjectList}
{$DEFINE TGListObject := TListObject}
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
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

// TListSimpleEvent<Integer, TSimpleEvent>
TListSimpleEvent = class(TListSimpleEventBase)
  procedure CallAll;
end;


// TFileListByte<Integer, Byte>
{$DEFINE TGFileListIndex := Integer}
{$DEFINE TGFileListItem := Byte}
{$DEFINE TGFileListList := TFileListListByte}
{$DEFINE TGFileList := TFileListByte}
{$DEFINE INTERFACE}
{$I 'GenericFileList.inc'}

function StrToStr(Value: string): string;





implementation

{$DEFINE IMPLEMENTATION_USES}
{$I 'GenericList.inc'}

// TListInteger<Integer, Integer>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Integer}
{$DEFINE TGList := TListInteger}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

// TListSmallInt<Integer, SmallInt>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := SmallInt}
{$DEFINE TGList := TListSmallInt}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

// TListBoolean<Integer, Boolean>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Boolean}
{$DEFINE TGList := TListBoolean}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

// TListDouble<Integer, Double>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Double}
{$DEFINE TGList := TListDouble}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

// TListPointer<Integer, Pointer>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Pointer}
{$DEFINE TGList := TListPointer}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

// TListString<Integer, string>
{$DEFINE TGListStringIndex := Integer}
{$DEFINE TGListStringItem := string}
{$DEFINE TGListString := TListString}
{$DEFINE IMPLEMENTATION}
{$I 'GenericListString.inc'}

// TListByte<Integer, Byte>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Byte}
{$DEFINE TGList := TListByteBase}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

// TListByte<Integer, Char>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := Char}
{$DEFINE TGList := TListCharBase}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

// TListObject<Integer, TObject>
{$DEFINE TGListObjectIndex := Integer}
{$DEFINE TGListObjectItem := TObject}
{$DEFINE TGListObjectList := TListObjectList}
{$DEFINE TGListObject := TListObject}
{$DEFINE IMPLEMENTATION}
{$I 'GenericListObject.inc'}

// TListMethod<Integer, TMethod>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := TMethod}
{$DEFINE TGList := TListMethodBase}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

// TListNotifyEventBase<Integer, TNotifyEvent>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := TNotifyEvent}
{$DEFINE TGList := TListNotifyEventBase}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

// TListSimpleEventBase<Integer, TBaseEvent>
{$DEFINE TGListIndex := Integer}
{$DEFINE TGListItem := TBaseEvent}
{$DEFINE TGList := TListSimpleEventBase}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

// TFileListByte<Integer, Byte>
{$DEFINE TGFileListIndex := Integer}
{$DEFINE TGFileListItem := Byte}
{$DEFINE TGFileListList := TFileListListByte}
{$DEFINE TGFileList := TFileListByte}
{$DEFINE IMPLEMENTATION}
{$I 'GenericFileList.inc'}


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
  I := Count - 1;
  while (I >= 0) do begin
    TNotifyEvent(Items[I])(Sender);
    I := I - 1;
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


end.
