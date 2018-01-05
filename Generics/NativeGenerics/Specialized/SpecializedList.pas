unit SpecializedList;

{$mode delphi}

interface

uses
  Classes, SysUtils, GenericList;

type
  TListByte = class(TGList<Byte>)
    procedure WriteToStream(Stream: TStream);
    procedure WriteToStreamPart(Stream: TStream; ItemIndex, ItemCount: Integer);
    procedure ReplaceStream(Stream: TStream);
    procedure ReplaceStreamPart(Stream: TStream; ItemIndex, ItemCount: Integer);
    procedure AddStream(Stream: TStream);
    procedure AddStreamPart(Stream: TStream; ItemCount: Integer);
  end;

  TListChar = class(TGList<Char>)
    procedure UpperCase;
    procedure LowerCase;
    procedure Trim;
    procedure TrimLeft;
    procedure TrimRight;
  end;

  TListMethod = class(TGList<TMethod>)
    procedure CallAll;
  end;

  TListNotifyEvent = class(TGList<TNotifyEvent>)
    procedure CallAll(Sender: TObject);
  end;

  TSimpleEvent = procedure of object;
  TListSimpleEvent = class(TGList<TSimpleEvent>)
    procedure CallAll;
  end;

  function StrToStr(Value: string): string;


implementation

function StrToStr(Value: string): string;
begin
  Result := Value;
end;

{ TListSimpleEvent }

procedure TListSimpleEvent.CallAll;
var
  I: Integer;
begin
  I := 0;
  while (I < Count) do begin
    TSimpleEvent(Items[I])();
    I := I + 1;
  end;
end;


{ TListChar }

procedure TListChar.UpperCase;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if (FItems[I] in ['a'..'z']) then
      FItems[I] := Char(Byte(FItems[I]) - 32);
end;

procedure TListChar.LowerCase;
var
  I: Integer;
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
  I: Integer;
begin
  I := 0;
  while (I < Count) and (FItems[I] = ' ') do
    I := I + 1;
  if I < Count then
    DeleteItems(0, I);
end;

procedure TListChar.TrimRight;
var
  I: Integer;
begin
  I := Count - 1;
  while (I >= 0) and (FItems[I] = ' ') do
    I := I - 1;
  if I >= 0 then
    DeleteItems(I + 1, Count - I - 1);
end;

procedure TListMethod.CallAll;
var
  I: Integer;
begin
  I := 0;
  while (I < Count) do begin
    Items[I];
    I := I + 1;
  end;
end;

procedure TListNotifyEvent.CallAll(Sender: TObject);
var
  I: Integer;
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
