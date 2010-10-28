unit ListChar;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TListIndex = Integer;
  TListItem = Char;
{$INCLUDE '..\Generic\ListInterface.tpl'}

type

  { TListChar }

  TListChar = class(TGList)
    procedure UpperCase;
    procedure LowerCase;
    procedure Trim;
    procedure TrimLeft;
    procedure TrimRight;
  end;

  TString = TListChar;

implementation

{$INCLUDE '..\Generic\ListImplementation.tpl'}


{ TListChar }

procedure TListChar.UpperCase;
var
  I: TListIndex;
begin
  for I := 0 to Count - 1 do
    if (FItems[I] in ['a'..'z']) then
      FItems[I] := Char(Byte(FItems[I]) - 32);
end;

procedure TListChar.LowerCase;
var
  I: TListIndex;
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
  I: TListIndex;
begin
  I := 0;
  while (I < Count) and (FItems[I] = ' ') do
    I := I + 1;
  if I < Count then
    DeleteItems(0, I);
end;

procedure TListChar.TrimRight;
var
  I: TListIndex;
begin
  I := Count - 1;
  while (I >= 0) and (FItems[I] = ' ') do
    I := I - 1;
  if I >= 0 then
    DeleteItems(I + 1, Count - I - 1);
end;

end.
