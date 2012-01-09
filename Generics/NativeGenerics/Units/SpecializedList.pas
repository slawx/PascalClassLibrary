unit SpecializedList;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, GenericList;

type
  TListChar = class(TGList<Char>)
    procedure UpperCase;
    procedure LowerCase;
    procedure Trim;
    procedure TrimLeft;
    procedure TrimRight;
  end;

implementation

{ TListChar }

procedure TListChar.UpperCase;
var
  I: TIndex;
begin
  for I := 0 to Count - 1 do
    if (FItems[I] in ['a'..'z']) then
      FItems[I] := Char(Byte(FItems[I]) - 32);
end;

procedure TListChar.LowerCase;
var
  I: TIndex;
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
  I: TIndex;
begin
  I := 0;
  while (I < Count) and (FItems[I] = ' ') do
    I := I + 1;
  if I < Count then
    DeleteItems(0, I);
end;

procedure TListChar.TrimRight;
var
  I: TIndex;
begin
  I := Count - 1;
  while (I >= 0) and (FItems[I] = ' ') do
    I := I - 1;
  if I >= 0 then
    DeleteItems(I + 1, Count - I - 1);
end;

end.

