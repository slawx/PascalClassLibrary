unit GenericString;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, GenericList;

type

  { TGString }

  TGString<TChar> = class(TGList<TChar>)
    procedure UpperCase;
    procedure LowerCase;
    procedure Trim(WhiteChar: TChar = ' ');
    procedure TrimLeft(WhiteChar: TChar = ' ');
    procedure TrimRight(WhiteChar: TChar = ' ');
  end;

implementation

{ TGString }

procedure TGString<TChar>.UpperCase;
var
  I: TIndex;
begin
  for I := 0 to Count - 1 do
    if (Items[I] in ['a'..'z']) then
      Items[I] := Char(Byte(Items[I]) - 32);
end;

procedure TGString<TChar>.LowerCase;
var
  I: TIndex;
begin
  for I := 0 to Count - 1 do
    if (Items[I] in ['A'..'Z']) then
      Items[I] := Char(Byte(Items[I]) + 32);
end;

procedure TGString<TChar>.Trim(WhiteChar: TChar = ' ');
begin
  TrimLeft(WhiteChar);
  TrimRight(WhiteChar);
end;

procedure TGString<TChar>.TrimLeft(WhiteChar: TChar = ' ');
var
  I: TIndex;
begin
  I := 0;
  while (I < Count) and (Items[I] = WhiteChar) do
    I := I + 1;
  if I < Count then
    DeleteItems(0, I);
end;

procedure TGString<TChar>.TrimRight(WhiteChar: TChar = ' ');
var
  I: TIndex;
begin
  I := Count - 1;
  while (I >= 0) and (Items[I] = WhiteChar) do
    I := I - 1;
  if I >= 0 then
    DeleteItems(I + 1, Count - I - 1);
end;


end.

