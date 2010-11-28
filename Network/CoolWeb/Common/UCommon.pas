unit UCommon;

{$mode Delphi}{$H+}

interface

uses
  {$IFDEF Windows}
  Windows,
  ShFolder,
  {$ENDIF} SysUtils, Classes;

type
  TArrayOfByte = array of Byte;

function BinToHexString(Source: AnsiString): string;
function IntToBin(Data: Cardinal; Count: Byte): string;
function TryHexToInt(Data: string; var Value: Integer): Boolean;
function TryBinToInt(Data: string; var Value: Integer): Boolean;
{$IFDEF Windows}
function GetSpecialFolderPath(Folder: Integer): string;
{$ENDIF}
function BCDToInt(Value: Byte): Byte;
function CompareByteArray(Data1, Data2: TArrayOfByte): Boolean;
function RightCutString(var Source, Output: string; Delimiter: string; Allowed: string = ''): Boolean;
function LeftCutString(var Source, Output: string; Delimiter: string; Allowed: string = ''): Boolean;

implementation

function BinToHexString(Source: AnsiString): string;
var
  I: Integer;
begin
  for I := 1 to Length(Source) do begin
    Result := Result + LowerCase(IntToHex(Ord(Source[I]), 2));
  end;
end;

function BCDToInt(Value: Byte): Byte;
begin
  Result := (Value shr 4) * 10 + (Value and 15);
end;

{$IFDEF Windows}
function GetSpecialFolderPath(Folder: Integer): string;
const
  SHGFP_TYPE_CURRENT = 0;
var
  Path: array[0..MAX_PATH] of Char;
begin
  if SUCCEEDED(SHGetFolderPath(0, Folder, 0, SHGFP_TYPE_CURRENT, @path[0])) then
    Result := path
  else
    Result := '';
end;
{$ENDIF}

function IntToBin(Data: Cardinal; Count: Byte): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
    Result := IntToStr((Data shr I) and 1) + Result;
end;

function IntToHex(Data: Cardinal; Count: Byte): string;
const
  Chars: array[0..15] of Char = '0123456789ABCDEF';
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
    Result := Result + Chars[(Data shr (I * 4)) and 15];
end;

function TryHexToInt(Data: string; var Value: Integer): Boolean;
var
  I: Integer;
begin
  Data := UpperCase(Data);
  Result := True;
  Value := 0;
  for I := 0 to Length(Data) - 1 do begin
    if (Data[I + 1] >= '0') and (Data[I + 1] <= '9') then
      Value := Value or (Ord(Data[I + 1]) - Ord('0')) shl ((Length(Data) - I - 1) * 4)
    else if (Data[I + 1] >= 'A') and (Data[I + 1] <= 'F') then
      Value := Value or (Ord(Data[I + 1]) - Ord('A') + 10) shl ((Length(Data) - I - 1) * 4)
    else Result := False;
  end;
end;

function TryBinToInt(Data: string; var Value: Integer): Boolean;
var
  I: Integer;
begin
  Result := True;
  Value := 0;
  for I := 0 to Length(Data) - 1 do begin
    if (Data[I + 1] >= '0') and (Data[I + 1] <= '1') then
      Value := Value or (Ord(Data[I + 1]) - Ord('0')) shl ((Length(Data) - I - 1))
    else Result := False;
  end;
end;

function CompareByteArray(Data1, Data2: TArrayOfByte): Boolean;
var
  I: Integer;
begin
  if Length(Data1) = Length(Data2) then begin
    Result := True;
    for I := 0 to Length(Data1) - 1 do begin
      if Data1[I] <> Data2[I] then begin
        Result := False;
        Break;
      end
    end;
  end else Result := False;
end;

function LeftCutString(var Source, Output: string; Delimiter: string; Allowed: string = ''): Boolean;
var
  I, J: Integer;
  Matched: Boolean;
begin
  I := 1;
  Matched := True;
  while (I < Length(Source)) and Matched do begin
    Matched := False;
    if (Source[I] >= 'A') and (Source[I] <= 'Z') then Matched := True;
    if (Source[I] >= 'a') and (Source[I] <= 'z') then Matched := True;
    if (Source[I] >= '0') and (Source[I] <= '9') then Matched := True;
    for J := 1 to Length(Allowed) do
      if Source[I] = Allowed[J] then Matched := True;
    if Matched then Inc(I);
  end;
  if (Delimiter = Copy(Source, I, Length(Delimiter))) or (I = Length(Source)) then begin
    Output := Copy(Source, 1, I-1);
    Delete(Source, 1, Length(Output) + Length(Delimiter));
    Result := True;
  end else begin
    Output := '';
    Result := False;
  end;
end;

function RightCutString(var Source, Output: string; Delimiter: string; Allowed: string = ''): Boolean;
var
  I, J: Integer;
  Matched: Boolean;
begin
  I := Length(Source);
  Matched := True;
  while (I > 0) and Matched do begin
    Matched := False;
    if (Source[I] >= 'A') and (Source[I] <= 'Z') then Matched := True;
    if (Source[I] >= 'a') and (Source[I] <= 'z') then Matched := True;
    if (Source[I] >= '0') and (Source[I] <= '9') then Matched := True;
    for J := 1 to Length(Allowed) do
      if Source[I] = Allowed[J] then Matched := True;
    if Matched then Dec(I);
  end;
  if (Delimiter = Copy(Source, I - Length(Delimiter) + 1, Length(Delimiter))) or (I = 0) then begin
    Output := Copy(Source, I + 1, Length(Source) - I);
    Delete(Source, I, Length(Output) + Length(Delimiter));
    Result := True;
  end else begin
    Output := '';
    Result := False;
  end;
end;


end.
