unit UCommon;

interface

uses
  Windows, SysUtils, ShFolder;

type
  TArrayOfByte = array of Byte;

function IntToBin(Data: Cardinal; Count: Byte): string;
function TryHexToInt(Data: string; var Value: Integer): Boolean;
function TryBinToInt(Data: string; var Value: Integer): Boolean;
function GetSpecialFolderPath(Folder: Integer): string;
function BCDToInt(Value: Byte): Byte;
function CompareByteArray(Data1, Data2: TArrayOfByte): Boolean;

implementation

function BCDToInt(Value: Byte): Byte;
begin
  Result := (Value shr 4) * 10 + (Value and 15);
end;

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


end.
