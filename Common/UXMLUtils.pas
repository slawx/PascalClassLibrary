unit UXMLUtils;

{$mode delphi}

interface

uses
  {$IFDEF WINDOWS}Windows,{$ENDIF}
  Classes, SysUtils, DateUtils;

function XMLTimeToDateTime(XMLDateTime: string): TDateTime;
function DateTimeToXMLTime(Value: TDateTime; ApplyLocalBias: Boolean = True): WideString;


implementation

function GetTimeZoneBias: Integer;
{$IFDEF WINDOWS}
var
  TimeZoneInfo: TTimeZoneInformation;
begin
  case GetTimeZoneInformation(TimeZoneInfo) of
  TIME_ZONE_ID_STANDARD: Result := TimeZoneInfo.Bias + TimeZoneInfo.StandardBias;
  TIME_ZONE_ID_DAYLIGHT: Result := TimeZoneInfo.Bias + TimeZoneInfo.DaylightBias;
  else
    Result := 0;
  end;
end;
{$ELSE}
begin
  Result := 0;
end;
{$ENDIF}

function LeftCutString(var Source: string; out Output: string; Delimiter: string; Allowed: string = ''): Boolean;
var
  I, J: Integer;
  Matched: Boolean;
begin
  I := 1;
  Matched := True;
  while (I < Length(Source)) and Matched do begin
    Matched := True;
    if (Source[I] = Delimiter) then Matched := False;
    //for J := 1 to Length(Allowed) do
    //  if Source[I] = Allowed[J] then Matched := True;
    if Matched then Inc(I);
  end;
  if (Delimiter = Copy(Source, I, Length(Delimiter))) or (I = Length(Source)) then begin
    Output := Copy(Source, 1, I - 1);
    Delete(Source, 1, Length(Output) + Length(Delimiter));
    Result := True;
  end else begin
    Output := '';
    Result := False;
  end;
end;

function XMLTimeToDateTime(XMLDateTime: string): TDateTime;
var
  Part: string;
  Year: Integer;
  Month: Integer;
  Day: Integer;
  Hour: Integer;
  Minute: Integer;
  Second: Integer;
  Millisecond: Integer;
begin
  if LeftCutString(XMLDateTime, Part, '-') then
    Year := StrToInt(Part);
  if LeftCutString(XMLDateTime, Part, '-') then
    Month := StrToInt(Part);
  if Pos('T', XMLDateTime) > 0 then begin
    if LeftCutString(XMLDateTime, Part, 'T') then
      Day := StrToInt(Part);
    if LeftCutString(XMLDateTime, Part, ':') then
      Hour := StrToInt(Part);
    if LeftCutString(XMLDateTime, Part, ':') then
      Minute := StrToInt(Part);
    if Pos('.', XMLDateTime) > 0 then begin
      if LeftCutString(XMLDateTime, Part, '.') then
        Second := StrToInt(Part);
      if Pos('+', XMLDateTime) > 0 then
        LeftCutString(XMLDateTime, Part, '+') else
      if Pos('-', XMLDateTime) > 0 then
        LeftCutString(XMLDateTime, Part, '-') else
      if Pos('Z', XMLDateTime) > 0 then
        LeftCutString(XMLDateTime, Part, 'Z');
      Millisecond := StrToInt(Part);
    end else begin
      if Pos('+', XMLDateTime) > 0 then
        LeftCutString(XMLDateTime, Part, '+') else
      if Pos('-', XMLDateTime) > 0 then
        LeftCutString(XMLDateTime, Part, '-') else
      if Pos('Z', XMLDateTime) > 0 then
        LeftCutString(XMLDateTime, Part, 'Z');
      Second := StrToInt(Part);
      Millisecond := 0;
    end;
  end else begin
    Day := StrToInt(XMLDateTime);
  end;
  Result := EncodeDateTime(Year, Month, Day, Hour, Minute, Second, Millisecond);
  // TODO: Correct time by zone bias
end;

function DateTimeToXMLTime(Value: TDateTime; ApplyLocalBias: Boolean = True): WideString;
const
  Neg: array[Boolean] of string =  ('+', '-');
var
  Bias: Integer;
begin
  Result := FormatDateTime('yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz', Value); { Do not localize }
  Bias := GetTimeZoneBias;
  if (Bias <> 0) and ApplyLocalBias then
  begin
    Result := Format('%s%s%.2d:%.2d', [Result, Neg[Bias > 0],                         { Do not localize }
                                       Abs(Bias) div MinsPerHour,
                                       Abs(Bias) mod MinsPerHour]);
  end else
    Result := Result + 'Z'; { Do not localize }
end;

end.

