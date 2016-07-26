unit UXMLUtils;

{$mode delphi}

interface

uses
  {$IFDEF WINDOWS}Windows,{$ENDIF}
  Classes, SysUtils, DateUtils, XMLRead, XMLWrite, DOM;

function XMLTimeToDateTime(XMLDateTime: string): TDateTime;
function DateTimeToXMLTime(Value: TDateTime; ApplyLocalBias: Boolean = True): WideString;
procedure WriteInteger(Node: TDOMNode; Name: string; Value: Integer);
procedure WriteInt64(Node: TDOMNode; Name: string; Value: Int64);
procedure WriteBoolean(Node: TDOMNode; Name: string; Value: Boolean);
procedure WriteString(Node: TDOMNode; Name: string; Value: string);
procedure WriteDateTime(Node: TDOMNode; Name: string; Value: TDateTime);
function ReadInteger(Node: TDOMNode; Name: string; DefaultValue: Integer): Integer;
function ReadInt64(Node: TDOMNode; Name: string; DefaultValue: Int64): Int64;
function ReadBoolean(Node: TDOMNode; Name: string; DefaultValue: Boolean): Boolean;
function ReadString(Node: TDOMNode; Name: string; DefaultValue: string): string;
function ReadDateTime(Node: TDOMNode; Name: string; DefaultValue: TDateTime): TDateTime;


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
  SecondFraction: Double;
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
      SecondFraction := StrToFloat('0' + DecimalSeparator + Part);
      Millisecond := Trunc(SecondFraction * 1000);
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

procedure WriteInteger(Node: TDOMNode; Name: string; Value: Integer);
var
  NewNode: TDOMNode;
begin
  NewNode := Node.OwnerDocument.CreateElement(Name);
  NewNode.TextContent := IntToStr(Value);
  Node.AppendChild(NewNode);
end;

procedure WriteInt64(Node: TDOMNode; Name: string; Value: Int64);
var
  NewNode: TDOMNode;
begin
  NewNode := Node.OwnerDocument.CreateElement(Name);
  NewNode.TextContent := IntToStr(Value);
  Node.AppendChild(NewNode);
end;

procedure WriteBoolean(Node: TDOMNode; Name: string; Value: Boolean);
var
  NewNode: TDOMNode;
begin
  NewNode := Node.OwnerDocument.CreateElement(Name);
  NewNode.TextContent := BoolToStr(Value);
  Node.AppendChild(NewNode);
end;

procedure WriteString(Node: TDOMNode; Name: string; Value: string);
var
  NewNode: TDOMNode;
begin
  NewNode := Node.OwnerDocument.CreateElement(Name);
  NewNode.TextContent := Value;
  Node.AppendChild(NewNode);
end;

procedure WriteDateTime(Node: TDOMNode; Name: string; Value: TDateTime);
var
  NewNode: TDOMNode;
begin
  NewNode := Node.OwnerDocument.CreateElement(Name);
  NewNode.TextContent := DateTimeToXMLTime(Value);
  Node.AppendChild(NewNode);
end;

function ReadInteger(Node: TDOMNode; Name: string; DefaultValue: Integer): Integer;
var
  NewNode: TDOMNode;
begin
  Result := DefaultValue;
  NewNode := Node.FindNode(Name);
  if Assigned(NewNode) then
    Result := StrToInt(NewNode.TextContent);
end;

function ReadInt64(Node: TDOMNode; Name: string; DefaultValue: Int64): Int64;
var
  NewNode: TDOMNode;
begin
  Result := DefaultValue;
  NewNode := Node.FindNode(Name);
  if Assigned(NewNode) then
    Result := StrToInt64(NewNode.TextContent);
end;

function ReadBoolean(Node: TDOMNode; Name: string; DefaultValue: Boolean): Boolean;
var
  NewNode: TDOMNode;
begin
  Result := DefaultValue;
  NewNode := Node.FindNode(Name);
  if Assigned(NewNode) then
    Result := StrToBool(NewNode.TextContent);
end;

function ReadString(Node: TDOMNode; Name: string; DefaultValue: string): string;
var
  NewNode: TDOMNode;
begin
  Result := DefaultValue;
  NewNode := Node.FindNode(Name);
  if Assigned(NewNode) then
    Result := NewNode.TextContent;
end;

function ReadDateTime(Node: TDOMNode; Name: string; DefaultValue: TDateTime
  ): TDateTime;
var
  NewNode: TDOMNode;
begin
  Result := DefaultValue;
  NewNode := Node.FindNode(Name);
  if Assigned(NewNode) then
    Result := XMLTimeToDateTime(NewNode.TextContent);
end;

end.

