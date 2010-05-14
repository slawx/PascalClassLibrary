unit UStringListEx;
  
// Date: 2010-04-14

interface
  
type
  
  { TStringListEx }

  TStringListEx = class(TStringList)
    procedure Explode(Separator, Text: string; SlicesCount: Integer = -1);
    function Implode(Separator: string): string;
    procedure AddNameValue(Name, Value: string);
  end;
  
implementation
  
{ TStringListEx }

procedure TStringListEx.Explode(Separator, Text: string; SlicesCount: Integer = -1);
begin
  Clear;
  while (Pos(Separator, Text) > 0) and
  ((Count < (SlicesCount - 1)) or (SlicesCount = -1)) do begin
    Add(Copy(Text, 1, Pos(Separator, Text) - 1));
    System.Delete(Text, 1, Pos(Separator, Text) + Length(Separator) - 1);
  end;
  Add(Text);
end;

function TStringListEx.Implode(Separator: string): string;
var
  I: Integer;
begin
  for I := 0 to Count - 2 do
    Result := Result + Strings[I] + Separator;
  if Count > 0 then
    Result := Result + Strings[Count - 1];
end;

procedure TStringListEx.AddNameValue(Name, Value: string);
begin
  Add(Name + NameValueSeparator + Value); 
end;

end.
