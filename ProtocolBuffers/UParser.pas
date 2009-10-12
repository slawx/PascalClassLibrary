unit UParser;

{$mode delphi}{$H+}

interface

uses
  Classes, DateUtils;

type

  { TParser }

  TParser = class
    WhiteSpaces: string;
    Text: string;
    Index: Integer;
    function CheckNext(Text: string; Shift: Boolean = False): Boolean;
    constructor Create;
  end;

implementation

{ TParser }

function TParser.CheckNext(Text: string; Shift: Boolean): Boolean;
begin

end;

constructor TParser.Create;
begin
  WhiteSpaces := ' '#9#10#13;
end;

end.
