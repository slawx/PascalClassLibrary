unit UObjectType;

interface

uses
  SysUtils;

type
  TInteger = class
    Value: Integer;
    procedure Assign(Source: TObject);
  end;

  TString = class
    Text: string;
    procedure Assign(Source: TObject);
    function Length: Integer;
    procedure UpperCase;
    procedure LowerCase;
    procedure Delete(Index: Integer; Count: Integer);
    procedure Insert(Index: Integer; SubString: string);
    function Pos(SubString: string): Integer;
  end;

implementation

{ TInteger }

procedure TInteger.Assign(Source: TObject);
begin
  if Source is TString then begin

  end;
end;

{ TString }

procedure TString.Assign(Source: TObject);
begin

end;

procedure TString.Delete(Index, Count: Integer);
begin
  System.Delete(Text, Index, Count);
end;

procedure TString.Insert(Index: Integer; SubString: string);
begin
  System.Insert(SubString, Text, Index);
end;

function TString.Length: Integer;
begin
  Result := System.Length(Text);
end;

procedure TString.LowerCase;
begin
  Text := SysUtils.LowerCase(Text);
end;

function TString.Pos(SubString: string): Integer;
begin
  Result := System.Pos(SubString, Text);
end;

procedure TString.UpperCase;
begin
  Text := SysUtils.UpperCase(Text);
end;

end.
