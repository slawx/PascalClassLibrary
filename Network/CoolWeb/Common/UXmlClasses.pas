unit UXmlClasses;

{$mode delphi}{$H+}

interface

uses Classes, SysUtils, StrUtils, SpecializedList, SpecializedObjectList,
  SpecializedDictionary;

type
  TXmlElement = class
  private
    function GetAsString: string; virtual;
  public
    property AsString: string read GetAsString;
  end;

  TXmlString = class(TXmlElement)
  private
    function GetAsString: string; override;
  public
    Text: string;
  end;

  TXmlTag = class(TXmlElement)
  private
    function GetAsString: string; override;
  public
    EndTagSymbol: string;
    ShringEmpty: Boolean;
    Name: string;
    Attributes: TDictionaryStringString;
    SubElements: TListObject; // TListObject<TXmlElement>;
    constructor Create;
    destructor Destroy; override;
  end;

  TXmlDocument = class
  private
    MainTag: TXmlTag;
    function FormatStructure(Text: string): string;
    function GetAsString: string;
  public
    Formated: Boolean;
    Content: TXmlTag;
    XmlVersion: string;
    Encoding: string;
    constructor Create;
    destructor Destroy; override;
    property AsString: string read GetAsString;
  end;

implementation

{ THtmlElement }

constructor TXmlTag.Create;
begin
  ShringEmpty := True;
  Attributes := TDictionaryStringString.Create;
  SubElements := TListObject.Create;
  EndTagSymbol := '/';
end;

destructor TXmlTag.Destroy;
begin
  Attributes.Free;
  SubElements.Free;
  inherited;
end;

function TXmlTag.GetAsString: string;
var
  AttributesText: string;
  I: Integer;
  Content: string;
begin
  Content := '';
  for I := 0 to SubElements.Count - 1 do
    Content := Content + TXmlElement(SubElements[I]).AsString;

  AttributesText := '';
  for I := 0 to Attributes.Count - 1 do
    AttributesText := AttributesText + ' ' + Attributes.Keys[I] + '="' + Attributes[I].Value + '"';

  if Name <> '' then begin
    if (Content <> '') or not ShringEmpty then
      Result :=  '<' + Name + AttributesText + '>' + Content + '<' + EndTagSymbol + Name + '>'
      else Result :=  '<' + Name + AttributesText + EndTagSymbol + '>';
  end else Result := Content;
end;

{ TXmlString }

function TXmlString.GetAsString: string;
begin
  Result := Text;
end;

{ TXmlElement }

function TXmlElement.GetAsString: string;
begin
  Result := ''; // dodelat
end;

{ TXmlDocument }

constructor TXmlDocument.Create;
begin
  inherited;
  Encoding := 'utf-8';
  XmlVersion := '1.0';
  MainTag := TXmlTag.Create;
  with MainTag do begin
    Name := '?xml';
    EndTagSymbol := '?';
    Attributes.Add('version', '1.0');
    Attributes.Add('encoding', 'utf-8');
  end;
  Content := TXmlTag.Create;
end;

destructor TXmlDocument.Destroy;
begin
  Content.Free;
  MainTag.Free;
  inherited;
end;

function TXmlDocument.FormatStructure(Text: string): string;
const
  NewLine = #13#10;
var
  IndentCount: Integer;
  I: Integer;
  LastPos: Integer;
  Content: string;
begin
  IndentCount := 0;
  Result := '';
  LastPos := 1;
  I := 1;
  while I < Length(Text) do begin
    if Text[I] = '<' then begin
      Content := Trim(Copy(Text, LastPos, I - LastPos));
      if Length(Content) > 0 then
        Result := Result + DupeString('  ', IndentCount) + Content + NewLine;
      LastPos := I;
    end;
    if Text[I] = '>' then begin
      if Text[LastPos + 1] = '/' then Dec(IndentCount);
      Result := Result + DupeString('  ', IndentCount) + Copy(Text, LastPos, I - LastPos + 1)
        + NewLine;
      if (Text[LastPos + 1] <> '/') and (Text[I - 1] <> '/') and
        (Text[I - 1] <> '?') and (Text[LastPos + 1] <> '!') then Inc(IndentCount);
      LastPos := I + 1;
    end;
    Inc(I);
  end;
  if Text[LastPos + 1] = '/' then Dec(IndentCount);
  Result := Result + DupeString('  ', IndentCount) + Copy(Text, LastPos, I - LastPos + 1)
    + NewLine;
end;

function TXmlDocument.GetAsString: string;
begin
  Result := MainTag.AsString + Content.AsString;
  if Formated then Result := FormatStructure(Result);
end;

end.