unit UHTMLClasses;

{$mode Delphi}{$H+}

interface

uses
  UXmlClasses, Classes, SysUtils, UCommon, UNetworkAddress;

type
  TURL = class(TPersistent)
  private
    function GetAsString: string;
    procedure SetAsString(Value: string);
  public
    Scheme: string;
    UserName: string;
    Password: string;
    Host: THostAddress;
    Port: Word;
    Path: string;
    Query: string;
    Fragment: string;
    constructor Create;
    destructor Destroy; override;
    property AsString: string read GetAsString write SetAsString;
  end;

  THtmlElement = class
  private
    function GetAsXmlElement: TXmlElement; virtual;
  public
    Id: string;
    Name: string;
    ClassId: string;
    Style: string;
    property AsXmlElement: TXmlElement read GetAsXmlElement;
  end;

  TBlockType = (btNoTag, btBlockLevel, btInline);

  THtmlString = class(THtmlElement)
  private
    function GetAsXmlElement: TXmlElement; override;
  public
    Text: string;
  end;

  THtmlBlock = class(THtmlElement)
  private
    function GetAsXmlElement: TXmlElement; override;
  public
    BlockType: TBlockType;
    SubItems: TList; // of THtmlElement;
    constructor Create;
    destructor Destroy; override;
  end;

  THtmlLink = class(THtmlElement)
  private
    function GetAsXmlElement: TXmlElement; override;
  public
    Target: TURL;
    Content: THtmlElement;
    constructor Create;
    destructor Destroy; override;
  end;

  TSizeUnits = (suPixels, suPercents);
  THtmlSize = record
    Width: Integer;
    Height: Integer;
    Units: TSizeUnits;
  end;

  THtmlImage = class(THtmlElement)
  private
    function GetAsXmlElement: TXmlElement; override;
  public
    Size: THtmlSize;
    Source: TURL;
    AlternateText: string;
    constructor Create;
    destructor Destroy; override;
  end;

  THtmlPage = class
  private
    function GetAsXmlDocument: TXmlDocument;
  public
    Title: string;
    Charset: string;
    Body: THtmlBlock;
    property AsXmlDocument: TXmlDocument read GetAsXmlDocument;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ THtmlPage }

constructor THtmlPage.Create;
begin
  Body := THtmlBlock.Create;
end;

destructor THtmlPage.Destroy;
begin
  Body.Free;
  inherited;
end;

function THtmlPage.GetAsXmlDocument: TXmlDocument;
begin
  Result := TXmlDocument.Create;
  with Result, Content do begin
    Formated := True;
    TagName := 'html';
    with TXmlTag(SubElements[SubElements.Add(TXmlTag.Create)]) do begin
      TagName := 'head';
      with TXmlTag(SubElements[SubElements.Add(TXmlTag.Create)]) do begin
        TagName := 'title';
        with TXmlString(SubElements[SubElements.Add(TXmlString.Create)]) do begin
          Text := Title;
        end;
      end;
      with TXmlTag(SubElements[SubElements.Add(TXmlTag.Create)]) do begin
        TagName := 'meta';
        Attributes.AddNameValue('http-equiv', 'Content-Language');
        Attributes.AddNameValue('content', 'cs');
      end;
      with TXmlTag(SubElements[SubElements.Add(TXmlTag.Create)]) do begin
        TagName := 'meta';
        Attributes.AddNameValue('http-equiv', 'Content-Type');
        Attributes.AddNameValue('content', 'text/html; charset=' + Charset);
      end;
    end;
    with TXmlTag(SubElements[SubElements.Add(TXmlTag.Create)]) do begin
      TagName := 'body';
      SubElements.Add(Body.AsXmlElement);


    end;
  end;
end;

{ THtmlBlock }

constructor THtmlBlock.Create;
begin
  SubItems := TList.Create;
end;

destructor THtmlBlock.Destroy;
var
  I: Integer;
begin
  for I := 0 to SubItems.Count - 1 do THtmlElement(SubItems[I]).Free;
  SubItems.Free;
  inherited;
end;

function THtmlBlock.GetAsXmlElement: TXmlElement;
var
  I: Integer;
begin
  Result := TXmlTag.Create;
  with TXmlTag(Result) do begin
    case BlockType of
      btBlockLevel: TagName := 'div';
      btInline: TagName := 'span';
      btNoTag: TagName := '';
    end;
    for I := 0 to SubItems.Count - 1 do
      SubElements.Add(THtmlElement(SubItems[I]).AsXmlElement);
   end;
end;

{ THtmlElement }

function THtmlElement.GetAsXmlElement: TXmlElement;
begin

end;

constructor TURL.Create;
begin
  Host := THostAddress.Create;
end;

destructor TURL.Destroy;
begin
  Host.Free;
  inherited;
end;

function TURL.GetAsString: string;
begin
  Result := '';
  if Scheme <> '' then Result := Scheme + '://';
  if UserName <> '' then begin
    Result := Result + UserName;
    if UserName <> '' then Result := Result + ':' + Password;
    Result := Result + '@';
  end;
  if Host.AsString <> '' then Result := Result + Host.AsString;
  if Port <> 0 then Result := Result + ':' + IntToStr(Port);
  if Path <> '' then Result := Result + Path;
  if Query <> '' then Result := Result + '?' + Query;
  if Fragment <> '' then Result := Result + '#' + Fragment;
end;

procedure TURL.SetAsString(Value: string);
var
  HostAddr: string;
  HostPort: string;
begin
  LeftCutString(Value, Scheme, '://');
  if LeftCutString(Value, UserName, ':') then LeftCutString(Value, Password, '@')
    else LeftCutString(Value, UserName, '@');
  RightCutString(Value, Fragment, '#');
  RightCutString(Value, Query, '?', '=&');
  if LeftCutString(Value, HostAddr, ':', '.') then begin
    LeftCutString(Value, HostPort, '');
    Port := StrToInt(HostPort);
  end else LeftCutString(Value, HostAddr, '', '.');
  Host.AsString := HostAddr;
  LeftCutString(Value, Path, '', '/.');
end;

{ THtmlLink }

constructor THtmlLink.Create;
begin
  Target := TURL.Create;
end;

destructor THtmlLink.Destroy;
begin
  Target.Free;
  inherited;
end;

function THtmlLink.GetAsXmlElement: TXmlElement;
begin
  Result := TXmlTag.Create;
  with TXmlTag(Result) do begin
    TagName := 'a';
    Attributes.Add('href='+Target.AsString);
    if Assigned(Content) then SubElements.Add(Content.AsXmlElement);
  end;
end;

{ THtmlString }

function THtmlString.GetAsXmlElement: TXmlElement;
begin
  Result := TXmlString.Create;
  TXmlString(Result).Text := Text;
end;

{ THtmlImage }

constructor THtmlImage.Create;
begin
  Source := TURL.Create;
end;

destructor THtmlImage.Destroy;
begin
  Source.Free;
  inherited;
end;

function THtmlImage.GetAsXmlElement: TXmlElement;
begin
  Result := TXmlTag.Create;
  with TXmlTag(Result) do begin
    TagName := 'img';
    Attributes.AddNameValue('src', Source.AsString);
    Attributes.AddNameValue('width', IntToStr(Size.Width));
    Attributes.AddNameValue('height', IntToStr(Size.Height));
    Attributes.AddNameValue('alt', AlternateText);
  end;
end;

end.
