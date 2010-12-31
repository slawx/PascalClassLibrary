unit UHtmlClasses;

{$mode delphi}{$H+}

interface

uses
  UXmlClasses, Classes, SysUtils, SpecializedList;

type
  TDomainAddress = class(TPersistent)
  private
    function GetAsString: string;
    procedure SetAsString(const Value: string);
  public
    Levels: array of string;
    property AsString: string read GetAsString write SetAsString;
  end;

  TAddrClass = (acA, acB, acC, acD, acE);

  TIpAddress = class(TPersistent)
  private
    function GetAddrClass: TAddrClass;
    function GetAsCardinal: Cardinal;
    function GetAsString: string;
    function GetBroadcast: Boolean;
    procedure SetBroadcast(const Value: Boolean);
    procedure SetAsCardinal(const Value: Cardinal);
    procedure SetAsString(const Value: string);
  public
    Octets: array[0..3] of Byte;
    procedure Assign(Source: TPersistent); override;
    property AsCardinal: Cardinal read GetAsCardinal write SetAsCardinal;
    property AsString: string read GetAsString write SetAsString;
    property AddrClass: TAddrClass read GetAddrClass;
    property Broadcast: Boolean read GetBroadcast write SetBroadcast;
  end;

  THostAddressState = (asDomainName, asIpAddress);
  THostAddress = class(TPersistent)
  private
    function GetAsString: string;
    procedure SetAsString(const Value: string);
  public
    State: THostAddressState;
    DomainName: TDomainAddress;
    IpAddress: TIpAddress;
    constructor Create;
    destructor Destroy; override;
    property AsString: string read GetAsString write SetAsString;
  end;

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

  { THtmlLineBreak }

  THtmlLineBreak = class(THtmlElement)
  private
    function GetAsXmlElement: TXmlElement; override;
  public
    constructor Create;
  end;

  THtmlBlock = class(THtmlElement)
  private
    function GetAsXmlElement: TXmlElement; override;
  public
    BlockType: TBlockType;
    SubItems: TListObject; // TListObject<THtmlElement>;
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

  THtmlInputType = (itText, itComboBox, itRadioButton, itReset, itPassword,
    itSubmit, itHidden, itFileSelect, itButton, itCheckBox);

  { THtmlInput }

  THtmlInput = class(THtmlElement)
  private
    function GetAsXmlElement: TXmlElement; override;
  public
    InputType: THtmlInputType;
    Value: Variant;
    constructor Create;
    destructor Destroy; override;
  end;

  { THtmlForm }

  THtmlForm = class(THtmlBlock)
  private
  public
    Method: string;
    Action: TURL;
    function GetAsXmlElement: TXmlElement; override;
    constructor Create;
    destructor Destroy; override;
  end;

  THtmlDocument = class
  private
    function GetAsXmlDocument: TXmlDocument;
  public
    Title: string;
    ContentEncoding: string;
    ContentLanguage: string;
    Body: THtmlBlock;
    Styles: TStringList;
    Scripts: TStringList;
    property AsXmlDocument: TXmlDocument read GetAsXmlDocument;
    constructor Create;
    destructor Destroy; override;
  end;

  { TQueryString }

  TQueryString = class
    Data: TStringList;
    procedure SetStringServer;
    procedure SetString(QueryString: string);
    function GetString: string;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

resourcestring
  SStringToIPConversionError = 'String to IP address conversion error';


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
    Output := Copy(Source, I+1, Length(Source) - I);
    Delete(Source, I, Length(Output) + Length(Delimiter));
    Result := True;
  end else begin
    Output := '';
    Result := False;
  end;
end;

{ THtmlLineBreak }

function THtmlLineBreak.GetAsXmlElement: TXmlElement;
begin
  Result := inherited GetAsXmlElement;
  TXmlTag(Result).Name := 'br';
end;

constructor THtmlLineBreak.Create;
begin
end;

{ THtmlInput }

function THtmlInput.GetAsXmlElement: TXmlElement;
var
  InputTypeString: string;
begin
  Result := TXmlTag.Create;
  with TXmlTag(Result) do begin
    Name := 'input';
    case InputType of
      itButton: InputTypeString := 'button';
      itRadioButton: InputTypeString := 'radio';
      itCheckBox: InputTypeString := 'checkbox';
      itText: InputTypeString := 'text';
      itFileSelect: InputTypeString := 'file';
      itSubmit: InputTypeString := 'submit';
      itHidden: InputTypeString := 'hidden';
      itPassword: InputTypeString := 'password';
    end;
    Attributes.Add('type', InputTypeString);
    Attributes.Add('value', Value);
    Attributes.Add('name', Name);
  end;
end;

constructor THtmlInput.Create;
begin

end;

destructor THtmlInput.Destroy;
begin
  inherited Destroy;
end;

{ THtmlForm }

function THtmlForm.GetAsXmlElement: TXmlElement;
begin
  Result := TXmlTag.Create;
  with TXmlTag(Result) do begin
    Name := 'form';
    Attributes.Add('action', Action.AsString);
    Attributes.Add('method', Method);
  end;
end;

constructor THtmlForm.Create;
begin
  inherited;
  Action := TURL.Create;
  BlockType := btBlockLevel;
  Method := 'get';
end;

destructor THtmlForm.Destroy;
begin
  Action.Free;
  inherited Destroy;
end;

{ THtmlDocument }

constructor THtmlDocument.Create;
begin
  Body := THtmlBlock.Create;
  Styles := TStringList.Create;
  Scripts := TStringList.Create;
  ContentLanguage := 'en';
  ContentEncoding := 'utf-8';
end;

destructor THtmlDocument.Destroy;
begin
  Body.Free;
  Styles.Free;
  Scripts.Free;
  inherited;
end;

function THtmlDocument.GetAsXmlDocument: TXmlDocument;
var
  DocType: TXMLTag;
  HTMLTag: TXMLTag;
  I: Integer;
begin
  Result := TXmlDocument.Create;
  with Result, Content do begin
    DocType := TXMlTag.Create;
    DocType.Name := '!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd"';
    Doctype.EndTagSymbol := '';
    SubElements.Add(DocType);
    HTMLTag := TXMLTag.Create;
    with HTMLTag do begin
      Name := 'html';
      with TXmlTag(SubElements[SubElements.Add(TXmlTag.Create)]) do begin
        Name := 'head';
        with TXmlTag(SubElements[SubElements.Add(TXmlTag.Create)]) do begin
          Name := 'title';
          with TXmlString(SubElements[SubElements.Add(TXmlString.Create)]) do begin
            Text := Title;
          end;
        end;
        with TXmlTag(SubElements[SubElements.Add(TXmlTag.Create)]) do begin
          Name := 'meta';
          Attributes.Add('http-equiv', 'Content-Language');
          Attributes.Add('content', ContentLanguage);
        end;
        with TXmlTag(SubElements[SubElements.Add(TXmlTag.Create)]) do begin
          Name := 'meta';
          Attributes.Add('http-equiv', 'Content-Type');
          Attributes.Add('content', 'text/html; charset=' + ContentEncoding);
        end;
        for I := 0 to Styles.Count - 1 do
        with TXmlTag(SubElements[SubElements.Add(TXmlTag.Create)]) do begin
          Name := 'link';
          Attributes.Add('rel', 'stylesheet');
          Attributes.Add('href', Styles[I]);
          Attributes.Add('type', 'text/css');
          Attributes.Add('media', 'all');
        end;
        for I := 0 to Scripts.Count - 1 do
        with TXmlTag(SubElements[SubElements.Add(TXmlTag.Create)]) do begin
          Name := 'script';
          ShringEmpty := False;
          Attributes.Add('type', 'text/javascript');
          Attributes.Add('src', Scripts[I]);
        end;
      end;
      with TXmlTag(SubElements[SubElements.Add(TXmlTag.Create)]) do begin
        Name := 'body';
        SubElements.Add(Body.AsXmlElement);
      end;
    end;
    SubElements.Add(HTMLTag);
  end;
end;

{ THtmlBlock }

constructor THtmlBlock.Create;
begin
  inherited;
  SubItems := TListObject.Create;
end;

destructor THtmlBlock.Destroy;
begin
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
      btBlockLevel: Name := 'div';
      btInline: Name := 'span';
      btNoTag: Name := '';
    end;
    for I := 0 to SubItems.Count - 1 do
      SubElements.Add(THtmlElement(SubItems[I]).AsXmlElement);
   end;
end;

{ THtmlElement }

function THtmlElement.GetAsXmlElement: TXmlElement;
begin
  Result := TXmlTag.Create;
  with TXmlTag(Result).Attributes do begin
    if Name <> '' then Add('name', Name);
    if Style <> '' then Add('style', Style);
    if ClassId <> '' then Add('class', ClassId);
    if Id <> '' then Add('id', Id);
  end;
end;

{ TIpAddress }

procedure TIpAddress.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Assigned(Source) then begin
    if Source is TIpAddress then begin
      for I := 0 to High(Octets) do
        Octets[I] := TIpAddress(Source).Octets[I];
    end else inherited;
  end else inherited;
end;

function TIpAddress.GetAddrClass: TAddrClass;
begin
  if (Octets[3] and $80) = 0 then Result := acA
  else begin
    if (Octets[3] and $40) = 0 then Result := acB
    else begin
      if (Octets[3] and $20) = 0 then Result := acC
      else begin
        if (Octets[3] and $10) = 0 then Result := acD
        else Result := acE;
      end;
    end;
  end;
end;

function TIpAddress.GetAsCardinal: Cardinal;
begin
  Result := Octets[0] or (Octets[1] shl 8) or (Octets[2] shl 16) or (Octets[3] shl 24);
end;

function TIpAddress.GetAsString: string;
begin
  Result := IntToStr(Octets[3]) + '.' + IntToStr(Octets[2]) + '.' +
    IntToStr(Octets[1]) + '.' + IntToStr(Octets[0]);
end;

function TIpAddress.GetBroadcast: Boolean;
begin
  Result := AsCardinal = High(Cardinal);
end;

procedure TIpAddress.SetAsCardinal(const Value: Cardinal);
begin
  Octets[0] := Byte(Value);
  Octets[1] := Byte(Value shr 8);
  Octets[2] := Byte(Value shr 16);
  Octets[3] := Byte(Value shr 24);
end;

procedure TIpAddress.SetAsString(const Value: string);
var
  Parts: TListString;
begin
  try
    Parts := TListString.Create;
    Parts.Explode(Value, '.', StrToStr);
    try
//    if Length(Parts) = 4 then begin
      Octets[0] := StrToInt(Parts[3]);
      Octets[1] := StrToInt(Parts[2]);
      Octets[2] := StrToInt(Parts[1]);
      Octets[3] := StrToInt(Parts[0]);
//    end else raise EConvertError.Create('String to IP address conversion error');
    except
      raise EConvertError.Create(SStringToIPConversionError);
    end;
  finally
    Parts.Free;
  end;
end;

procedure TIpAddress.SetBroadcast(const Value: Boolean);
begin
  AsCardinal := High(Cardinal);
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


{ TDomainAddress }

function TDomainAddress.GetAsString: string;
var
  I: Integer;
begin
  Result := '';
  for I := High(Levels) downto 0 do Result := Result + '.' + Levels[I];
  Delete(Result, 1, 1);
end;

procedure TDomainAddress.SetAsString(const Value: string);
var
  StrArray: TListString;
  I: Integer;
begin
  try
    StrArray := TListString.Create;
    StrArray.Explode(Value, '.', StrToStr);
    SetLength(Levels, StrArray.Count);
    for I := 0 to StrArray.Count - 1 do
      Levels[StrArray.Count - 1 - I] := StrArray[I];
  finally
    StrArray.Free;
  end;
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
    Name := 'a';
    Attributes.Add('href', Target.AsString);
    if Assigned(Content) then SubElements.Add(Content.AsXmlElement);
  end;
end;

{ THtmlString }

function THtmlString.GetAsXmlElement: TXmlElement;
begin
  Result := TXmlString.Create;
  TXmlString(Result).Text := Text;
end;

{ THostAddress }

constructor THostAddress.Create;
begin
  DomainName := TDomainAddress.Create;
  IpAddress := TIpAddress.Create;
  State := asDomainName;
  DomainName.AsString := 'localhost';
end;

destructor THostAddress.Destroy;
begin
  DomainName.Free;
  IpAddress.Free;
  inherited;
end;

function THostAddress.GetAsString: string;
begin
  case State of
    asDomainName: Result := DomainName.AsString;
    asIpAddress: Result := IpAddress.AsString;
  end;
end;

procedure THostAddress.SetAsString(const Value: string);
begin
  State := asIpAddress;
  try
    IpAddress.AsString := Value;
  except
    on EConvertError do State := asDomainName;
  end;
  if State = asDomainName then DomainName.AsString := Value;
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
    Name := 'img';
    Attributes.Add('src', Source.AsString);
    Attributes.Add('width', IntToStr(Size.Width));
    Attributes.Add('height', IntToStr(Size.Height));
    Attributes.Add('alt', AlternateText);
  end;
end;

procedure TQueryString.SetStringServer;
begin
  //$this->SetString($_SERVER['QUERY_STRING']);
end;

procedure TQueryString.SetString(QueryString: string);
begin
  (*
  $this->Data = array();
  $Parts = explode('&', $QueryString);
  foreach($Parts as $Part)
  {
    if($Part != '')
    {
      $Item = explode('=', $Part);
      $this->Data[$Item[0]] = $Item[1];
    end;
  end;*)
end;

function TQueryString.GetString: string;
begin
  (*$Parts = array();
  foreach($this->Data as $Index => $Item)
  {
    $Parts[] = $Index.'='.$Item;
  }
  return(implode('&', $Parts));*)
end;

constructor TQueryString.Create;
begin
  Data := TStringList.Create;
end;

destructor TQueryString.Destroy;
begin
  Data.Free;
  inherited Destroy;
end;

end.
