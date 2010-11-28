unit UHTTPServer;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, UTCPServer, UCommon, UMemoryStreamEx, UMIMEType,
  Synautil, SpecializedObjectList, SpecializedList;

type
  THTTPServer = class;
  EEmptyHTTPHandler = class(Exception);

  TQueryParameterList = class(TStringList)
    procedure Parse(Text: string);
    function Syntetize: string;
  end;

  { TCookieList }

  TCookieList = class(TStringList)
    procedure Parse(Text: string);
    function Syntetize: string;
  end;

  { THTTPRequest }

  THTTPRequest = class
    Query: TQueryParameterList;
    QueryParts: TListString;
    Path: string;
    Method: string;
    Headers: TStringList;
    Cookies: TCookieList;
    constructor Create;
    destructor Destroy; override;
  end;

  { THTTPResponse }

  THTTPResponse = class
    ContentType: string;
    Stream: TMemoryStreamEx;
    Headers: TStringList;
    Cookies: TCookieList;
    constructor Create;
    destructor Destroy; override;
  end;

  { THTTPHandlerData }

  THTTPHandlerData = class
    Server: THTTPServer;
    Request: THTTPRequest;
    Response: THTTPResponse;
    SessionId: string;
    Session: TStringList;
    constructor Create;
    destructor Destroy; override;
  end;

  TRequestEvent = procedure(HandlerData: THTTPHandlerData) of object;

  TRequestHandler = class
    Name: string;
    Handler: TRequestEvent;
  end;

  { TRequestHandlerList }

  TRequestHandlerList = class(TListObject)
    procedure Add(AName: string; AHandler: TRequestEvent);
    function IndexOfName(AName: string): TRequestHandler;
  end;

  { THTTPSessionStorage }

  THTTPSessionStorage = class
    procedure Load(HandlerData: THTTPHandlerData); virtual;
    procedure Save(HandlerData: THTTPHandlerData); virtual;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  { THTTPServer }

  THTTPServer = class
  private
    FOnRequest: TRequestEvent;
    FShowExceptions: Boolean;
    procedure SetShowExceptions(const AValue: Boolean);
  public
    Name: string;
    DocumentRoot: string;
    SessionStorage: THTTPSessionStorage;
    ShowExceptions: Boolean;
    procedure Run; virtual;
    procedure ErrorResponse(HandlerData: THTTPHandlerData);
    procedure FileResponse(HandlerData: THTTPHandlerData);
    procedure ServerInfo(HandlerData: THTTPHandlerData); virtual;
    constructor Create;
    destructor Destroy; override;
    property OnRequest: TRequestEvent read FOnRequest write FOnRequest;
    property ShowExceptions: Boolean read FShowExceptions write SetShowExceptions;
  end;

procedure HTTPExceptionShow(Obj: TObject; Addr: Pointer; FrameCount: Longint; Frames: PPointer);
procedure HTTPExceptionHide(Obj: TObject; Addr: Pointer; FrameCount: Longint; Frames: PPointer);


resourcestring
  SEmptyHTTPHandler = 'No handler defined for HTTP server.';
  SFileNotFound = 'File %s not found.';

implementation

{ THTTPServer }

procedure THTTPServer.ServerInfo(HandlerData: THTTPHandlerData);
var
  I: Integer;
begin
  with HandlerData, Response.Stream do begin
    //Response.Cookies.Values['Test'] := 'Halo';
    //Response.Cookies.Values['Test2'] := 'Halo2';

    //HTTPServer.SessionHandler.Variables.Values['Session1'] := 'Value1';
    //HTTPServer.SessionHandler.Variables.Values['Session2'] := 'Value2';

    WriteString('<a href="?ServerInfo">Refresh</a>');

    WriteString('<h5>Request HTTP headers</h5>');
    for I := 0 to Request.Headers.Count - 1 do begin;
      WriteString(Request.Headers.Strings[I] + '<br/>');
    end;

    WriteString('<h5>Request HTTP cookies</h5>');
    for I := 0 to Request.Cookies.Count - 1 do begin;
      WriteString(Request.Cookies.Strings[I] + '<br/>');
    end;

    //WriteString('Session id: ' + SessionId);
    WriteString('<h5>Session variables</h5>');
    for I := 0 to Session.Count - 1 do begin;
      WriteString(Session.Strings[I] + '<br/>');
    end;

    WriteString('<h5>Response HTTP headers</h5>');
    with Response.Stream do
    for I := 0 to Response.Headers.Count - 1 do begin;
      WriteString(Response.Headers.Strings[I] + '<br/>');
    end;

    WriteString('<h5>Response HTTP cookies</h5>');
    for I := 0 to Response.Cookies.Count - 1 do begin;
      WriteString(Response.Cookies.Strings[I] + '<br/>');
    end;
  end;
end;

procedure THTTPServer.ErrorResponse(HandlerData: THTTPHandlerData);
begin
  with HandlerData, Response.Stream do begin
    WriteString('<html><body>Page ' + Request.Path + ' not found.</body></html>');
  end;
end;

procedure THTTPServer.SetShowExceptions(const AValue: Boolean);
begin
  FShowExceptions := AValue;
  if AValue then ExceptProc := HTTPExceptionShow
    else ExceptProc := HTTPExceptionHide;
end;

procedure THTTPServer.Run;
begin

end;

procedure THTTPServer.FileResponse(HandlerData: THTTPHandlerData);
var
  BinaryFile: TFileStream;
  FileName: string;
begin
  with HandlerData do begin
    FileName := DocumentRoot + Request.Path;
    if FileExists(FileName) then begin
      Response.Headers.Values['Content-Type'] := GetMIMEType(Copy(ExtractFileExt(FileName), 2, 255));
      BinaryFile := TFileStream.Create(FileName, fmOpenRead);
      Response.Stream.WriteStream(BinaryFile, BinaryFile.Size);
      BinaryFile.Destroy;
    end else
    with Response.Stream do begin
      WriteLn(Format(SFileNotFound, [Request.Path]));
      WriteString('<html><body>File ' + Request.Path + ' not found.</body></html>');
    end;
  end;
end;

constructor THTTPServer.Create;
begin
  ShowExceptions := False;
  DocumentRoot := './';
  Name := 'THTTPServer';
end;

destructor THTTPServer.Destroy;
begin
  SessionStorage.Free;
  inherited Destroy;
end;

{ THTTPResponse }

constructor THTTPResponse.Create;
begin
  Stream := TMemoryStreamEx.Create;
  Cookies := TCookieList.Create;
  Headers := TStringList.Create;
end;

destructor THTTPResponse.Destroy;
begin
  Stream.Free;
  Headers.Free;
  Cookies.Free;
  inherited Destroy;
end;

{ TRequestHandlerList }

procedure TRequestHandlerList.Add(AName: string; AHandler: TRequestEvent);
begin
  with TRequestHandler(Items[inherited Add(TRequestHandler.Create)]) do begin
    Name := AName;
    Handler := AHandler;
  end;
end;

function TRequestHandlerList.IndexOfName(AName: string): TRequestHandler;
var
  I: Integer;
begin
  I := 0;
  while (I < Count) and (TRequestHandler(Items[I]).Name <> AName) do Inc(I);
  if I < Count then Result := TRequestHandler(Items[I]) else Result := nil;
end;

{ THTTPRequest }

constructor THTTPRequest.Create;
begin
  Query := TQueryParameterList.Create;
  QueryParts := TListString.Create;
  Headers := TStringList.Create;
  Cookies := TCookieList.Create;
end;

destructor THTTPRequest.Destroy;
begin
  Query.Free;
  QueryParts.Free;
  Headers.Free;
  Cookies.Free;
  inherited Destroy;
end;

{ TQueryParameterList }

procedure TQueryParameterList.Parse(Text: string);
var
  I: Integer;
  Parts: TListString;
  Pair: TListString;
begin
  try
    Parts := TListString.Create;
    Pair := TListString.Create;
    Clear;
    Parts.Explode(Text, '&', StrToStr);
    for I := 0 to Parts.Count - 1 do begin
      Pair.Explode(Parts[I], '=', StrToStr);
      if Pair.Count >= 2 then
        Values[Pair[0]] := Pair[1];
    end;

  finally
    Parts.Free;
    Pair.Free;
  end;
end;

function TQueryParameterList.Syntetize: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
    Result := Result + '&amp;' + Names[I] + '=' + ValueFromIndex[I];
  Result := Copy(Result, 6, Length(Result));
end;

{ TCookieList }

procedure TCookieList.Parse(Text: string);
var
  I: Integer;
  Parts: TListString;
  Pair: TListString;
begin
  try
    Parts := TListString.Create;
    Pair := TListString.Create;
    Clear;
    Parts.Explode(Text, ';', StrToStr);
    for I := 0 to Parts.Count - 1 do begin
      Pair.Explode(Parts[I], '=', StrToStr);
      if Pair.Count >= 2 then
        Values[Trim(Pair[0])] := Trim(Pair[1]);
    end;
  finally
    Pair.Free;
    Parts.Free;
  end;
end;

function TCookieList.Syntetize: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
    Result := Result + '; ' + Names[I] + '=' + ValueFromIndex[I];
  Result := Copy(Result, 2, Length(Result));
end;

{ THTTPSessionStorage }

procedure THTTPSessionStorage.Load(HandlerData: THTTPHandlerData);
begin

end;

procedure THTTPSessionStorage.Save(HandlerData: THTTPHandlerData);
begin

end;

constructor THTTPSessionStorage.Create;
begin

end;

destructor THTTPSessionStorage.Destroy;
begin
  inherited Destroy;
end;

{ THTTPHandlerData }

constructor THTTPHandlerData.Create;
begin
  Request := THTTPRequest.Create;
  Response := THTTPResponse.Create;
  Session := TStringList.Create;
end;

destructor THTTPHandlerData.Destroy;
begin
  Request.Free;
  Response.Free;
  Session.Free;
  inherited Destroy;
end;

procedure HTTPExceptionShow(Obj: TObject; Addr: Pointer; FrameCount: Longint; Frames: PPointer);
type
  TArrayOfPointer = array of Pointer;
var
  Message: string;
  i: LongInt;
  hstdout: ^Text;
begin
  hstdout := @stdout;
  WriteLn(hstdout^, 'Content-type: text/html');
  WriteLn(hstdout^);
  Writeln(hstdout^, 'An unhandled exception occurred at $', HexStr(PtrUInt(Addr), SizeOf(PtrUInt) * 2), ' :');
  if Obj is exception then
   begin
     Message := Exception(Obj).ClassName + ' : ' + Exception(Obj).Message;
     Writeln(hstdout^, Message);
   end
  else
    Writeln(hstdout^, 'Exception object ', Obj.ClassName, ' is not of class Exception.');
  Writeln(hstdout^, BackTraceStrFunc(Addr));
  if (FrameCount > 0) then
    begin
      for i := 0 to FrameCount - 1 do
        if I < Length(TArrayOfPointer(Frames)) then
          Writeln(hstdout^, BackTraceStrFunc(TArrayOfPointer(Frames)[i]));
    end;
  Writeln(hstdout^,'');
end;

procedure HTTPExceptionHide(Obj: TObject; Addr: Pointer; FrameCount: Longint; Frames: PPointer);
type
  TArrayOfPointer = array of Pointer;
var
  Message: string;
  i: LongInt;
  hstdout: ^Text;
  hstderr: ^Text;
begin
  hstdout := @stdout;
  hstderr := @stderr;
  WriteLn(hstdout^, 'Content-type: text/html');
  WriteLn(hstdout^);
  WriteLn(hstdout^, 'Error occured during page generation.');
  Writeln(hstderr^, 'An unhandled exception occurred at $', HexStr(PtrUInt(Addr), SizeOf(PtrUInt) * 2), ' :');
  if Obj is exception then
   begin
     Message := Exception(Obj).ClassName + ' : ' + Exception(Obj).Message;
     Writeln(hstderr^, Message);
   end
  else
    Writeln(hstderr^, 'Exception object ', Obj.ClassName, ' is not of class Exception.');
  Writeln(hstderr^, BackTraceStrFunc(Addr));
  if (FrameCount > 0) then
    begin
      for i := 0 to FrameCount - 1 do
        if I < Length(TArrayOfPointer(Frames)) then
          Writeln(hstderr^, BackTraceStrFunc(TArrayOfPointer(Frames)[i]));
    end;
  Writeln(hstderr^,'');
end;

end.

