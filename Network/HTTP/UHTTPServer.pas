unit UHTTPServer;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, UTCPServer, UCommon, UMemoryStreamEx, UMIMEType,
  Synautil, UStringListEx;

type
  THTTPServer = class;

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

  TRequestHandlerList = class(TList)
    procedure Add(AName: string; AHandler: TRequestEvent);
    function IndexOfName(AName: string): TRequestHandler;
  end;

  { THTTPSession }

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
    procedure HandleClient(Sender: TObject);
    procedure ErrorResponse(HandlerData: THTTPHandlerData);
  public
    Name: string;
    Socket: TTCPServer;
    DocumentRoot: string;
    MaxConnection: Integer;
    RequestHandlerList: TRequestHandlerList;
    SessionStorage: THTTPSessionStorage;
    procedure FileResponse(HandlerData: THTTPHandlerData);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ THTTPServer }

procedure THTTPServer.HandleClient(Sender: TObject);
var
  RequestHandler: TRequestHandler;
  Line: string;
  LineIndex: Integer;
  LineParts: TStringListEx;
  HandlerData: THTTPHandlerData;
  I: Integer;
begin
  with TTCPClientThread(Sender), Socket do begin
    WriteLn('Used thrads ' + IntToStr(Parent.ThreadPool.UsedCount) + '. Client connected from ' + GetRemoteSinIP);

    HandlerData := THTTPHandlerData.Create;
    with HandlerData do begin
      Server := Self;

      Response := THTTPResponse.Create;
      Response.Headers.Values['Server'] := Name;
      Request := THTTPRequest.Create;
      LineIndex := 0;
      LineParts := TStringListEx.Create;
      repeat
        Line := RecvString(10000);
        if (LineIndex = 0) then begin
          LineParts.Explode(' ', Line);
          if (LineParts.Count >= 3) then begin
            Request.Method := LineParts[0];
            if Pos('?', LineParts[1]) > 0 then begin
              Request.Path := Copy(LineParts[1], 1, Pos('?', LineParts[1]) - 1);
              Request.Query.Parse(Copy(LineParts[1], Pos('?', LineParts[1]) + 1, Length(LineParts[1])));
            end else Request.Path := LineParts[1];
          end;
        end else begin
          LineParts.Explode(' ', Line, 2);
          if (LineParts.Count = 2) and (LineParts[0][Length(LineParts[0])] = ':') then begin
            LineParts[0] := Copy(LineParts[0], 1, Length(LineParts[0]) - 1);
            Request.Headers.Values[LineParts[0]] := LineParts[1];
            //WriteLn(Line);
          end;
        end;
        Inc(LineIndex);
      until Line = '';
      LineParts.Destroy;

    // Process cookies
    if Request.Headers.IndexOfName('Cookie') <> -1 then
      Request.Cookies.Parse(Request.Headers.Values['Cookie']);

    // Load session variables
    if Assigned(SessionStorage) then
      SessionStorage.Load(HandlerData);

    Response.Stream.Clear;
    Response.Headers.Values['Content-Type'] := 'text/html';

    WriteLn('Requested path: ' + Request.Path);
    RequestHandler := RequestHandlerList.IndexOfName(Request.Path);

    if Assigned(RequestHandler) then RequestHandler.Handler(HandlerData)
      else ErrorResponse(HandlerData);

    // Store session variables
    if Assigned(SessionStorage) then
      SessionStorage.Save(HandlerData);

    with Response do begin
      SendString('HTTP/1.0 200 OK'#13#10);
      Headers.Values['Content-Length'] := IntToStr(Stream.Size);
      Headers.Values['Connection'] := 'close';
      Headers.Values['Date'] := RFC822DateTime(Now);

      // Handle cookies
      for I := 0 to Cookies.Count - 1 do
        Headers.Add('Set-Cookie' + Headers.NameValueSeparator + Cookies.Names[I] + '=' + Cookies.ValueFromIndex[I]);
        // + ';path=/;expires=' + RFC822DateTime(Now);

      // Send headers
      for I := 0 to Headers.Count - 1 do begin
        //WriteLn(Headers.Names[I] + ': ' + Headers.ValueFromIndex[I] + #13#10);
        SendString(Headers.Names[I] + ': ' + Headers.ValueFromIndex[I] + #13#10);
      end;
      SendString(#13#10);
      SendBuffer(Stream.Memory, Stream.Size);
      SendString(#13#10);
    end;

      Destroy;
    end;
  end;
end;

procedure THTTPServer.ErrorResponse(HandlerData: THTTPHandlerData);
begin
  with HandlerData, Response.Stream do begin
    WriteString('<html><body>Page ' + Request.Path + ' not found.</body></html>');
  end;
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
      Response.Stream.WriteStream(BinaryFile);
      BinaryFile.Destroy;
    end else
    with Response.Stream do begin
      WriteLn('File ' + Request.Path + ' not found.');
      WriteString('<html><body>File ' + Request.Path + ' not found.</body></html>');
    end;
  end;
end;

constructor THTTPServer.Create;
begin
  Socket := TTCPServer.Create;
  Socket.OnClientConnect := HandleClient;
  DocumentRoot := './';
  RequestHandlerList := TRequestHandlerList.Create;
  Name := 'THTTPServer';
  MaxConnection := 10000;
end;

destructor THTTPServer.Destroy;
begin
  Socket.Destroy;
  RequestHandlerList.Destroy;
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
  Stream.Destroy;
  Headers.Destroy;
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
  Headers := TStringList.Create;
  Cookies := TCookieList.Create;
end;

destructor THTTPRequest.Destroy;
begin
  Query.Destroy;
  Headers.Destroy;
  inherited Destroy;
end;

{ TQueryParameterList }

procedure TQueryParameterList.Parse(Text: string);
var
  I: Integer;
  Parts: TStringListEx;
  Pair: TStringListEx;
begin
  Parts := TStringListEx.Create;
  Pair := TStringListEx.Create;
  Clear;
  Parts.Explode('&', Text);
  for I := 0 to Parts.Count - 1 do begin
    Pair.Explode('=', Parts[I]);
    if Pair.Count >= 2 then
      Values[Pair[0]] := Pair[1];
  end;
  Parts.Destroy;
  Pair.Destroy;
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
  Parts: TStringListEx;
  Pair: TStringListEx;
begin
  Parts := TStringListEx.Create;
  Pair := TStringListEx.Create;
  Clear;
  Parts.Explode(';', Text);
  for I := 0 to Parts.Count - 1 do begin
    Pair.Explode('=', Parts[I]);
    if Pair.Count >= 2 then
      Values[Trim(Pair[0])] := Trim(Pair[1]);
  end;
  Pair.Destroy;
  Parts.Destroy;
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
  Request.Destroy;
  Response.Destroy;
  Session.Destroy;
  inherited Destroy;
end;

end.

