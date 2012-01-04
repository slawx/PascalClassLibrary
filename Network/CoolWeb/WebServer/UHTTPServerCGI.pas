unit UHTTPServerCGI;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, UHTTPServer, SpecializedList, IOStream;

type

  { THTTPServerCGI }

  THTTPServerCGI = class(THTTPServer)
  public
    EnvVars: TStringList;
    procedure Run; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ServerInfo(HandlerData: THTTPHandlerData); override;
  end;


procedure Register;

implementation

resourcestring
  SEnvironmentVariables = 'Environment variables:';

procedure Register;
begin
  RegisterComponents('CoolWeb', [THTTPServerCGI]);
end;


{ THTTPServerCGI }

constructor THTTPServerCGI.Create(AOwner: TComponent);
begin
  inherited;
  EnvVars := TStringList.Create;
end;

destructor THTTPServerCGI.Destroy;
begin
  EnvVars.Free;
  inherited Destroy;
end;

procedure THTTPServerCGI.Run;
var
  I: Integer;
  HandlerData: THTTPHandlerData;
  InputStream: TIOStream;
  Line: string;
  Buffer: string;
  Count: Integer;
begin
  HandlerData := THTTPHandlerData.Create;
  with HandlerData do try
    // Load headers
    try
      InputStream := TIOStream.Create(iosInput);
      SetLength(Buffer, 1000);
      repeat
        Count := InputStream.Read(Buffer[1], Length(Buffer));
        if Count > 0 then Request.Content.Write(Buffer[1], Count);
      until Count = 0;
    finally
      InputStream.Free;
    end;

    //repeat
    //  ReadLn(Line);
    //until Line = '';

    // Load data
    (*if Request.Headers.IndexOfName('Content-length') <> -1 then
    try
      InputStream := TIOStream.Create(iosInput);
      Request.Content.CopyFrom(InputStream, StrToInt(Request.Headers.Values['Content-length']));
    finally
      InputStream.Free;
    end;  *)

    // Load environment variables
    for I := 0 to GetEnvironmentVariableCount - 1 do begin
      EnvVars.Add(GetEnvironmentString(I));
    end;

    // Process cookies
    if EnvVars.IndexOfName('HTTP_COOKIE') <> -1 then
      Request.Cookies.Parse(EnvVars.Values['HTTP_COOKIE']);

    // Parse query string
    if Length(EnvVars.Values['QUERY_STRING']) > 0 then
    if EnvVars.Values['QUERY_STRING'][Length(EnvVars.Values['QUERY_STRING'])] = '/' then
      EnvVars.Values['QUERY_STRING'] := Copy(EnvVars.Values['QUERY_STRING'], 1,
        Length(EnvVars.Values['QUERY_STRING']) - 1);
    Request.QueryParts.Explode(EnvVars.Values['QUERY_STRING'], '/', StrToStr);
    if Pos('?', EnvVars.Values['REQUEST_URI']) > 0 then
      Request.Query.Parse(Copy(EnvVars.Values['REQUEST_URI'],
        Pos('?', EnvVars.Values['REQUEST_URI']) + 1,
        Length(EnvVars.Values['REQUEST_URI'])));

    // Load session variables
    if Assigned(SessionStorage) then
      SessionStorage.Load(HandlerData);

    // Load post data
    if EnvVars.IndexOfName('REQUEST_METHOD') <> -1 then begin
      if EnvVars.Values['REQUEST_METHOD'] = 'POST' then begin
        Request.Content.Position := 0;
        Buffer := Request.Content.ReadString;
        Request.Post.Parse(Buffer);
      end;
    end;

    Response.Content.Clear;
    Response.Headers.Values['Content-type'] := 'text/html';

    // Execute content handler
    if Assigned(OnRequest) then OnRequest(HandlerData)
      else raise EEmptyHTTPHandler.Create(SEmptyHTTPHandler);

     // Store session variables
    if Assigned(SessionStorage) then
      SessionStorage.Save(HandlerData);

    with Response do begin
      // Generate cookies
      for I := 0 to Cookies.Count - 1 do
        Headers.Add('Set-Cookie' + Headers.NameValueSeparator + Cookies.Names[I] + '=' + Cookies.ValueFromIndex[I]);
        // + ';path=/;expires=' + RFC822DateTime(Now);

      // Generate headers
      for I := 0 to Headers.Count - 1 do begin
        WriteLn(Headers.Names[I] + ': ' + Headers.ValueFromIndex[I]);
      end;

      WriteLn; // Empty line header separator

      // Emit page content
      Content.Position := 0;
      WriteLn(Content.ReadString);
    end;
  finally
    HandlerData.Free;
  end;
end;

procedure THTTPServerCGI.ServerInfo(HandlerData: THTTPHandlerData);
var
  I: Integer;
begin
  inherited;
  with HandlerData, Response.Content do begin
    WriteString('<h5>' + SEnvironmentVariables + '</h5>');
    WriteString('<table border="1">');
    for I := 0 to EnvVars.Count - 1 do begin
      WriteString('<tr><td>' + EnvVars.Names[I] + '</td><td>' +
        EnvVars.ValueFromIndex[I] + '</td></tr>');
    end;
    WriteString('</table>');
  end;
end;

end.

