unit UHTTPServerCGI;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, UHTTPServer, SpecializedList;

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
begin
  HandlerData := THTTPHandlerData.Create;
  with HandlerData do try
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

    Response.Stream.Clear;
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
      Stream.Position := 0;
      WriteLn(Stream.ReadString);
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
  with HandlerData, Response.Stream do begin
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

