unit UHTTPServerTCP;

{$mode delphi}

interface

uses
  Classes, SysUtils, UHTTPServer, UTCPServer, SpecializedList, SynaUtil;

type

  { THTTPServerTCP }

  THTTPServerTCP = class(THTTPServer)
  private
    procedure HandleClient(Sender: TObject);
  public
    Socket: TTCPServer;
    MaxConnection: Integer;
    RequestHandlerList: TRequestHandlerList;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

procedure THTTPServerTCP.HandleClient(Sender: TObject);
var
  RequestHandler: TRequestHandler;
  Line: string;
  LineIndex: Integer;
  LineParts: TListString;
  HandlerData: THTTPHandlerData;
  I: Integer;
begin
  with TTCPClientThread(Sender), Socket do begin
    WriteLn('Used thrads ' + IntToStr(Parent.ThreadPool.UsedCount) + '. Client connected from ' + GetRemoteSinIP);

    HandlerData := THTTPHandlerData.Create;
    with HandlerData do try
      Server := Self;

      Response := THTTPResponse.Create;
      Response.Headers.Values['Server'] := Name;
      Request := THTTPRequest.Create;
      LineIndex := 0;
      try
        LineParts := TListString.Create;
        repeat
          Line := RecvString(10000);
          if (LineIndex = 0) then begin
            LineParts.Explode(Line, ' ', StrToStr);
            if (LineParts.Count >= 3) then begin
              Request.Method := LineParts[0];
              if Pos('?', LineParts[1]) > 0 then begin
                Request.Path := Copy(LineParts[1], 1, Pos('?', LineParts[1]) - 1);
                Request.Query.Parse(Copy(LineParts[1], Pos('?', LineParts[1]) + 1, Length(LineParts[1])));
              end else Request.Path := LineParts[1];
            end;
          end else begin
            LineParts.Explode(Line, ' ', StrToStr, 2);
            if (LineParts.Count = 2) and (LineParts[0][Length(LineParts[0])] = ':') then begin
              LineParts[0] := Copy(LineParts[0], 1, Length(LineParts[0]) - 1);
              Request.Headers.Values[LineParts[0]] := LineParts[1];
              //WriteLn(Line);
            end;
          end;
          Inc(LineIndex);
        until Line = '';
      finally
        LineParts.Free;
      end;

    // Process cookies
    if Request.Headers.IndexOfName('Cookie') <> -1 then
      Request.Cookies.Parse(Request.Headers.Values['Cookie']);

    // Load session variables
    if Assigned(SessionStorage) then
      SessionStorage.Load(HandlerData);

    Response.Stream.Clear;
    Response.Headers.Values['Content-Type'] := 'text/html';

    if Assigned(OnRequest) then OnRequest(HandlerData)
      else raise EEmptyHTTPHandler.Create(SEmptyHTTPHandler);

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
    finally
      Free;
    end;
  end;
end;


{ THTTPServerTCP }

constructor THTTPServerTCP.Create;
begin
  inherited;
  MaxConnection := 10000;
  Socket := TTCPServer.Create;
  Socket.OnClientConnect := HandleClient;
  RequestHandlerList := TRequestHandlerList.Create;
end;

destructor THTTPServerTCP.Destroy;
begin
  Socket.Free;
  RequestHandlerList.Free;
  inherited Destroy;
end;

end.

