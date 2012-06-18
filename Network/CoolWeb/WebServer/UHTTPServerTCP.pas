unit UHTTPServerTCP;

{$mode delphi}

interface

uses
  Classes, SysUtils, UHTTPServer, UTCPServer, SpecializedList, SynaUtil;

type

  { THTTPServerTCP }

  THTTPServerTCP = class(THTTPServer)
  private
    FMaxConnection: Integer;
    procedure HandleClient(Sender: TObject);
  public
    Socket: TTCPServer;
    RequestHandlerList: TRequestHandlerList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Run; override;
  published
    property MaxConnection: Integer read FMaxConnection write FMaxConnection;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CoolWeb', [THTTPServerTCP]);
end;

procedure THTTPServerTCP.HandleClient(Sender: TObject);
var
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
      Response.Headers.Add('Server', Name);
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
                Request.Query.Parse(Copy(LineParts[1], Pos('?', LineParts[1]) + 1, Length(LineParts[1])));
                Request.Path.Explode(Copy(LineParts[1], 1, Pos('?', LineParts[1]) - 1), '/', StrToStr);
              end else begin
                Request.Path.Explode(LineParts[1], '/', StrToStr);
                Request.Query.Clear;
              end;
            end;
          end else begin
            LineParts.Explode(Line, ' ', StrToStr, 2);
            if (LineParts.Count = 2) and (LineParts[0][Length(LineParts[0])] = ':') then begin
              LineParts[0] := Copy(LineParts[0], 1, Length(LineParts[0]) - 1);
              Request.Headers.Add(LineParts[0], LineParts[1]);
              //WriteLn(Line);
            end;
          end;
          Inc(LineIndex);
        until Line = '';
      finally
        LineParts.Free;
      end;

    // Process cookies
    if Request.Headers.SearchKey('Cookie') <> -1 then
      Request.Cookies.Parse(Request.Headers.Values['Cookie']);

    // Load session variables
    if Assigned(SessionStorage) then
      SessionStorage.Load(HandlerData);

    Response.Content.Clear;
    Response.Headers.Add('Content-Type', 'text/html');

    if Assigned(OnRequest) then OnRequest(HandlerData)
      else raise EEmptyHTTPHandler.Create(SEmptyHTTPHandler);

    // Store session variables
    if Assigned(SessionStorage) then
      SessionStorage.Save(HandlerData);

    with Response do begin
      SendString('HTTP/1.0 200 OK'#13#10);
      Headers.Add('Content-Length', IntToStr(Content.Size));
      Headers.Add('Connection', 'close');
      Headers.Add('Date', RFC822DateTime(Now));

      // Handle cookies
      for I := 0 to Cookies.Count - 1 do
        Headers.Add('Set-Cookie', Cookies.Names[I] + '=' + Cookies.ValueFromIndex[I]);
        // + ';path=/;expires=' + RFC822DateTime(Now);

      // Send headers
      for I := 0 to Headers.Count - 1 do begin
        WriteLn(Headers.Keys[I] + ': ' + Headers.Items[I].Value + #13#10);
        SendString(Headers.Keys[I] + ': ' + Headers.Items[I].Value + #13#10);
      end;
      SendString(#13#10);
      SendBuffer(Content.Memory, Content.Size);
      SendString(#13#10);
    end;
    finally
      Free;
    end;
  end;
end;


{ THTTPServerTCP }

constructor THTTPServerTCP.Create(AOwner: TComponent);
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

procedure THTTPServerTCP.Run;
begin
  inherited Run;
  WriteLn('HTTP Server started in TCP mode.');
  WriteLn('Listen on ' + Socket.Address + ':' + IntToStr(Socket.Port));
  WriteLn('Press any key to terminate...');
  Socket.ThreadPool.TotalCount := MaxConnection;
  Socket.Active := True;
  ReadLn;
  WriteLn('Exiting');
end;

end.

