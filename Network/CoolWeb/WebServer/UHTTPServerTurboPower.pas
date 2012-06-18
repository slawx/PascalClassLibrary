unit UHTTPServerTurboPower;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, UHTTPServer, IpHtml;

type

  { THTTPServerTurboPower }

  THTTPServerTurboPower = class(THTTPServer)
  public
    procedure Run; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure Register;


implementation

uses
  UTurboPowerForm;

procedure Register;
begin
  RegisterComponents('CoolWeb', [THTTPServerTurboPower]);
end;


{ THTTPServerTurboPower }

procedure THTTPServerTurboPower.Run;
var
  HandlerData: THTTPHandlerData;
begin
  inherited Run;
  HandlerData := THTTPHandlerData.Create;
  with HandlerData do try
    // Execute content handler
    if Assigned(OnRequest) then OnRequest(HandlerData)
      else raise EEmptyHTTPHandler.Create(SEmptyHTTPHandler);
    Response.Content.Position := 0;
    FormWebBrowser.HtmlSource := Response.Content.ReadString;
    FormWebBrowser.IpHtmlPanel1.SetHtmlFromStr(FormWebBrowser.HtmlSource);
  finally
    Free;
  end;
end;

constructor THTTPServerTurboPower.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Application.CreateForm(TFormWebBrowser, FormWebBrowser);
  FormWebBrowser.Show;
end;

destructor THTTPServerTurboPower.Destroy;
begin
  inherited Destroy;
end;

end.

