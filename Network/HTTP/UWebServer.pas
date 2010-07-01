unit UWebServer;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, UHTTPServer, UHTTPSessionFile,
  UTCPServer, Contnrs,
  UCommon, syncobjs,
  UMemoryStreamEx,
  UMIMEType, Synautil, UPool,
  USqlDatabase, DOM, XMLRead, UHTMLControls;

const
  ConfigFileName = 'Config.xml';

type
  { TWebServer }

  TWebServer = class
  private
  public
    HTTPServer: THTTPServer;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

constructor TWebServer.Create;
begin
  inherited Create;
  HTTPServer := THTTPServer.Create;
  with HTTPServer, Socket do begin
  end;
end;

destructor TWebServer.Destroy;
begin
  HTTPServer.Destroy;
  inherited Destroy;
end;


end.

