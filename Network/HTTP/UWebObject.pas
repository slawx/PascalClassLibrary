unit UWebObject;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, UHTTPServer, UHTTPSessionFile,
  UTCPServer, Contnrs,
  UCommon, syncobjs,
  UMemoryStreamEx,
  UMIMEType, Synautil, UPool,
  USqlDatabase, DOM, XMLRead, UHTMLControls;

type

  { TWebObject }

  TWebObject = class
    Database: TSqlDatabase;
    HandlerData: THTTPHandlerData;
    constructor Create(Database: TSqlDatabase; HandlerData: THTTPHandlerData); overload;
    constructor Create; virtual; overload;
  end;

implementation

{ TWebObject }

constructor TWebObject.Create(Database: TSqlDatabase;
  HandlerData: THTTPHandlerData);
begin
  Self.Database := Database;
  Self.HandlerData := HandlerData;
  Create;
end;

constructor TWebObject.Create;
begin

end;

end.

