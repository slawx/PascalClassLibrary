unit UWebPage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UHTTPServer, Controls;

type
  TOnProduceEvent = procedure(HandlerData: THTTPHandlerData) of object;

  { TWebPage }

  TWebPage = class(TDataModule)
  private
    FCaption: string;
    FOnProduce: TOnProduceEvent;
  published
    property Caption: string read FCaption write FCaption;
    property OnProduce: TOnProduceEvent read FOnProduce write FOnProduce;
  end;


  TWebPageClass = class of TWebPage;

procedure Register;


implementation

procedure Register;
begin
  RegisterNoIcon([TWebPage]);
end;

end.

