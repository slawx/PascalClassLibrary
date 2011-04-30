unit UWebPage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TOnProduceEvent = procedure of object;

  { TWebPage }

  TWebPage = class(TComponent)
  private
    FOnProduce: TOnProduceEvent;
  published
    property OnProduce: TOnProduceEvent read FOnProduce write FOnProduce;
  end;

implementation

end.

