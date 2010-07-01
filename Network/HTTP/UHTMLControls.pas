unit UHTMLControls;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, UHTTPServer;

type
  { TPageList }

  TPageList = class
    NavigatorVisibleItems: Integer;
    PageIndexName: string;
    Page: Integer;
    TotalCount: Integer;
    ItemPerPage: Integer;
    Output: string;
    HandlerData: THTTPHandlerData;
    function SQLLimit: string;
    procedure Process;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TPageList }

function TPageList.SQLLimit: string;
begin
  Result := ' LIMIT ' + IntToStr(Page * ItemPerPage) + ', ' + IntToStr(ItemPerPage);
end;

procedure TPageList.Process;
var
  Count: Integer;
  PagesMax: Integer;
  PagesMin: Integer;
  I: Integer;
begin
  if Assigned(HandlerData) and
  (HandlerData.Session.IndexOfName(PageIndexName) <> -1) then
    Page := StrToInt(HandlerData.Session.Values[PageIndexName])
    else Page := 0;
  if HandlerData.Request.Query.IndexOfName(PageIndexName) <> -1 then
    Page := StrToInt(HandlerData.Request.Query.Values[PageIndexName]);

  Count := Trunc(TotalCount / ItemPerPage) + 1;
  if Page > Count - 1 then Page := Count - 1;

  Output := '';
  if Count > 1 then
  with HandlerData, Request do begin

    if Page > 0 then begin
      Query.Values[PageIndexName] := IntToStr(0);
      Output := Output + '<a href="?' + Query.Syntetize + '">&lt;&lt;</a>';
      Query.Values[PageIndexName] := IntToStr(Page - 1);
      Output := Output + ' <a href="?' + Query.Syntetize + '">&lt;</a>';
    end;

    // Get navigator visible range
    PagesMin := Page - (NavigatorVisibleItems - 1) div 2;
    if PagesMin < 0 then PagesMin := 0;
    PagesMax := PagesMin + (NavigatorVisibleItems - 1);
    if PagesMax > Count - 1 then PagesMax := Count - 1;

    if PagesMin > 0 then Output := Output + ' .. ';
    // Show page numbers
    for I := PagesMin to PagesMax do begin
      Query.Values[PageIndexName] := IntToStr(I);
      if I = Page then begin
        Output := Output + ' <strong>' + IntToStr(I + 1) + '</strong>'
      end else begin
        Output := Output + ' <a href="?' + Query.Syntetize + '">' +
          IntToStr(I + 1) + '</a>';
      end;
    end;
    if PagesMax < (Count - 1) then Output := Output + ' .. ';

    if Page < (Count - 1) then begin
      Query.Values[PageIndexName] := IntToStr(Page + 1);
      Output := Output + ' <a href="?' + Query.Syntetize + '">&gt;</a>';
      Query.Values[PageIndexName] := IntToStr(Count - 1);
      Output := Output + ' <a href="?' + Query.Syntetize + '">&gt;&gt;</a>';
    end;
  end;

  HandlerData.Session.Values[PageIndexName] := IntToStr(Page);
end;

constructor TPageList.Create;
begin
  PageIndexName := 'Page';
  ItemPerPage := 40;
  NavigatorVisibleItems := 30;
end;

destructor TPageList.Destroy;
begin
  inherited Destroy;
end;

end.

