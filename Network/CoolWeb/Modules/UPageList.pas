unit UPageList;

{$mode delphi}

interface

uses
  Classes, SysUtils, UHtmlClasses;

type
  TPageList = class
    TotalCount: Integer;
    Page: Integer;
    PageCount: Integer;
    Output: string;
    SQLLimit: string;
    ItemPerPage: Integer;
    VisiblePageItems: Integer;
    HTMLId: string;
    QueryItems: TQueryString;
    Around: Integer;
    constructor Create;
    function Hyperlink: string;
    function Process: string;
  end;


implementation

constructor TPageList.Create;
begin
  ItemPerPage := 40;
  VisiblePageItems := 30;
  QueryItems := TQueryString.Create;
  QueryItems.SetStringServer;
  HTMLId := '';
end;

function TPageList.Hyperlink: string;
begin
  // Create hyperlink
  if HTMLId = '' then Hyperlink := 'href="?' + QueryItems.GetString + '"'
  else begin
    QueryItems.Data.Values['Panel'] := '1';
    Hyperlink := 'href="" onclick="ReloadElement(''' + HTMLId + ''', ''?' + QueryItems.GetString + ''');return false;"';
  end;
  Result := Hyperlink;
end;

function TPageList.Process: string;
begin
(*  Around := Round(VisiblePageItems / 2);
  Output := '';
  PageCount := Trunc(TotalCount / ItemPerPage) + 1;

  if not array_key_exists('Page', $_SESSION)) $_SESSION['Page'] := 0;
    if(array_key_exists('Page', $_GET)) $_SESSION['Page'] := $_GET['Page'] * 1;
    if($_SESSION['Page'] < 0) $_SESSION['Page'] = 0;
    if($_SESSION['Page'] >= $PageCount) $_SESSION['Page'] := PageCount - 1;
    Page := $_SESSION['Page'];

    Output := Output + 'Počet položek: <strong>' + IntToStr(TotalCount) + '</strong> &nbsp; Stránky: ';

    Output := '';
    if PageCount > 1 then begin
      if Page > 0 then begin
        QueryItems.Data['Page'] := 0;
        Output := Output + '<a ' + Hyperlink + '>&lt;&lt;</a> ';
        QueryItems.Data['Page'] := (Page - 1);
        Output := Output + '<a ' + Hyperlink + '>&lt;</a> ';
      end;
      PagesMax := PageCount - 1;
      PagesMin := 0;
      if PagesMax > (Page + Around) then PagesMax := Page + Around;
      if PagesMin < (Page - Around) then begin
        Output := Output + ' ... ';
        PagesMin := Page - Around;
      end;
      for I := PagesMin to PagesMax - 1 do begin
        if I = Page then Output := Output + '<strong>' + IntToStr(I + 1) '</strong> ';
        else begin
         QueryItems.Data['Page'] := I;
         Output := Output + '<a ' + Hyperlink + '>' + IntToStr(I + 1) + '</a> ';
        end;
      end;
      if PagesMax < (PageCount - 1) then Output := Output + ' ... ';
      if Page < (PageCount - 1) then begin
        QueryItems.Data.Values['Page'] := (Page + 1);
        Output := Output + '<a ' + Hyperlink + '>&gt;</a> ';
        QueryItems.Data.Values['Page'] := (PageCount - 1);
        Output := Output + '<a ' + Hyperlink + '>&gt;&gt;</a>';
      end;
    end;
    if Output <> '' Output := '<div style="text-align: center">' + Output + '</div>';
    SQLLimit := ' LIMIT ' + IntToStr(Page * ItemPerPage) + ', ' + IntToStr(ItemPerPage);
  end;*)
end;

end.

