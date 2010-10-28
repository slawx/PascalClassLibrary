unit ListInteger;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TListIndex = Integer;
  TListItem = Integer;
{$INCLUDE 'ListGenericInterface.tpl'}

type
  TListInteger = class(TGList)
  end;

implementation

{$INCLUDE 'ListGenericImplementation.tpl'}


end.
