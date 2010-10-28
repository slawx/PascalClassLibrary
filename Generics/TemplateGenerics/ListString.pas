unit ListString;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TListIndex = Integer;
  TListItem = string;
{$INCLUDE 'ListGenericInterface.tpl'}

type
  TListString = class(TGList)
  end;

implementation

{$INCLUDE 'ListGenericImplementation.tpl'}


end.
