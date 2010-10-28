unit ListDouble;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TListIndex = Integer;
  TListItem = Double;
{$INCLUDE 'ListGenericInterface.tpl'}

type
  TListDouble = class(TGList)
  end;

implementation

{$INCLUDE 'ListGenericImplementation.tpl'}


end.
