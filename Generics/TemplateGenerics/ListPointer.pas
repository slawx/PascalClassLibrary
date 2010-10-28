unit ListPointer;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TListIndex = Integer;
  TListItem = Pointer;
{$INCLUDE 'ListGenericInterface.tpl'}

type
  TListPointer = class(TGList)
  end;

implementation

{$INCLUDE 'ListGenericImplementation.tpl'}


end.
