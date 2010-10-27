unit StringList;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TListIndex = Integer;
  TListItem = string;
{$INCLUDE 'GenericListInterface.tpl'}

type
  TStringList = class(TGList)
  end;

implementation

{$INCLUDE 'GenericListImplementation.tpl'}


end.
