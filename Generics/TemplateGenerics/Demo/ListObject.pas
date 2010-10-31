unit ListObject;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TObjectListIndex = Integer;
  TObjectListItem = TObject;
{$INCLUDE '..\Generic\ObjectListInterface.tpl'}

type

  { TListObject }

  TListObject = class(TGObjectList)
  end;

implementation

{$INCLUDE '..\Generic\ObjectListImplementation.tpl'}

end.
