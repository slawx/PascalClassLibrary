unit ObjectList;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TIndexType = Integer;
  TItemType = TObject;
{$INCLUDE 'GenericListInterface.tpl'}

type
  TObjectGList = class(TGList)
  end;

implementation

{$INCLUDE 'GenericListImplementation.tpl'}


end.
