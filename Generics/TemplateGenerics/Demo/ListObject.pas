unit ListObject;

{$mode delphi}
{$MACRO ON}

interface

uses
  Classes, SysUtils;

type
// TListObject<Integer, TObject>
{$DEFINE TGObjectListIndex := Integer}
{$DEFINE TGObjectListItem := TObject}
{$DEFINE TGObjectListList := TObjectListList}
{$DEFINE TGObjectList := TListObject}
{$DEFINE TGObjectListSortCompare := TObjectListSortCompareInteger}
{$DEFINE TGObjectListStringConverter := TObjectListStringConverterInteger}
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericObjectList.inc'}


implementation

// TListObject<Integer, TObject>
{$DEFINE TGObjectListIndex := Integer}
{$DEFINE TGObjectListItem := TObject}
{$DEFINE TGObjectListList := TObjectListList}
{$DEFINE TGObjectList := TListObject}
{$DEFINE TGObjectListSortCompare := TObjectListSortCompareInteger}
{$DEFINE TGObjectListStringConverter := TObjectListStringConverterInteger}
{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericObjectList.inc'}

end.
