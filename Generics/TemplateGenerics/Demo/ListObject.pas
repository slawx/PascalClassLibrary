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
{$I 'GenericObjectList.inc'}


implementation

{$DEFINE IMPLEMENTATION_USES}
{$I 'GenericObjectList.inc'}

// TListObject<Integer, TObject>
{$DEFINE TGObjectListIndex := Integer}
{$DEFINE TGObjectListItem := TObject}
{$DEFINE TGObjectListList := TObjectListList}
{$DEFINE TGObjectList := TListObject}
{$DEFINE TGObjectListSortCompare := TObjectListSortCompareInteger}
{$DEFINE TGObjectListStringConverter := TObjectListStringConverterInteger}
{$DEFINE IMPLEMENTATION}
{$I 'GenericObjectList.inc'}

end.
