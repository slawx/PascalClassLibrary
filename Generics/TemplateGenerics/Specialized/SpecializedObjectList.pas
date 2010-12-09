unit SpecializedObjectList;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
{$MACRO ON}

// TListObject<Integer, TObject>
{$DEFINE TGObjectListIndex := Integer}
{$DEFINE TGObjectListItem := TObject}
{$DEFINE TGObjectListList := TObjectListListObject}
{$DEFINE TGObjectList := TListObject}
{$DEFINE TGObjectListSortCompare := TObjectListSortCompareObject}
{$DEFINE TGObjectListToStringConverter := TObjectListToStringConverterObject}
{$DEFINE TGObjectListFromStringConverter := TObjectListFromStringConverterObject}
{$DEFINE INTERFACE}
{$I 'GenericObjectList.inc'}

implementation

{$DEFINE IMPLEMENTATION_USES}
{$I 'GenericObjectList.inc'}

// TListObject<Integer, TObject>
{$DEFINE TGObjectListIndex := Integer}
{$DEFINE TGObjectListItem := TObject}
{$DEFINE TGObjectListList := TObjectListListObject}
{$DEFINE TGObjectList := TListObject}
{$DEFINE TGObjectListSortCompare := TObjectListSortCompareObject}
{$DEFINE TGObjectListToStringConverter := TObjectListToStringConverterObject}
{$DEFINE TGObjectListFromStringConverter := TObjectListFromStringConverterObject}
{$DEFINE IMPLEMENTATION}
{$I 'GenericObjectList.inc'}

end.