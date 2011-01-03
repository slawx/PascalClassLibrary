unit SpecializedSet;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
{$MACRO ON}

// TSetInteger<Integer, Integer>
{$DEFINE TGSetIndex := Integer}
{$DEFINE TGSetItem := Integer}
{$DEFINE TGSetList := TSetListInteger}
{$DEFINE TGSet := TSetInteger}
{$DEFINE TGSetSortCompare := TSetSortCompareInteger}
{$DEFINE TGSetToStringConverter := TSetToStringConverterInteger}
{$DEFINE TGSetFromStringConverter := TSetFromStringConverterInteger}
{$DEFINE TGSetItemArray := TSetIntegerItemArray}
{$DEFINE INTERFACE}
{$I 'GenericSet.inc'}

// TSetPointer<Integer, Pointer>
{$DEFINE TGSetIndex := Integer}
{$DEFINE TGSetItem := Pointer}
{$DEFINE TGSetList := TSetListPointer}
{$DEFINE TGSet := TSetPointer}
{$DEFINE TGSetSortCompare := TSetSortComparePointer}
{$DEFINE TGSetToStringConverter := TSetToStringConverterPointer}
{$DEFINE TGSetFromStringConverter := TSetFromStringConverterPointer}
{$DEFINE TGSetItemArray := TSetPointerItemArray}
{$DEFINE INTERFACE}
{$I 'GenericSet.inc'}

implementation

{$DEFINE IMPLEMENTATION_USES}
{$I 'GenericSet.inc'}

// TSetInteger<Integer, Integer>
{$DEFINE TGSetIndex := Integer}
{$DEFINE TGSetItem := Integer}
{$DEFINE TGSetList := TSetListInteger}
{$DEFINE TGSet := TSetInteger}
{$DEFINE TGSetSortCompare := TSetSortCompareInteger}
{$DEFINE TGSetToStringConverter := TSetToStringConverterInteger}
{$DEFINE TGSetFromStringConverter := TSetFromStringConverterInteger}
{$DEFINE TGSetItemArray := TSetIntegerItemArray}
{$DEFINE IMPLEMENTATION}
{$I 'GenericSet.inc'}

// TSetPointer<Integer, Pointer>
{$DEFINE TGSetIndex := Integer}
{$DEFINE TGSetItem := Pointer}
{$DEFINE TGSetList := TSetListPointer}
{$DEFINE TGSet := TSetPointer}
{$DEFINE TGSetSortCompare := TSetSortComparePointer}
{$DEFINE TGSetToStringConverter := TSetToStringConverterPointer}
{$DEFINE TGSetFromStringConverter := TSetFromStringConverterPointer}
{$DEFINE TGSetItemArray := TSetPointerItemArray}
{$DEFINE IMPLEMENTATION}
{$I 'GenericSet.inc'}

end.
