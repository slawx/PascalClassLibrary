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
{$DEFINE TGSetStringConverter := TSetStringConverterInteger}
{$DEFINE INTERFACE}
{$I 'GenericSet.inc'}

// TSetPointer<Integer, Pointer>
{$DEFINE TGSetIndex := Integer}
{$DEFINE TGSetItem := Pointer}
{$DEFINE TGSetList := TSetListPointer}
{$DEFINE TGSet := TSetPointer}
{$DEFINE TGSetSortCompare := TSetSortComparePointer}
{$DEFINE TGSetStringConverter := TSetStringConverterPointer}
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
{$DEFINE TGSetStringConverter := TSetStringConverterInteger}
{$DEFINE IMPLEMENTATION}
{$I 'GenericSet.inc'}

// TSetPointer<Integer, Pointer>
{$DEFINE TGSetIndex := Integer}
{$DEFINE TGSetItem := Pointer}
{$DEFINE TGSetList := TSetListPointer}
{$DEFINE TGSet := TSetPointer}
{$DEFINE TGSetSortCompare := TSetSortComparePointer}
{$DEFINE TGSetStringConverter := TSetStringConverterPointer}
{$DEFINE IMPLEMENTATION}
{$I 'GenericSet.inc'}

end.
