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
{$DEFINE INTERFACE}
{$I 'GenericSet.inc'}

// TSetPointer<Integer, Pointer>
{$DEFINE TGSetIndex := Integer}
{$DEFINE TGSetItem := Pointer}
{$DEFINE TGSetList := TSetListPointer}
{$DEFINE TGSet := TSetPointer}
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
{$DEFINE IMPLEMENTATION}
{$I 'GenericSet.inc'}

// TSetPointer<Integer, Pointer>
{$DEFINE TGSetIndex := Integer}
{$DEFINE TGSetItem := Pointer}
{$DEFINE TGSetList := TSetListPointer}
{$DEFINE TGSet := TSetPointer}
{$DEFINE IMPLEMENTATION}
{$I 'GenericSet.inc'}

end.
