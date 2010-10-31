unit SpecializedStack;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
{$MACRO ON}

// TStackInteger<Integer, Integer>
{$DEFINE TGStackIndex := Integer}
{$DEFINE TGStackItem := Integer}
{$DEFINE TGStackList := TListStackInteger}
{$DEFINE TGStack := TStackInteger}
{$DEFINE TGStackSortCompare := TStackSortCompareInteger}
{$DEFINE TGStackStringConverter := TStackStringConverterInteger}
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericStack.inc'}

// TStackInteger<Integer, Pointer>
{$DEFINE TGStackIndex := Integer}
{$DEFINE TGStackItem := Pointer}
{$DEFINE TGStackList := TListStackPointer}
{$DEFINE TGStack := TStackPointer}
{$DEFINE TGStackSortCompare := TStackSortComparePointer}
{$DEFINE TGStackStringConverter := TStackStringConverterPointer}
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericStack.inc'}


implementation

{$DEFINE IMPLEMENTATION_USES}
{$INCLUDE '..\Generic\GenericStack.inc'}

// TStackInteger<Integer, Integer>
{$DEFINE TGStackIndex := Integer}
{$DEFINE TGStackItem := Integer}
{$DEFINE TGStackList := TListStackInteger}
{$DEFINE TGStack := TStackInteger}
{$DEFINE TGStackSortCompare := TStackSortCompareInteger}
{$DEFINE TGStackStringConverter := TStackStringConverterInteger}
{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericStack.inc'}      

// TStackInteger<Integer, Pointer>
{$DEFINE TGStackIndex := Integer}
{$DEFINE TGStackItem := Pointer}
{$DEFINE TGStackList := TListStackPointer}
{$DEFINE TGStack := TStackPointer}
{$DEFINE TGStackSortCompare := TStackSortComparePointer}
{$DEFINE TGStackStringConverter := TStackStringConverterPointer}
{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericStack.inc'}
end.
