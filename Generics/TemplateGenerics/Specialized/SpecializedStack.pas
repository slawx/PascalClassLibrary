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
{$DEFINE INTERFACE}
{$I 'GenericStack.inc'}

// TStackInteger<Integer, Pointer>
{$DEFINE TGStackIndex := Integer}
{$DEFINE TGStackItem := Pointer}
{$DEFINE TGStackList := TListStackPointer}
{$DEFINE TGStack := TStackPointer}
{$DEFINE INTERFACE}
{$I 'GenericStack.inc'}


implementation

{$DEFINE IMPLEMENTATION_USES}
{$I 'GenericStack.inc'}

// TStackInteger<Integer, Integer>
{$DEFINE TGStackIndex := Integer}
{$DEFINE TGStackItem := Integer}
{$DEFINE TGStackList := TListStackInteger}
{$DEFINE TGStack := TStackInteger}
{$DEFINE IMPLEMENTATION}
{$I 'GenericStack.inc'}

// TStackInteger<Integer, Pointer>
{$DEFINE TGStackIndex := Integer}
{$DEFINE TGStackItem := Pointer}
{$DEFINE TGStackList := TListStackPointer}
{$DEFINE TGStack := TStackPointer}
{$DEFINE IMPLEMENTATION}
{$I 'GenericStack.inc'}
end.
