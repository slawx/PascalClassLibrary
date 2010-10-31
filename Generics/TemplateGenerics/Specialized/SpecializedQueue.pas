unit SpecializedQueue;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
{$MACRO ON}

// TQueueInteger<Integer, Integer>
{$DEFINE TGQueueIndex := Integer}
{$DEFINE TGQueueItem := Integer}
{$DEFINE TGQueue := TQueueInteger}
{$DEFINE TGQueueList := TQueueListInteger}
{$DEFINE TGQueueSortCompare := TQueueSortCompareInteger}
{$DEFINE TGQueueStringConverter := TQueueStringConverterInteger}
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericQueue.inc'}

// TQueueInteger<Integer, Pointer>
{$DEFINE TGQueueIndex := Integer}
{$DEFINE TGQueueItem := Pointer}
{$DEFINE TGQueue := TQueuePointer}
{$DEFINE TGQueueList := TQueueListPointer}
{$DEFINE TGQueueSortCompare := TQueueSortComparePointer}
{$DEFINE TGQueueStringConverter := TQueueStringConverterPointer}
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericQueue.inc'}

// TQueueByte<Integer, Byte>
{$DEFINE TGQueueIndex := Integer}
{$DEFINE TGQueueItem := Byte}
{$DEFINE TGQueue := TQueueByte}
{$DEFINE TGQueueList := TQueueListByte}
{$DEFINE TGQueueSortCompare := TQueueSortCompareByte}
{$DEFINE TGQueueStringConverter := TQueueStringConverterByte}
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericQueue.inc'}

implementation

{$DEFINE IMPLEMENTATION_USES}
{$INCLUDE '..\Generic\GenericQueue.inc'}

// TQueueInteger<Integer, Integer>
{$DEFINE TGQueueIndex := Integer}
{$DEFINE TGQueueItem := Integer}
{$DEFINE TGQueue := TQueueInteger}
{$DEFINE TGQueueList := TQueueListInteger}
{$DEFINE TGQueueSortCompare := TQueueSortCompareInteger}
{$DEFINE TGQueueStringConverter := TQueueStringConverterInteger}
{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericQueue.inc'}

// TQueueInteger<Integer, Pointer>
{$DEFINE TGQueueIndex := Integer}
{$DEFINE TGQueueItem := Pointer}
{$DEFINE TGQueue := TQueuePointer}
{$DEFINE TGQueueList := TQueueListPointer}
{$DEFINE TGQueueSortCompare := TQueueSortComparePointer}
{$DEFINE TGQueueStringConverter := TQueueStringConverterPointer}
{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericQueue.inc'}

// TQueueByte<Integer, Byte>
{$DEFINE TGQueueIndex := Integer}
{$DEFINE TGQueueItem := Byte}
{$DEFINE TGQueue := TQueueByte}
{$DEFINE TGQueueList := TQueueListByte}
{$DEFINE TGQueueSortCompare := TQueueSortCompareByte}
{$DEFINE TGQueueStringConverter := TQueueStringConverterByte}
{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericQueue.inc'}

end.
