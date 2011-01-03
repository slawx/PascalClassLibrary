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
{$DEFINE TGQueueToStringConverter := TQueueToStringConverterInteger}
{$DEFINE TGQueueFromStringConverter := TQueueFromStringConverterInteger}
{$DEFINE TGQueueItemArray := TQueueIntegerItemArray}
{$DEFINE INTERFACE}
{$I 'GenericQueue.inc'}

// TQueueInteger<Integer, Pointer>
{$DEFINE TGQueueIndex := Integer}
{$DEFINE TGQueueItem := Pointer}
{$DEFINE TGQueue := TQueuePointer}
{$DEFINE TGQueueList := TQueueListPointer}
{$DEFINE TGQueueSortCompare := TQueueSortComparePointer}
{$DEFINE TGQueueToStringConverter := TQueueToStringConverterPointer}
{$DEFINE TGQueueFromStringConverter := TQueueFromStringConverterPointer}
{$DEFINE TGQueueItemArray := TQueuePointerItemArray}
{$DEFINE INTERFACE}
{$I 'GenericQueue.inc'}

// TQueueByte<Integer, Byte>
{$DEFINE TGQueueIndex := Integer}
{$DEFINE TGQueueItem := Byte}
{$DEFINE TGQueue := TQueueByte}
{$DEFINE TGQueueList := TQueueListByte}
{$DEFINE TGQueueSortCompare := TQueueSortCompareByte}
{$DEFINE TGQueueToStringConverter := TQueueToStringConverterByte}
{$DEFINE TGQueueFromStringConverter := TQueueFromStringConverterByte}
{$DEFINE TGQueueItemArray := TQueueByteItemArray}
{$DEFINE INTERFACE}
{$I 'GenericQueue.inc'}

implementation

{$DEFINE IMPLEMENTATION_USES}
{$I 'GenericQueue.inc'}

// TQueueInteger<Integer, Integer>
{$DEFINE TGQueueIndex := Integer}
{$DEFINE TGQueueItem := Integer}
{$DEFINE TGQueue := TQueueInteger}
{$DEFINE TGQueueList := TQueueListInteger}
{$DEFINE TGQueueSortCompare := TQueueSortCompareInteger}
{$DEFINE TGQueueToStringConverter := TQueueToStringConverterInteger}
{$DEFINE TGQueueFromStringConverter := TQueueFromStringConverterInteger}
{$DEFINE TGQueueItemArray := TQueueIntegerItemArray}
{$DEFINE IMPLEMENTATION}
{$I '..\Generic\GenericQueue.inc'}

// TQueueInteger<Integer, Pointer>
{$DEFINE TGQueueIndex := Integer}
{$DEFINE TGQueueItem := Pointer}
{$DEFINE TGQueue := TQueuePointer}
{$DEFINE TGQueueList := TQueueListPointer}
{$DEFINE TGQueueSortCompare := TQueueSortComparePointer}
{$DEFINE TGQueueToStringConverter := TQueueToStringConverterPointer}
{$DEFINE TGQueueFromStringConverter := TQueueFromStringConverterPointer}
{$DEFINE TGQueueItemArray := TQueuePointerItemArray}
{$DEFINE IMPLEMENTATION}
{$I 'GenericQueue.inc'}

// TQueueByte<Integer, Byte>
{$DEFINE TGQueueIndex := Integer}
{$DEFINE TGQueueItem := Byte}
{$DEFINE TGQueue := TQueueByte}
{$DEFINE TGQueueList := TQueueListByte}
{$DEFINE TGQueueSortCompare := TQueueSortCompareByte}
{$DEFINE TGQueueToStringConverter := TQueueToStringConverterByte}
{$DEFINE TGQueueFromStringConverter := TQueueFromStringConverterByte}
{$DEFINE TGQueueItemArray := TQueueByteItemArray}
{$DEFINE IMPLEMENTATION}
{$I 'GenericQueue.inc'}

end.
