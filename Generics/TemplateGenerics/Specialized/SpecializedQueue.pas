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
{$DEFINE INTERFACE}
{$I 'GenericQueue.inc'}

// TQueueInteger<Integer, Pointer>
{$DEFINE TGQueueIndex := Integer}
{$DEFINE TGQueueItem := Pointer}
{$DEFINE TGQueue := TQueuePointer}
{$DEFINE TGQueueList := TQueueListPointer}
{$DEFINE INTERFACE}
{$I 'GenericQueue.inc'}

// TQueueByte<Integer, Byte>
{$DEFINE TGQueueIndex := Integer}
{$DEFINE TGQueueItem := Byte}
{$DEFINE TGQueue := TQueueByte}
{$DEFINE TGQueueList := TQueueListByte}
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
{$DEFINE IMPLEMENTATION}
{$I '..\Generic\GenericQueue.inc'}

// TQueueInteger<Integer, Pointer>
{$DEFINE TGQueueIndex := Integer}
{$DEFINE TGQueueItem := Pointer}
{$DEFINE TGQueue := TQueuePointer}
{$DEFINE TGQueueList := TQueueListPointer}
{$DEFINE IMPLEMENTATION}
{$I 'GenericQueue.inc'}

// TQueueByte<Integer, Byte>
{$DEFINE TGQueueIndex := Integer}
{$DEFINE TGQueueItem := Byte}
{$DEFINE TGQueue := TQueueByte}
{$DEFINE TGQueueList := TQueueListByte}
{$DEFINE IMPLEMENTATION}
{$I 'GenericQueue.inc'}

end.
