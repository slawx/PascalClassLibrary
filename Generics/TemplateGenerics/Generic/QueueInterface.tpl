
  TListIndex = TQueueIndex;
  TListItem = TQueueItem;
  {$INCLUDE 'ListInterface.tpl'}

  // TGQueue<TListIndex, TListItem> = class(TGList)
  TGQueue = class(TGList)
  private
  public
    procedure Enqueue(Value: TQueueItem);
    function Dequeue: TQueueItem;
    function Peek: TQueueItem;
  end;