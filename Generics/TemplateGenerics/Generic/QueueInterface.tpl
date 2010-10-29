
  TListIndex = TQueueIndex;
  TListItem = TQueueItem;
  {$INCLUDE 'ListInterface.tpl'}

  // TGQueue<TSetIndex, TSetItem> = class(TGList)
  TGQueue = class
  private
    FList: TGList;
  public
    procedure Enqueue(Value: TQueueItem);
    function Dequeue: TQueueItem;
    function Peek: TQueueItem;
    constructor Create;
    destructor Destroy; override;
    property List: TGList read FList;
  end;
