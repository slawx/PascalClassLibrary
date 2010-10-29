
  TListIndex = TStackIndex;
  TListItem = TStackItem;
  {$INCLUDE 'ListInterface.tpl'}

  // TGStack<TStackIndex, TStackItem> = class(TGList)
  TGStack = class
  private
    FList: TGList;
  public
    procedure Push(Value: TStackItem);
    function Pop: TStackItem;
    constructor Create;
    destructor Destroy; override;
    property List: TGList read FList;
  end;
