
  TListIndex = TStackIndex;
  TListItem = TStackItem;
  {$INCLUDE 'ListInterface.tpl'}

  // TGStack<TListIndex, TListItem> = class(TGList)
  TGStack = class(TGList)
  private
  public
    procedure Push(Value: TStackItem);
    function Pop: TStackItem;
  end;