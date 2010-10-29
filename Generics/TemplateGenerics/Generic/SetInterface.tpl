
  TListIndex = TSetIndex;
  TListItem = TSetItem;
  {$INCLUDE 'ListInterface.tpl'}

  // TGSet<TSetIndex, TSetItem> = class(TGSet)
  TGSet = class
  private
    FList: TGList;
  public
    function IsIn(Item: TSetItem): Boolean;
    constructor Create;
    destructor Destroy; override;
    property List: TGList read FList;
  end;
