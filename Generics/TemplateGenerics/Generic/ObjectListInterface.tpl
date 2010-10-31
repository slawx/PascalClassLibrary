
  TListIndex = TObjectListIndex;
  TListItem = TObjectListItem;
  {$INCLUDE 'ListInterface.tpl'}

  // TGObjectList<TObjectListIndex, TObjectListItem> = class(TGList)
  TGObjectList = class(TGList)
  private
    procedure Put(Index: TListIndex; const AValue: TListItem); override;
  public
    OwnsObjects: Boolean;
    procedure Delete(Index: TListIndex); override;
    procedure Clear; override;
    constructor Create;
    destructor Destroy; override;
  end;
