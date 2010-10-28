
  TGListSortCompare = function(const Item1, Item2: TListItem): Integer of object;
  TGListStringConverter = function(Item: TListItem): string;
  //TGListNotification = (lnAdded, lnExtracted, lnDeleted);

  // TGList<TListIndex, TListItem> = class
  TGList = class
  private
    FItems: array of TListItem;
    FCount: TListIndex;
    function Get(Index: TListIndex): TListItem;
    function GetCount: TListIndex;
    function GetCapacity: TListIndex;
    procedure SetCapacity(const AValue: TListIndex);
    procedure Put(Index: TListIndex; const AValue: TListItem);
    procedure SetCount(const AValue: TListIndex);
    procedure QuickSort(L, R : TListIndex; Compare: TGListSortCompare);
    property Capacity: TListIndex read GetCapacity write SetCapacity;
  public
    // All items
    procedure Reverse;
    procedure Clear;
    procedure Expand;
    procedure Sort(Compare: TGListSortCompare);
    function Implode(Separator: string; Converter: TGListStringConverter): string;
    // Many items
    procedure MoveItems(CurIndex, NewIndex, Count: TListIndex);
    procedure Exchange(Index1, Index2: TListIndex);
    // One item
    function Add(Item: TListItem): TListIndex;
    procedure Delete(Index: TListIndex);
    function Extract(Item: TListItem): TListItem;
    function First: TListItem;
    function IndexOf(Item: TListItem): TListIndex;
    procedure Insert(Index: TListIndex; Item: TListItem);
    function Last: TListItem;
    procedure Move(CurIndex, NewIndex: TListIndex);
    function Remove(Item: TListItem): TListIndex;
    property Items[Index: TListIndex]: TListItem read Get write Put; default;
    // List
    procedure AddList(List: TGList);
    procedure Assign(List: TGList);
    procedure DeleteItems(Index, Count: TListIndex);
    //function Equals(Obj: TObject): Boolean; override;
    procedure InsertList(Index: TListIndex; List: TGList);
    // Other
    property Count: TListIndex read GetCount write SetCount;
    // Additional
    procedure AddArray(Values: array of TListItem);
  end;