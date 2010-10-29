
  PGListItem = ^TListItem;
  TGList = class;

  TGListSortCompare = function(const Item1, Item2: TListItem): Integer of object;
  TGListStringConverter = function(Item: TListItem): string;
  TGListOperation = procedure(List: TGList; Item: PGListItem);
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
  public
    // All items
    procedure Reverse;
    procedure Clear;
    procedure Expand;
    procedure Sort(Compare: TGListSortCompare);
    function Implode(Separator: string; Converter: TGListStringConverter): string;
    procedure Perform(Operation: TGListOperation);
    // Many items
    procedure MoveItems(CurIndex, NewIndex, Count: TListIndex);
    procedure DeleteItems(Index, Count: TListIndex);
    procedure Fill(Start, Count: TListIndex; Value: TListItem);
    // One item
    function Add(Item: TListItem): TListIndex;
    procedure Delete(Index: TListIndex);
    function Extract(Item: TListItem): TListItem;
    procedure Exchange(Index1, Index2: TListIndex);
    function First: TListItem;
    function IndexOf(Item: TListItem; Start: TListIndex = 0): TListIndex;
    procedure Insert(Index: TListIndex; Item: TListItem);
    function Last: TListItem;
    procedure Move(CurIndex, NewIndex: TListIndex);
    function Remove(Item: TListItem): TListIndex;
    property Items[Index: TListIndex]: TListItem read Get write Put; default;
    // List
    procedure AddList(List: TGList);
    procedure Assign(List: TGList);
    function Equals(List: TGList): Boolean;
    procedure InsertList(Index: TListIndex; List: TGList);
    function IndexOfList(List: TGList; Start: TListIndex = 0): TListIndex;
    // Other
    property Count: TListIndex read GetCount write SetCount;
    property Capacity: TListIndex read GetCapacity write SetCapacity;
    // Array
    procedure AddArray(Values: array of TListItem);
    procedure SetArray(Values: array of TListItem);
    procedure InsertArray(Index: TListIndex; Values: array of TListItem);
  end;
