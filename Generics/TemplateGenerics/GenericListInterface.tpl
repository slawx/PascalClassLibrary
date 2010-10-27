// TGList<TIndexType, TItemType>

resourcestring
  SNotImplemented = 'Not implemented';

type
  TGListSortCompare = function(const Item1, Item2: TItemType): Integer;
  //TGListNotification = (lnAdded, lnExtracted, lnDeleted);

  TGList = class
  private
    FItems: array of TItemType;
    FCount: TIndexType;
    function Get(Index: TIndexType): TItemType;
    function GetCount: TIndexType;
    function GetCapacity: TIndexType;
    procedure SetCapacity(const AValue: TIndexType);
    procedure Put(Index: TIndexType; const AValue: TItemType);
    procedure SetCount(const AValue: TIndexType);
    procedure QuickSort(L, R : TIndexType; Compare: TGListSortCompare);
    property Capacity: TIndexType read GetCapacity write SetCapacity;
  public
    // All items
    procedure Reverse;
    procedure Clear;
    procedure Expand;
    procedure Sort(Compare: TGListSortCompare);
    function Implode(Separator: string): string;
    // Many items
    procedure MoveItems(CurIndex, NewIndex, Count: TIndexType);
    // One item
    function Add(Item: TItemType): TIndexType;
    procedure Delete(Index: TIndexType);
    procedure Exchange(Index1, Index2: TIndexType);
    function Extract(Item: TItemType): TItemType;
    function First: TItemType;
    function IndexOf(Item: TItemType): TIndexType;
    procedure Insert(Index: TIndexType; Item: TItemType);
    function Last: TItemType;
    procedure Move(CurIndex, NewIndex: TIndexType);
    function Remove(Item: TItemType): TIndexType;
    property Items[Index: TIndexType]: TItemType read Get write Put; default;
    // List
    procedure AddList(List: TGList);
    procedure Assign(List: TGList);
    procedure DeleteItems(Index, Count: TIndexType);
    function Equals(Obj: TObject): Boolean; override;
    function ExtractList(Item: TItemType; Count: TIndexType): TItemType;
    procedure InsertList(Index: TIndexType; List: TGList);
    // Other
    property Count: TIndexType read GetCount write SetCount;
    // Additional
    procedure SetArray(Values: array of TItemType);
  end;
