{$IFDEF INTERFACE}

  { TGAbstractList }

  TGAbstractList = class
  private
    FOnUpdate: TNotifyEvent;
    function Get(const Index: Integer): Integer; virtual; abstract;
    function GetCapacity: Integer; virtual; abstract;
    function GetCount: Integer; virtual; abstract;
    function GetFirst: Integer; virtual; abstract;
    function GetLast: Integer; virtual; abstract;
    procedure Put(const Index: Integer; const AValue: Integer); virtual; abstract;
    procedure SetCapacity(const AValue: Integer); virtual; abstract;
    procedure SetCount(const AValue: Integer); virtual; abstract;
    procedure SetFirst(const AValue: Integer); virtual; abstract;
    procedure SetLast(const AValue: Integer); virtual; abstract;
  public
    type
      PItem = ^TGListItem;
    property Count: TGListIndex read GetCount write SetCount;
    property Capacity: TGListIndex read GetCapacity write SetCapacity;
    property Items[Index: TGListIndex]: TGListItem read Get write Put; default;
    property First: TGListItem read GetFirst write SetFirst;
    property Last: TGListItem read GetLast write SetLast;
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  end;


{$UNDEF INTERFACE}
{$ENDIF}

{$IFDEF IMPLEMENTATION}
{$UNDEF IMPLEMENTATION}
{$ENDIF}

