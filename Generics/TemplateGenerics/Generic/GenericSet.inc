{$IFDEF INTERFACE}

  TListIndex = TSetIndex;
  TListItem = TSetItem;
  {$DEFINE INTERFACE}
  {$INCLUDE 'GenericList.inc'}

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
{$UNDEF INTERFACE}
{$ENDIF}

{$IFDEF IMPLEMENTATION}

{$DEFINE IMPLEMENTATION}
{$INCLUDE 'GenericList.inc'}

{ TGSet }

function TGSet.IsIn(Item: TSetItem): Boolean;
begin
  Result := FList.IndexOf(Item) <> -1;
end;

constructor TGSet.Create;
begin
  FList := TGList.Create;
end;

destructor TGSet.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

{$ENDIF}
