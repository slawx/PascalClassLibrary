{$IFDEF INTERFACE}

{$DEFINE TGListIndex := TGSetIndex}
{$DEFINE TGListItem := TGSetItem}
{$DEFINE TGList := TGSetList}
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

  // TGSet<TGSetIndex, TGSetItem> = class
  TGSet = class
  private
    FList: TGList;
  public
    function IsIn(Item: TGSetItem): Boolean;
    constructor Create;
    destructor Destroy; override;
    property List: TGList read FList;
  end;

{$UNDEF INTERFACE}
{$ENDIF}

{$IFDEF IMPLEMENTATION_USES}

  {$DEFINE IMPLEMENTATION_USES}
  {$I 'GenericList.inc'}

{$UNDEF IMPLEMENTATION_USES}
{$ENDIF}

{$IFDEF IMPLEMENTATION}

{$DEFINE TGListIndex := TGSetIndex}
{$DEFINE TGListItem := TGSetItem}
{$DEFINE TGList := TGSetList}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

{ TGSet }

function TGSet.IsIn(Item: TGSetItem): Boolean;
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

{$UNDEF IMPLEMENTATION}
{$ENDIF}
