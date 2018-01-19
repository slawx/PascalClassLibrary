{$IFDEF INTERFACE}

{$DEFINE TGListIndex := TGStackIndex}
{$DEFINE TGListItem := TGStackItem}
{$DEFINE TGList := TGStackList}
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

  // TGStack<TStackIndex, TStackItem> = class(TGList)
  TGStack = class
  private
    FList: TGList;
  public
    procedure Push(Value: TGStackItem);
    function Pop: TGStackItem;
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

{$DEFINE TGListIndex := TGStackIndex}
{$DEFINE TGListItem := TGStackItem}
{$DEFINE TGList := TGStackList}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

{ TGStack }

procedure TGStack.Push(Value: TGStackItem);
begin
  FList.Add(Value);
end;

function TGStack.Pop: TGStackItem;
begin
  Result := FList.Extract(FList.Last);
end;

constructor TGStack.Create;
begin
  FList := TGList.Create;
end;

destructor TGStack.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

{$UNDEF IMPLEMENTATION}
{$ENDIF}
