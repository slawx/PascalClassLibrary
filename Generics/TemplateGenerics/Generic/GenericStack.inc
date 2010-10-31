{$IFDEF INTERFACE}

  TListIndex = TStackIndex;
  TListItem = TStackItem;
  {$DEFINE INTERFACE}
  {$INCLUDE 'GenericList.inc'}

  // TGStack<TStackIndex, TStackItem> = class(TGList)
  TGStack = class
  private
    FList: TGList;
  public
    procedure Push(Value: TStackItem);
    function Pop: TStackItem;
    constructor Create;
    destructor Destroy; override;
    property List: TGList read FList;
  end;
{$UNDEF INTERFACE}
{$ENDIF}

{$IFDEF IMPLEMENTATION}

{$DEFINE IMPLEMENTATION}
{$INCLUDE 'GenericList.inc'}

{ TGStack }

procedure TGStack.Push(Value: TStackItem);
begin
  FList.Add(Value);
end;

function TGStack.Pop: TStackItem;
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

{$ENDIF}
