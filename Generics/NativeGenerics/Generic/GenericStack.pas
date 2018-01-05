unit GenericStack;

{$mode delphi}

interface

uses
  GenericList;

type
  TGStack<T> = class
  private
    FList: TGList<T>;
  public
    procedure Push(Value: T);
    function Pop: T;
    constructor Create;
    destructor Destroy; override;
    property List: TGList<T> read FList;
  end;


implementation

{ TGStack }

procedure TGStack<T>.Push(Value: T);
begin
  FList.Add(Value);
end;

function TGStack<T>.Pop: T;
begin
  Result := FList.Extract(FList.Last);
end;

constructor TGStack<T>.Create;
begin
  FList := TGList<T>.Create;
end;

destructor TGStack<T>.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

end.
