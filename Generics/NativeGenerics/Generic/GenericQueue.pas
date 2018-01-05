unit GenericQueue;

{$mode delphi}

interface

uses
  GenericList;

type
  TGQueue<T> = class
  private
    FList: TGList<T>;
    function GetCount: Integer;
  public
    procedure Enqueue(Value: T);
    procedure EnqueueArray(Values: array of T);
    procedure EnqueueList(List: TGList<T>);
    function Dequeue: T;
    function Peek: T;
    constructor Create;
    destructor Destroy; override;
    property List: TGList<T> read FList;
    property Count: Integer read GetCount;
  end;


implementation

{ TGQueue }

procedure TGQueue<T>.Enqueue(Value: T);
begin
  FList.Add(Value);
end;

procedure TGQueue<T>.EnqueueArray(Values: array of T);
begin
  FList.AddArray(Values);
end;

procedure TGQueue<T>.EnqueueList(List: TGList<T>);
begin
  FList.AddList(List);
end;

function TGQueue<T>.Peek: T;
begin
  Result := FList.First;
end;

constructor TGQueue<T>.Create;
begin
  FList := TGList<T>.Create;
end;

destructor TGQueue<T>.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TGQueue<T>.Dequeue: T;
begin
  Result := FList.Extract(FList.First);
end;

function TGQueue<T>.GetCount: Integer;
begin
  Result := FList.Count;
end;

end.
