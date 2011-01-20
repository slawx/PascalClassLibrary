unit GenericQueue;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes, GenericList;

type
  TGQueue<TItem> = class
  private
  type
    TGQueueList = TGList<TItem>;
  var
    FList: TGQueueList;
  public
    procedure Enqueue(Value: TItem);
    function Dequeue: TItem;
    function Peek: TItem;
    constructor Create;
    destructor Destroy; override;
    property List: TGQueueList read FList;
  end;

implementation

{ TGQueue }

procedure TGQueue<TItem>.Enqueue(Value: TItem);
begin
  FList.Add(Value);
end;

function TGQueue<TItem>.Peek: TItem;
begin
  Result := FList.First;
end;

constructor TGQueue<TItem>.Create;
begin
  FList := TGList.Create;
end;

destructor TGQueue<TItem>.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TGQueue<TItem>.Dequeue: TItem;
begin
  Result := FList.Extract(FList.First);
end;

end.
