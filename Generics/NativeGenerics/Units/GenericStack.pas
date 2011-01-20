unit GenericStack;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes, GenericList;

type
  TGStack<TItem> = class
  private
  type
    TGStackList = TGList<TItem>;
  var
    FList: TGStackList;
  public
    procedure Push(Value: TItem);
    function Pop: TItem;
    constructor Create;
    destructor Destroy; override;
    property List: TGStackList read FList;
  end;

implementation
{ TGStack }

procedure TGStack<TItem>.Push(Value: TItem);
begin
  FList.Add(Value);
end;

function TGStack<TItem>.Pop: TItem;
begin
  Result := FList.Extract(FList.Last);
end;

constructor TGStack<TItem>.Create;
begin
  FList := TGList.Create;
end;

destructor TGStack<TItem>.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

end.
