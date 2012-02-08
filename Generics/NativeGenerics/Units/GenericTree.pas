unit GenericTree;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes, GenericList;

type

  { TGAbstractTreeNode }

  TGAbstractTreeNode<TItem> = class
  private
    function GetValue: TItem; virtual; abstract;
    procedure SetValue(AValue: TItem); virtual; abstract;
  public
    type
      TNode = TGAbstractTreeNode<TItem>;
  var
    Childs: TGAbstractList<TNode>;
    procedure Clear; virtual; abstract;
    constructor Create; virtual;
    destructor Destroy; override;
    property Value: TItem read GetValue write SetValue;
  end;

  { TGAbstractTree }

  TGAbstractTree<TItem> = class
  public
    type
      TNode = TGAbstractTreeNode<TItem>;
      TIndex = NativeInt;
  private
    function GetItem(Index: TIndex): TItem;
    procedure SetItem(Index: TIndex; AValue: TItem);
  var
    TopItem: TGAbstractTreeNode<TItem>;
    procedure Clear; virtual; abstract;
    constructor Create; virtual;
    property Items[Index: TIndex]: TItem read GetItem write SetItem;
  end;


  TGTreeNode<TItem> = class(TGAbstractTreeNode<TItem>)
  public
    type
      TNode = TGTreeNode<TItem>;
  private
    FValue: TItem;
    function GetValue: TItem; override;
    procedure SetValue(AValue: TItem); override;
  public
    procedure Clear; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TGTree<TItem> = class
  public
    type
      TNode = TGTreeNode<TItem>;
  private
    FItems: TGList<TNode>;
  public
    procedure Clear; override;
    constructor Create; override;
    destructor Destroy; override;
  end;


implementation

{ TGTreeNode }

function TGTreeNode<TItem>.GetValue: TItem;
begin
  Result := FValue;
end;

procedure TGTreeNode<TItem>.SetValue(AValue: TItem);
begin
  FValue := AValue;
end;

procedure TGTreeNode<TItem>.Clear;
begin
  //Childs.Clear;
end;

{ TGTree }

procedure TGTree<TItem>.Clear;
begin
  //if Assigned(TopItem) then TopItem.Clear;
end;

constructor TGTree<TItem>.Create;
begin
  inherited;
  FItems := TGList<TNode>.Create;
end;

destructor TGTree<TItem>.Destroy;
begin
  inherited Destroy;
  FItems.Free;
end;

{ TGAbstractTree<TItem> }

function TGAbstractTree<TItem>.GetItem(Index: TIndex): TItem;
begin

end;

procedure TGAbstractTree<TItem>.SetItem(Index: TIndex; AValue: TItem);
begin

end;

constructor TGAbstractTree<TItem>.Create;
begin
end;

constructor TGTreeNode<TItem>.Create;
begin
  inherited;
  Childs := TGList<TNode>.Create;
end;

destructor TGTreeNode<TItem>.Destroy;
begin
  Childs.Free;
  inherited;
end;

{ TGAbstractTreeNode<TItem> }

constructor TGAbstractTreeNode<TItem>.Create;
begin
end;

destructor TGAbstractTreeNode<TItem>.Destroy;
begin
  inherited Destroy;
end;


end.
