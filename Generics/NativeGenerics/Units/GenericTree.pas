unit GenericTree;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes, GenericList;

type
  //TGAbstractTree<TItem> = class;

  { TGAbstractTreeNode }

  TGAbstractTreeNode<TItem> = class
  public
    type
      TIndex = NativeInt;
      TNode = TGAbstractTreeNode<TItem>;
  protected
    function GetCount: TIndex; virtual; abstract;
    function GetItem(Index: TIndex): TItem; virtual; abstract;
    function GetValue: TItem; virtual; abstract;
    procedure SetCount(AValue: TIndex); virtual; abstract;
    procedure SetItem(Index: TIndex; AValue: TItem); virtual; abstract;
    procedure SetValue(AValue: TItem); virtual; abstract;
  public
    Parent: TGAbstractTreeNode<TItem>;
    //Tree: TGAbstractTree<TItem>;
    procedure Clear; virtual; abstract;
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(Item: TItem): TIndex; virtual; abstract;
    property Count: TIndex read GetCount write SetCount;
    property Items[Index: TIndex]: TItem read GetItem write SetItem;
    property Value: TItem read GetValue write SetValue;
  end;

  { TGAbstractTree }

  TGAbstractTree<TItem> = class
  public
    type
      TIndex = NativeInt;
      TNode = TGAbstractTreeNode<TItem>;
  private
  public
    TopItem: TGAbstractTreeNode<TItem>;
    constructor Create; virtual;
  end;


  { TGTreeNode }

  TGTreeNode<TItem> = class(TGAbstractTreeNode<TItem>)
  public
    type
      TNode = TGAbstractTreeNode<TItem>;
  private
    FValue: TItem;
    FItems: TGAbstractList<TNode>;
  protected
    function GetItem(Index: TIndex): TItem; override;
    procedure SetItem(Index: TIndex; AValue: TItem); override;
    function GetCount: TIndex; override;
    procedure SetCount(AValue: TIndex); override;
    function GetValue: TItem; override;
    procedure SetValue(AValue: TItem); override;
  public
    function Add(Item: TItem): TIndex; override;
    procedure Clear; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TGTree<TItem> = class(TGAbstractTree<TItem>)
  public
    constructor Create; override;
    destructor Destroy; override;
  end;


implementation

{ TGTreeNode }

function TGTreeNode<TItem>.GetItem(Index: TIndex): TItem;
begin
  Result := FItems[Index];
end;

procedure TGTreeNode<TItem>.SetItem(Index: TIndex; AValue: TItem);
begin
  FItems[Index] := AValue;
end;

function TGTreeNode<TItem>.GetCount: TIndex;
begin
  Result := FItems.Count;
end;

procedure TGTreeNode<TItem>.SetCount(AValue: TIndex);
begin
  FItems.Count := AValue;
end;

function TGTreeNode<TItem>.GetValue: TItem;
begin
  Result := FValue;
end;

procedure TGTreeNode<TItem>.SetValue(AValue: TItem);
begin
  FValue := AValue;
end;

function TGTreeNode<TItem>.Add(Item: TItem): TIndex;
begin
  Result := FItems.Add(Item);
end;

procedure TGTreeNode<TItem>.Clear;
begin
  //Childs.Clear;
end;

constructor TGTreeNode<TItem>.Create;
begin
  inherited;
  FItems := TGList<TNode>.Create;
end;

destructor TGTreeNode<TItem>.Destroy;
begin
  FItems.Free;
  inherited;
end;

{ TGTree }

constructor TGTree<TItem>.Create;
begin
  inherited;
  TopItem := TGTreeNode<TItem>.Create;
end;

destructor TGTree<TItem>.Destroy;
begin
  inherited Destroy;
  TopItem.Free;
end;

{ TGAbstractTree<TItem> }

constructor TGAbstractTree<TItem>.Create;
begin
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
