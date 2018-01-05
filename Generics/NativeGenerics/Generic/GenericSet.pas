unit GenericSet;

{$mode delphi}

interface

uses
  GenericList;

type
  TGSet<T> = class
  private
    FList: TGList<T>;
  public
    function IsIn(Item: T): Boolean;
    constructor Create;
    destructor Destroy; override;
    property List: TGList<T> read FList;
  end;

implementation

{ TGSet }

function TGSet<T>.IsIn(Item: T): Boolean;
begin
  Result := FList.IndexOf(Item) <> -1;
end;

constructor TGSet<T>.Create;
begin
  FList := TGList<T>.Create;
end;

destructor TGSet<T>.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

end.
