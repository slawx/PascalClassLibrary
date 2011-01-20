unit GenericSet;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes, GenericList;

type
  TGSet<TItem> = class
  private
  type
    TGSetList = TGList<TItem>;
  var
    FList: TGSetList;
  public
    function IsIn(Item: TItem): Boolean;
    constructor Create;
    destructor Destroy; override;
    property List: TGSetList read FList;
  end;

implementation

{ TGSet }

function TGSet<TItem>.IsIn(Item: TItem): Boolean;
begin
  Result := FList.IndexOf(Item) <> -1;
end;

constructor TGSet<TItem>.Create;
begin
  FList := TGList.Create;
end;

destructor TGSet<TItem>.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

end.
