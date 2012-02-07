unit GenericSet;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes, GenericList;

type
  TGSet<TItem> = class
  private
  var
    FList: TGList<TItem>;
  public
    function IsIn(Item: TItem): Boolean;
    constructor Create;
    destructor Destroy; override;
    property List: TGList<TItem> read FList;
  end;

implementation

{ TGSet }

function TGSet<TItem>.IsIn(Item: TItem): Boolean;
begin
  Result := FList.IndexOf(Item) <> -1;
end;

constructor TGSet<TItem>.Create;
begin
  FList := TGList<TItem>.Create;
end;

destructor TGSet<TItem>.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

end.
