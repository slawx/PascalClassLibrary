unit UObjectStringList;

interface

uses
  SysUtils, Classes, Types, UObjectTypeBase, UObjectBoolean, UObjectString;

type
  TStringList = class(TInterfacedObject, IAssignable, IComparable)
  private
    Value: TList; // TList<TString>
  public
    procedure Explode(Text: TString; Separator: TString);
    function Implode(Separator: TString): TString;
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TInterfacedObject);
    function EqualTo(Operand: IComparable): TBoolean;
  end;

implementation

{ TStringList }

procedure TStringList.Assign(Source: TInterfacedObject);
begin

end;

constructor TStringList.Create;
begin
  Value := TList.Create;
end;

destructor TStringList.Destroy;
var
  I: Integer;
begin
  for I := 0 to Value.Count - 1 do
    TString(Value[I]).Free;
  Value.Destroy;
  inherited;
end;

function TStringList.EqualTo(Operand: IComparable): TBoolean;
begin

end;

procedure TStringList.Explode(Text, Separator: TString);
begin

end;

function TStringList.Implode(Separator: TString): TString;
begin

end;

end.
