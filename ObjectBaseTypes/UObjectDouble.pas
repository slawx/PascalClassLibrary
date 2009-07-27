unit UObjectDouble;

interface

uses
  UObjectNumber, UObjectTypeBase, UObjectBoolean;

type
  TDouble = class(TInterfacedObject, IAssignable, IOrderable)
    procedure Assign(Source: TInterfacedObject);
    function HigherThen(Operand: IOrderable): TBoolean;
    function LowerThan(Operand: IOrderable): TBoolean;
    function Max(Operand1, Operand2: IOrderable): IOrderable;
    function Min(Operand1, Operand2: IOrderable): IOrderable;
  end;

implementation

{ TDouble }

procedure TDouble.Assign(Source: TInterfacedObject);
begin

end;

function TDouble.HigherThen(Operand: IOrderable): TBoolean;
begin

end;

function TDouble.LowerThan(Operand: IOrderable): TBoolean;
begin

end;

function TDouble.Max(Operand1, Operand2: IOrderable): IOrderable;
begin

end;

function TDouble.Min(Operand1, Operand2: IOrderable): IOrderable;
begin

end;

end.
