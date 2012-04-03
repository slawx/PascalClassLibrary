unit UPDClientRegistry;

{$mode delphi}

interface

uses
  Classes, SysUtils, UPDClient, Registry;

type

  { TPDClientRegistry }

  TPDClientRegistry = class(TPDClient)
    Reg: TRegistry;
    //procedure GetItemList(Condition: TCondition; ItemList: TItemList); override;
    //procedure SetItemList(Condition: TCondition; ItemList: TItemList); override;
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TPDClientRegistry }

(*procedure TPDClientRegistry.GetItemList(Condition: TCondition;
  ItemList: TItemList);
begin
  inherited GetItemList(Condition, ItemList);
end;

procedure TPDClientRegistry.SetItemList(Condition: TCondition;
  ItemList: TItemList);
begin
  inherited SetItemList(Condition, ItemList);
end;*)

constructor TPDClientRegistry.Create;
begin
  Reg := TRegistry.Create;
  inherited Create;
end;

destructor TPDClientRegistry.Destroy;
begin
  Reg.Free;
  inherited Destroy;
end;

end.

