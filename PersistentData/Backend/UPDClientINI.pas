unit UPDClientINI;

{$mode delphi}

interface

uses
  Classes, SysUtils, UPDClient;

type

  { TPDClientINI }

  TPDClientINI = class(TPDClient)
    //procedure GetItemList(Condition: TCondition; ItemList: TItemList); override;
    //procedure SetItemList(Condition: TCondition; ItemList: TItemList); override;
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TPDClientINI }

(*procedure TPDClientINI.GetItemList(Condition: TCondition;
  ItemList: TItemList);
begin
  inherited GetItemList(Condition, ItemList);
end;

procedure TPDClientINI.SetItemList(Condition: TCondition;
  ItemList: TItemList);
begin
  inherited SetItemList(Condition, ItemList);
end;*)

constructor TPDClientINI.Create;
begin
  inherited Create;
end;

destructor TPDClientINI.Destroy;
begin
  inherited Destroy;
end;

end.

