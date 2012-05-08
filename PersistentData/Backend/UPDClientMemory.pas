unit UPDClientMemory;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, UPDClient, SpecializedDictionary, SpecializedList;

type

  { TPDClientMemory }

  TPDClientMemory = class(TPDClient)
  protected
    procedure InitSystemTypes; override;
    function GetConnected: Boolean; override;
    procedure Init; override;
  public
    Objects: TListObject;
    procedure ObjectLoad(AObject: TObjectProxy); override;
    procedure ObjectSave(AObject: TObjectProxy); override;
    procedure ObjectDelete(AObject: TObjectProxy); override;
    procedure ListLoad(AList: TListProxy); override;
    procedure ListSave(AList: TListProxy); override;
    procedure TypeDefine(AType: TPDType); override;
    procedure TypeUndefine(AType: TPDType); override;
    function TypeIsDefined(AType: TPDType): Boolean; override;
    procedure Install;
    procedure Uninstall;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Connect; override;
    procedure Disconnect; override;
  end;

implementation

{ TPDClientMemory }

procedure TPDClientMemory.InitSystemTypes;
begin
  inherited InitSystemTypes;
end;

function TPDClientMemory.GetConnected: Boolean;
begin
  Result:=inherited GetConnected;
end;

procedure TPDClientMemory.Init;
begin
  inherited Init;
end;

procedure TPDClientMemory.ObjectLoad(AObject: TObjectProxy);
begin

end;

procedure TPDClientMemory.ObjectSave(AObject: TObjectProxy);
begin

end;

procedure TPDClientMemory.ObjectDelete(AObject: TObjectProxy);
begin

end;

procedure TPDClientMemory.ListLoad(AList: TListProxy);
begin

end;

procedure TPDClientMemory.ListSave(AList: TListProxy);
begin

end;

procedure TPDClientMemory.TypeDefine(AType: TPDType);
begin

end;

procedure TPDClientMemory.TypeUndefine(AType: TPDType);
begin

end;

function TPDClientMemory.TypeIsDefined(AType: TPDType): Boolean;
begin

end;

procedure TPDClientMemory.Install;
begin

end;

procedure TPDClientMemory.Uninstall;
begin

end;

constructor TPDClientMemory.Create(AOwner: TComponent);
begin
  inherited;
  Objects := TListObject.Create;
  BackendName := 'Memory';
end;

destructor TPDClientMemory.Destroy;
begin
  Objects.Free;
  inherited Destroy;
end;

procedure TPDClientMemory.Connect;
begin
  inherited Connect;
end;

procedure TPDClientMemory.Disconnect;
begin
  inherited Disconnect;
end;

end.

