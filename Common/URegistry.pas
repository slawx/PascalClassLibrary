unit URegistry;

{$MODE Delphi}

interface

uses
  Classes, Registry;

type

  { TRegistryEx }

  TRegistryEx = class(TRegistry)
  private
  public
    function ReadBoolWithDefault(const Name: string;
      DefaultValue: Boolean): Boolean;
    function ReadIntegerWithDefault(const Name: string; DefaultValue: Integer): Integer;
    function ReadStringWithDefault(const Name: string; DefaultValue: string): string;
    function ReadFloatWithDefault(const Name: string;
      DefaultValue: Double): Double;
    function DeleteKeyRecursive(const Key: string): Boolean;
  end;

implementation

{ TRegistryEx }

function TRegistryEx.ReadIntegerWithDefault(const Name: string;
  DefaultValue: Integer): Integer;
begin
  if ValueExists(Name) then Result := ReadInteger(Name)
    else begin
      WriteInteger(Name, DefaultValue);
      Result := DefaultValue;
    end;
end;

function TRegistryEx.ReadStringWithDefault(const Name: string;
  DefaultValue: string): string;
begin
  if ValueExists(Name) then Result := ReadString(Name)
    else begin
      WriteString(Name, DefaultValue);
      Result := DefaultValue;
    end;
end;

function TRegistryEx.ReadFloatWithDefault(const Name: string;
  DefaultValue: Double): Double;
begin
  if ValueExists(Name) then Result := ReadFloat(Name)
    else begin
      WriteFloat(Name, DefaultValue);
      Result := DefaultValue;
    end;
end;

function TRegistryEx.DeleteKeyRecursive(const Key: string): Boolean;
var
  SubKeys: TStringList;
  I: Integer;
begin
  try
    SubKeys := TStringList.Create;
    if OpenKey(Key, False) and HasSubKeys then begin
      GetKeyNames(SubKeys);
      for I := 0 to SubKeys.Count - 1 do
        DeleteKeyRecursive(Key + '\' + SubKeys[I]);
    end;
    Result := DeleteKey(Key);
  finally
    SubKeys.Free;
  end;
end;

function TRegistryEx.ReadBoolWithDefault(const Name: string;
  DefaultValue: Boolean): Boolean;
begin
  if ValueExists(Name) then Result := ReadBool(Name)
    else begin
      WriteBool(Name, DefaultValue);
      Result := DefaultValue;
    end;
end;

end.
