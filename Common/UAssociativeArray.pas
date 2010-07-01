unit UAssociativeArray;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils;

type 
  TAssociativeArray = class(TStringList)
  private
    function GetValues(Index: string): string;
    function GetValuesAtIndex(Index: Integer): string;
    procedure SetValues(Index: string; const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    function GetAllValues: string;
    procedure AddKeyValue(Key, Value: string);
    property ValuesAtIndex[Index: Integer]: string read GetValuesAtIndex;
    property Values[Index: string]: string read GetValues write SetValues; default;
  end;

implementation      

{ TAssociativeArray }

procedure TAssociativeArray.SetValues(Index: string; const Value: string);
begin
  inherited Values[Index] := Value;
end;

procedure TAssociativeArray.AddKeyValue(Key, Value: string);
begin
  Add(Key + NameValueSeparator + Value);
end;

constructor TAssociativeArray.Create;
begin
  NameValueSeparator := '|';
end;

destructor TAssociativeArray.Destroy;
begin
  inherited;
end;

function TAssociativeArray.GetAllValues: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do begin
    Result := Result + Names[I] + '=' + ValuesAtIndex[I] + ',';
  end;
end;

function TAssociativeArray.GetValues(Index: string): string;
begin
  Result := inherited Values[Index];
end;

function TAssociativeArray.GetValuesAtIndex(Index: Integer): string;
begin
  Result := inherited Values[Names[Index]];
end;


end.
