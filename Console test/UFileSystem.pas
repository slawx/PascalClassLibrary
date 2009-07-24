unit UFileSystem;

interface

uses
  SysUtils;

type
  TDirectory = class
    Path: string;
    procedure Make;
    procedure Remove;
    function Exists: Boolean;
  end;

implementation

{ TDirectory }

function TDirectory.Exists: Boolean;
begin
  Result := DirectoryExists(Path);
end;

procedure TDirectory.Make;
begin
  MkDir(Path);
end;

procedure TDirectory.Remove;
begin
  RmDir(Path);
end;

end.
