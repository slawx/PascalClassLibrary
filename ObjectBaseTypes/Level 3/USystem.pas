unit USystem;

interface

uses
  Types, SysUtils, UFileSystem;

type
  TSystem = class
    FileSystem: TFileSystem;
    procedure Sleep(Delay: Cardinal);
    procedure Abort;
    function GetCurrentTime: TDateTime; 
    constructor Create;
    destructor Destroy; override;
  end;
  
var
  System: TSystem;

implementation

{ TSystem }

procedure TSystem.Abort;
begin
  SysUtils.Abort;
end;

constructor TSystem.Create;
begin
  FileSystem := TFileSystem.Create;
  FileSystem.Drive := 3;
end;

destructor TSystem.Destroy;
begin
  FileSystem.Free;
  inherited;
end;

function TSystem.GetCurrentTime: TDateTime;
begin
  Result := SysUtils.Now;
end;

procedure TSystem.Sleep(Delay: Cardinal);
begin
  SysUtils.Sleep(Delay);
end;

end.
