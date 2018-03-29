unit UProject;

{$mode delphi}

interface

uses
  Classes, SysUtils, UDataFile;

type

  { TProject }

  TProject = class(TDataFile)
  private
  public
    constructor Create; override;
    function GetFileExt: string; override;
    function GetFileDialogFilter: string; override;
  end;


implementation

resourcestring
  SProjectName = 'New project';
  SProjectFiles = 'Project files';


{ TProject }

constructor TProject.Create;
begin
  inherited Create;
  FileName := SProjectName + GetFileExt;
end;

function TProject.GetFileExt: string;
begin
  Result := '.prj';
end;

function TProject.GetFileDialogFilter: string;
begin
  Result := SProjectFiles + ' (' + GetFileExt + ')|*' + GetFileExt + '|' +
    SAllFiles + '|*.*';
end;

end.

