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
  end;


implementation


{ TProject }

constructor TProject.Create;
begin
  inherited Create;
end;

function TProject.GetFileExt: string;
begin
  Result := '.prj';
end;

end.

