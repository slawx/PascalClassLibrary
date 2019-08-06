unit UCore;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, UGeneralRegistry;

type

  { TCore }

  TCore = class(TDataModule)
    ActiveRegistry: TGeneralRegistry;
    procedure DataModuleCreate(Sender: TObject);
  private
    { private declarations }
  public
  end;

var
  Core: TCore;

implementation

{$R *.lfm}

{ TCore }

procedure TCore.DataModuleCreate(Sender: TObject);
begin
  TWinRegistry(ActiveRegistry.Backend).OpenKey('\Software\Chronosoft\RegistryDemo', True);
end;

end.

