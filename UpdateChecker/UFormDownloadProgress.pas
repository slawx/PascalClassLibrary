unit UFormDownloadProgress;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls;

type

  { TFormDownloadProgress }

  TFormDownloadProgress = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    LabelProgress: TLabel;
    LabelFileName: TLabel;
    ProgressBar1: TProgressBar;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{ TFormDownloadProgress }

procedure TFormDownloadProgress.FormCreate(Sender: TObject);
begin
  DoubleBuffered := True;
  ControlStyle := ControlStyle + [csOpaque];
end;

initialization
  {$I UFormDownloadProgress.lrs}

end.

