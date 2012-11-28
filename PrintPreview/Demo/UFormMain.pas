unit UFormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UPrintPreview;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonPreview: TButton;
    PrintPreview1: TPrintPreview;
    procedure ButtonPreviewClick(Sender: TObject);
    procedure PrintPreview1Print(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.ButtonPreviewClick(Sender: TObject);
begin
  PrintPreview1.Execute;
end;

procedure TFormMain.PrintPreview1Print(Sender: TObject);
begin
  with PrintPreview1, Canvas do begin
    Font.Name := 'Arial';
    Font.Height := -Round(Font.PixelsPerInch / SizeDivider);
    Font.Color := clBlack;
    PageTitle := 'Print preview demo';
    Line(MMToPixels(10), MMToPixels(10), MMToPixels(100), MMToPixels(100));
    TextOut(MMToPixels(50), MMToPixels(100), 'Some simple text');
    Font.Height := -Round(Font.PixelsPerInch / SizeDivider * 4);
    TextOut(MMToPixels(100), MMToPixels(150), 'Big text');
  end;
end;

end.

