unit UDpiFormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UDpiControls, Dialogs;

type

  { TDpiFormMain }

  TDpiFormMain = class(TDpiForm)
    DpiButton1: TDpiButton;
    procedure DpiButton1Click(Sender: TObject);
    procedure DpiFormMainCreate(Sender: TObject);
  private

  public

  end;

var
  DpiFormMain: TDpiFormMain;

implementation

{$R *.lfm}

{ TDpiFormMain }

procedure TDpiFormMain.DpiFormMainCreate(Sender: TObject);
var
  DpiButton: TDpiButton;
  DpiImage: TDpiImage;
begin
  DpiButton := TDpiButton.Create(DpiFormMain);
  DpiButton.Parent := Self;
  DpiButton.SetBounds(10, 10, 100, 30);
  DpiButton.Caption := 'Click me';
  DpiButton.Visible := True;
  DpiButton1.Parent := Self;

  DpiImage := TDpiImage.Create(DpiFormMain);
  DpiImage.Parent := Self;
  DpiImage.SetBounds(150, 10, 100, 100);
  DpiImage.Visible := True;
  DpiImage.Stretch := True;
  DpiImage.VclImage.Picture.LoadFromFile('dance.jpg');
end;

procedure TDpiFormMain.DpiButton1Click(Sender: TObject);
begin
  ShowMessage('Hello');
end;

end.

