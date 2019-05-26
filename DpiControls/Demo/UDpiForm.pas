unit UDpiForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UDpiControls, Dialogs;

type

  { TDpiForm1 }

  TDpiForm1 = class(TDpiForm)
    DpiButton1: TDpiButton;
    procedure DpiButton1Click(Sender: TObject);
    procedure DpiForm1Show(Sender: TObject);
  private

  public

  end;

var
  DpiForm1: TDpiForm1;

implementation

{$R *.lfm}

{ TDpiForm1 }

procedure TDpiForm1.DpiForm1Show(Sender: TObject);
var
  DpiButton: TDpiButton;
begin
  DpiButton := TDpiButton.Create(DpiForm1);
  DpiButton.Parent := DpiForm1;
  DpiButton.SetBounds(10, 10, 100, 30);
  DpiButton.Caption := 'Click me';
  DpiButton.Visible := True;
  DpiButton1.Parent := Self;
end;

procedure TDpiForm1.DpiButton1Click(Sender: TObject);
begin
  ShowMessage('Hello');
end;

end.

