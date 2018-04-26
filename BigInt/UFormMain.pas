unit UFormMain;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, UInt128;

type

  { TFormMain }

  TFormMain = class(TForm)
    ListView1: TListView;
    procedure FormShow(Sender: TObject);
  private
    procedure TestInt128;

  public
    procedure AddTest(Name: string; Result: Boolean; Value: string);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.TestInt128;
var
  A, B, C: Int128;
begin
  ListView1.Items.Clear;
  A := 123456789;
  AddTest('IntToHex(123456789)', IntToHex(A, 16) = '00000000075BCD15', IntToHex(A, 16));
  AddTest('IntToStr(123456789)', IntToStr(A) = '123456789', IntToStr(A));
  A := 21;
  B := 33;
  C := A + B;
  AddTest(IntToStr(A) + ' + ' + IntToStr(B), C = 54, IntToStr(C));
  C := A * B;
  AddTest(IntToStr(A) + ' * ' + IntToStr(B), C = 693, IntToStr(C));
  A := 30;
  B := 10;
  C := A div B;
  AddTest(IntToStr(A) + ' div ' + IntToStr(B), C = 3, IntToStr(C));
  A := $1234567812345678;
  B := 31;
  C := A shr B;
  AddTest(IntToStr(A) + ' shr ' + IntToStr(B), C = 610839792, IntToStr(C));
  A := 100000000000;
  C := -A;
  AddTest('-' + IntToStr(A), C = -100000000000, IntToStr(C));
end;

procedure TFormMain.AddTest(Name: string; Result: Boolean; Value: string);
var
  ListItem: TListItem;
begin
  ListItem := ListView1.Items.Add;
  ListItem.Caption := Name;
  if Result then ListItem.SubItems.Add('Passed')
    else ListItem.SubItems.Add('Failed');
  ListItem.SubItems.Add(Value);
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  ListView1.Items.Clear;
  TestInt128;
end;

end.

