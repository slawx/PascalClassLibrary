unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  GenericList;

type
  { TMainForm }

  TMainForm = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    IntegerList: TIntegerList;
    StringList: TStringList;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  IntegerList := TIntegerList.Create;
  StringList := TStringList.Create;
end;

function IntegerListCompare(const Item1, Item2: Integer): Integer;
begin
  if Item2 < Item1 then Result := 1
  else if Item2 > Item1 then Result := -1
  else Result := 0;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  I: Integer;
begin
  IntegerList.Add(1);
  IntegerList.Add(3);
  IntegerList.Add(12121);
  IntegerList.Add(5);
  StringList.Add('One');
  StringList.Add('Two');
  StringList.Add('Three');

  for I := 0 to StringList.Count - 1 do
    Memo1.Lines.Add(StringList[I]);

  IntegerList.Sort(@IntegerListCompare);
  for I := 0 to IntegerList.Count - 1 do
    Memo1.Lines.Add(IntToStr(IntegerList[I]));
end;

end.

