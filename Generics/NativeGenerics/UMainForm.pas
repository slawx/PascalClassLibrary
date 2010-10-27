unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  GenericList, GenericSet;

type

  { TMyObject }

  TMyObject = class
    Name: string;
    constructor Create(AName: string);
  end;

  TMyObjectList = specialize TGList<Integer, TMyObject>;

  { TMainForm }

  TMainForm = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    IntegerList: TIntegerGList;
    StringList: TStringGList;
    MyObjectList: TMyObjectList;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  IntegerList := TIntegerGList.Create;
  StringList := TStringGList.Create;
  MyObjectList := TMyObjectList.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  IntegerList.Destroy;
  StringList.Destroy;
  MyObjectList.Destroy;
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

  for I := 0 to StringList.Count - 1 do
    Memo1.Lines.Add(StringList[I]);

  StringList.Add('One');
  StringList.Add('Two');
  StringList.Add('Three');

  IntegerList.Sort(@IntegerListCompare);
  for I := 0 to IntegerList.Count - 1 do
    Memo1.Lines.Add(IntToStr(IntegerList[I]));

  MyObjectList.Add(TMyObject.Create('Object 1'));
  MyObjectList.Add(TMyObject.Create('Object 2'));
  MyObjectList.Add(TMyObject.Create('Object 3'));

  for I := 0 to MyObjectList.Count - 1 do
    Memo1.Lines.Add(MyObjectList[I].Name);
end;

{ TObjectItem }

constructor TMyObject.Create(AName: string);
begin
  Name := AName;
end;

end.

