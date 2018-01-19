unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UCoolTranslator, ULanguages;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    CoolTranslator1: TCoolTranslator;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ListBox1: TListBox;
    ListBox2: TListBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

resourcestring
  STranslatedText = 'Text stored in resourcestring';

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  CoolTranslator1.LanguageListToStrings(ListBox1.Items);
  CoolTranslator1.ComponentExcludes.DumpToStrings(ListBox2.Items);
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  ShowMessage(MainForm.Name);
end;

procedure TMainForm.ListBox1SelectionChange(Sender: TObject; User: boolean);
begin
  if ListBox1.ItemIndex <> - 1 then
  with CoolTranslator1 do
    Language := TLanguage(ListBox1.Items.Objects[ListBox1.ItemIndex]);
end;

end.
