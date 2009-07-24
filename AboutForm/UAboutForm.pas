unit UAboutForm;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, UApplicationInfo;

type
  TAboutForm = class(TForm)
    Panel1: TPanel;
    OKButton: TButton;
    Image1: TImage;
    Memo1: TMemo;
    procedure OKButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.dfm}

procedure TAboutForm.FormShow(Sender: TObject);
begin
  with Memo1, Lines, ApplicationInfo do begin
    BeginUpdate;
    Clear;
    Add('Název aplikace: ' + Name);
    Add('Verze: ' + IntToStr(MajorVersion) + '.' +
      IntToStr(MinorVersion));
    Add('Datum uvolnìní: ' + ReleaseDate);
    Add('Firma: ' + CompanyName);
    Add('Autor: ' + AuthorName);
    Add('Email: ' + EmailContact);
    EndUpdate;
  end;
  FocusControl(OKButton);
end;

procedure TAboutForm.OKButtonClick(Sender: TObject);
begin
  Close;
end;

end.
 
