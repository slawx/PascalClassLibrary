unit UFormAbout;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, ExtCtrls, UApplicationInfo, UCommon;

type

  { TFormAbout }

  TFormAbout = class(TForm)
    ButtonHomePage: TButton;
    ButtonClose: TButton;
    Image1: TImage;
    LabelAppName: TLabel;
    LabelDescription: TLabel;
    LabelContent: TLabel;
    PanelTitle: TPanel;
    procedure ButtonHomePageClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    ApplicationInfo: TApplicationInfo;
  end;

var
  FormAbout: TFormAbout;

implementation

{$R *.lfm}

uses
  UCore;

resourcestring
  SVersion = 'Version';
  SReleaseDate = 'Release date';
  SLicense = 'License';

{ TFormAbout }

procedure TFormAbout.FormShow(Sender: TObject);
begin
  if Assigned(ApplicationInfo) then begin
    LabelAppName.Caption := ApplicationInfo.AppName;
    LabelAppName.AutoSize := True;
    LabelDescription.Caption := ApplicationInfo.Description;
    LabelContent.Caption := SVersion + ': ' + ApplicationInfo.Version + LineEnding +
      SReleaseDate + ': ' + DateToStr(ApplicationInfo.ReleaseDate) + LineEnding +
      SLicense + ': ' + ApplicationInfo.License;
    Image1.Picture.Bitmap.Assign(Application.Icon);
  end;
end;

procedure TFormAbout.ButtonHomePageClick(Sender: TObject);
begin
  if Assigned(ApplicationInfo) then
    OpenWebPage(ApplicationInfo.HomePage);
end;

procedure TFormAbout.FormCreate(Sender: TObject);
begin
  Core.CoolTranslator1.TranslateComponentRecursive(Self);
end;

end.

