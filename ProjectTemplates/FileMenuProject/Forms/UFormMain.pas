unit UFormMain;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls;

type

  { TFormMain }

  TFormMain = class(TForm)
    CoolBar1: TCoolBar;
    MainMenu1: TMainMenu;
    MenuItemToolbar: TMenuItem;
    MenuItemView: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemHomePage: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItemFileNew: TMenuItem;
    MenuItemFileOpen: TMenuItem;
    MenuItemSettings: TMenuItem;
    MenuItemTools: TMenuItem;
    MenuItemFileOpenRecent: TMenuItem;
    MenuItemFileSave: TMenuItem;
    MenuItemFileSaveAs: TMenuItem;
    MenuItemFileClose: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemFile: TMenuItem;
    PopupMenuOpenRecent: TPopupMenu;
    ToolBarOther: TToolBar;
    ToolBarFile: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItemToolbarClick(Sender: TObject);
  private
    procedure UpdateFormTitle;
  public
    procedure UpdateInterface;
  end;

var
  FormMain: TFormMain;


implementation

{$R *.lfm}

uses
  UCore;

resourcestring
  SModified = 'Modified';

{ TFormMain }

procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Core.PersistentForm1.Save(Self);
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  Core.AFileClose.Execute;
  CanClose := Core.FileClosed;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to ToolBarFile.ButtonCount - 1 do
    ToolBarFile.Buttons[I].Hint := ToolBarFile.Buttons[I].Caption;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  Core.PersistentForm1.Load(Self);
  Core.Initialize;
end;

procedure TFormMain.MenuItemToolbarClick(Sender: TObject);
begin
  UpdateInterface;
end;

procedure TFormMain.UpdateFormTitle;
var
  Title: string;
begin
  Title := '';
  if Assigned(Core.DataFile) and
  (ExtractFileNameWithoutExt(ExtractFileName(Core.DataFile.FileName)) <> '') then
    Title := Title + ExtractFileNameWithoutExt(ExtractFileName(Core.DataFile.FileName));
  if Assigned(Core.DataFile) and Core.DataFile.Modified then
    Title := Title + ' (' + SModified + ')';
  if Title <> '' then Title := Title + ' - ';
  Title := Title + Core.ApplicationInfo1.AppName;
  Application.Title := Title;
  Caption := Title;
end;

procedure TFormMain.UpdateInterface;
begin
  UpdateFormTitle;
  CoolBar1.Visible := MenuItemToolbar.Checked;
end;

end.

