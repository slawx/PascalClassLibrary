unit UCore;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Controls, ActnList, Forms, Dialogs,
  ULastOpenedList, UApplicationInfo, UPersistentForm, UScaleDPI, UCommon,
  UCoolTranslator, UDataFile, Menus, URegistry, UTheme, Registry;

type

  { TCore }

  TCore = class(TDataModule)
    AAbout: TAction;
    ASettings: TAction;
    AFileOpenRecent: TAction;
    AHomePage: TAction;
    AFileClose: TAction;
    AFileSaveAs: TAction;
    AFileSave: TAction;
    AFileOpen: TAction;
    AFileNew: TAction;
    AExit: TAction;
    ActionList1: TActionList;
    ApplicationInfo1: TApplicationInfo;
    CoolTranslator1: TCoolTranslator;
    ImageList1: TImageList;
    LastOpenedList1: TLastOpenedList;
    OpenDialog1: TOpenDialog;
    PersistentForm1: TPersistentForm;
    SaveDialog1: TSaveDialog;
    ScaleDPI1: TScaleDPI;
    ThemeManager1: TThemeManager;
    procedure AAboutExecute(Sender: TObject);
    procedure AExitExecute(Sender: TObject);
    procedure AFileNewExecute(Sender: TObject);
    procedure AFileOpenExecute(Sender: TObject);
    procedure AFileOpenRecentExecute(Sender: TObject);
    procedure AFileSaveExecute(Sender: TObject);
    procedure AFileSaveAsExecute(Sender: TObject);
    procedure AFileCloseExecute(Sender: TObject);
    procedure AHomePageExecute(Sender: TObject);
    procedure ASettingsExecute(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure LastOpenedList1Change(Sender: TObject);
  private
    InitializeStarted: Boolean;
    InitializeFinished: Boolean;
    procedure FileModified(Sender: TObject);
    function FindFirstNonOption: string;
    procedure UpdateFile;
    procedure LoadConfig;
    procedure SaveConfig;
  public
    DataFile: TDataFile;
    DataFileClass: TDataFileClass;

    FileClosed: Boolean;
    ReopenLastFileOnStart: Boolean;
    ToolbarVisible: Boolean;
    procedure FileNew;
    procedure FileOpen(FileName: string);
    procedure FileClose;
    procedure Initialize;
    procedure Finalize;
    procedure UpdateInterface;
  end;

var
  Core: TCore;


implementation

{$R *.lfm}

uses
  UFormMain, UFormAbout, UFormSettings;

resourcestring
  SAppExit = 'Application exit';
  SAppExitQuery = 'File was modified. Do you want to save it before exit?';

{ TCore }

procedure TCore.AExitExecute(Sender: TObject);
begin
  FormMain.Close;
end;

procedure TCore.AAboutExecute(Sender: TObject);
begin
  FormAbout := TFormAbout.Create(nil);
  try
    FormAbout.ApplicationInfo := ApplicationInfo1;
    FormAbout.ShowModal;
  finally
    FormAbout.Free;
  end;
end;

procedure TCore.AFileCloseExecute(Sender: TObject);
begin
  FileClose;
  UpdateFile;
end;

procedure TCore.AHomePageExecute(Sender: TObject);
begin
  OpenWebPage(ApplicationInfo1.HomePage);
end;

procedure TCore.ASettingsExecute(Sender: TObject);
begin
  FormSettings := TFormSettings.Create(nil);
  try
    FormSettings.LoadData;
    if FormSettings.ShowModal = mrOK then
      FormSettings.SaveData;
  finally
    FormSettings.Free;
  end;
end;

procedure TCore.AFileNewExecute(Sender: TObject);
begin
  FileNew;
  UpdateFile;
end;

procedure TCore.AFileOpenExecute(Sender: TObject);
var
  TempFile: TDataFile;
begin
  OpenDialog1.DefaultExt := '';
  TempFile := DataFileClass.Create;
  try
    OpenDialog1.Filter := TempFile.GetFileDialogFilter;
    OpenDialog1.DefaultExt := TempFile.GetFileExt;
  finally
    TempFile.Free;
  end;
  if Assigned(DataFile) then begin
    OpenDialog1.InitialDir := ExtractFileDir(DataFile.FileName);
    OpenDialog1.FileName := ExtractFileName(DataFile.FileName);
  end;
  if OpenDialog1.Execute then begin
    FileOpen(OpenDialog1.FileName);
    UpdateFile;
  end;
end;

procedure TCore.AFileOpenRecentExecute(Sender: TObject);
begin
  FileOpen(TMenuItem(Sender).Caption);
  UpdateFile;
end;

procedure TCore.AFileSaveAsExecute(Sender: TObject);
begin
  SaveDialog1.DefaultExt := DataFile.GetFileExt;
  SaveDialog1.InitialDir := ExtractFileDir(DataFile.FileName);
  SaveDialog1.FileName := ExtractFileName(DataFile.FileName);
  SaveDialog1.Filter := DataFile.GetFileDialogFilter;
  if SaveDialog1.Execute then begin
    DataFile.SaveToFile(SaveDialog1.FileName);
    LastOpenedList1.AddItem(SaveDialog1.FileName);
    UpdateFile;
  end;
end;

procedure TCore.AFileSaveExecute(Sender: TObject);
begin
  if FileExists(DataFile.FileName) then begin
    DataFile.SaveToFile(DataFile.FileName);
    LastOpenedList1.AddItem(DataFile.FileName);
    UpdateFile;
  end else AFileSaveAs.Execute;
end;

procedure TCore.DataModuleCreate(Sender: TObject);
begin
  DataFile := nil;
  FileClosed := True;
  DataFileClass := TDataFile;
end;

procedure TCore.DataModuleDestroy(Sender: TObject);
begin
end;

procedure TCore.LastOpenedList1Change(Sender: TObject);
begin
  LastOpenedList1.LoadToMenuItem(FormMain.MenuItemFileOpenRecent, AFileOpenRecentExecute);
  LastOpenedList1.LoadToMenuItem(FormMain.PopupMenuOpenRecent.Items, AFileOpenRecentExecute);
end;

procedure TCore.FileModified(Sender: TObject);
begin
  UpdateFile;
end;

procedure TCore.FileOpen(FileName: string);
begin
  FileClose;
  if FileClosed then begin
    FileNew;
    DataFile.LoadFromFile(FileName);
    LastOpenedList1.AddItem(FileName);
  end;
end;

procedure TCore.FileClose;
var
  ModalResult: TModalResult;
  DoClose: Boolean;
begin
  DoClose := False;
  if Assigned(DataFile) then begin
    if DataFile.Modified then begin
       ModalResult := MessageDlg(SAppExit, SAppExitQuery,
       mtConfirmation, [mbYes, mbNo, mbCancel], 0);
      if ModalResult = mrYes then begin
        AFileSave.Execute;
        DoClose := True;
      end
      else if ModalResult = mrNo then begin
        DoClose := True;
      end else FileClosed := False;
    end else DoClose := True;
  end else DoClose := True;
  if DoClose then begin
    FreeAndNil(DataFile);
    FileClosed := True;
  end;
end;

procedure TCore.FileNew;
begin
  FileClose;
  if FileClosed then begin
    DataFile := TDataFile.Create;
    DataFile.OnModify := FileModified;
  end;
end;

procedure TCore.UpdateFile;
begin
  UpdateInterface;
  FormMain.UpdateInterface;
end;

procedure TCore.LoadConfig;
begin
  PersistentForm1.RegistryContext := ApplicationInfo1.GetRegistryContext;
  LastOpenedList1.LoadFromRegistry(TRegistryContext.Create(ApplicationInfo1.RegistryRoot,
    ApplicationInfo1.RegistryKey + '\RecentFiles'));

  with TRegistryEx.Create do
  try
    CurrentContext := ApplicationInfo1.GetRegistryContext;
    if ValueExists('LanguageCode') then
      CoolTranslator1.Language := CoolTranslator1.Languages.SearchByCode(ReadStringWithDefault('LanguageCode', ''))
      else CoolTranslator1.Language := CoolTranslator1.Languages.SearchByCode('');
    FormMain.MenuItemToolbar.Checked := ReadBoolWithDefault('ToolBarVisible', True);
    ReopenLastFileOnStart := ReadBoolWithDefault('ReopenLastFileOnStart', True);
    ThemeManager1.Theme := ThemeManager1.Themes.FindByName(ReadStringWithDefault('Theme', 'System'));
  finally
    Free;
  end;
end;

procedure TCore.SaveConfig;
begin
  LastOpenedList1.SaveToRegistry(TRegistryContext.Create(ApplicationInfo1.RegistryRoot,
    ApplicationInfo1.RegistryKey + '\RecentFiles'));

  with TRegistryEx.Create do
  try
    CurrentContext := ApplicationInfo1.GetRegistryContext;
    if Assigned(CoolTranslator1.Language) and (CoolTranslator1.Language.Code <> '') then
      WriteString('LanguageCode', CoolTranslator1.Language.Code)
      else DeleteValue('LanguageCode');
    WriteBool('ToolBarVisible', FormMain.MenuItemToolbar.Checked);
    WriteBool('ReopenLastFileOnStart', ReopenLastFileOnStart);
    WriteString('Theme', ThemeManager1.Theme.Name);
  finally
    Free;
  end;
end;

procedure TCore.UpdateInterface;
begin
  AFileSave.Enabled := Assigned(DataFile);
  AFileSaveAs.Enabled := Assigned(DataFile);
  AFileClose.Enabled := Assigned(DataFile);
end;

procedure TCore.Initialize;
var
  FileNameOption: string;
begin
  if not InitializeStarted then begin
    InitializeStarted := True;
    LoadConfig;

    FileNameOption := FindFirstNonOption;
    if FileNameOption <> '' then begin
      // Open file specified as command line parameter
      AFileNew.Execute;
      DataFile.LoadFromFile(FileNameOption);
      LastOpenedList1.AddItem(FileNameOption);
    end else
    if (LastOpenedList1.Items.Count > 0) and FileExists(LastOpenedList1.Items[0]) then begin
      // Open last opened file
      AFileNew.Execute;
      DataFile.LoadFromFile(LastOpenedList1.Items[0])
    end;

    UpdateFile;
    InitializeFinished := True;
  end;
end;

procedure TCore.Finalize;
begin
  SaveConfig;
end;

function TCore.FindFirstNonOption: string;
var
  S: string;
  I: Integer;
begin
  Result := '';
  for I := 1 to Application.ParamCount do begin
    S := Application.Params[I];
    if S[1] = Application.OptionChar then Continue;
    Result := S;
    Break;
  end;
end;

end.

