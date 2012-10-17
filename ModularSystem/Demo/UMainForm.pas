unit UMainForm;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Menus, ActnList, UModularSystem;

type

  { TMainForm }

  TMainForm = class(TForm)
  published
    AModuleStart: TAction;
    AModuleStop: TAction;
    AModuleInstall: TAction;
    AModuleUninstall: TAction;
    AModuleUpdate: TAction;
    ActionList1: TActionList;
    ButtonUpdate: TButton;
    ButtonUninstall: TButton;
    ButtonInstall: TButton;
    ButtonUpdate1: TButton;
    ButtonUpdate2: TButton;
    ListViewModules: TListView;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    ModuleManager: TModuleManager;
    PopupMenu1: TPopupMenu;
    procedure AModuleStartExecute(Sender: TObject);
    procedure AModuleStopExecute(Sender: TObject);
    procedure ButtonInstallClick(Sender: TObject);
    procedure ButtonUninstallClick(Sender: TObject);
    procedure ButtonUpdateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListViewModulesData(Sender: TObject; Item: TListItem);
    procedure ListViewModulesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    procedure RegisterModules;
  public
    procedure Log(Text: string);
    procedure RefreshList;
  end;

const
  BoolText: array[Boolean] of string = ('No', 'Yes');

var
  MainForm: TMainForm;


implementation

{$R *.lfm}

uses
  UModuleUser, UModuleBase, UModuleACL, ULogForm;


{ TMainForm }

procedure TMainForm.ListViewModulesData(Sender: TObject; Item: TListItem);
begin
  if (Item.Index >= 0) and (Item.Index < ModuleManager.Modules.Count) then
  with TModule(ModuleManager.Modules[Item.Index]) do begin
    Item.Caption := Title;
    Item.Data := ModuleManager.Modules[Item.Index];
    Item.SubItems.Add(Identification);
    Item.SubItems.Add(Version);
    Item.SubItems.Add(BoolText[Installed]);
    Item.SubItems.Add(BoolText[Running]);
    Item.SubItems.Add(License);
    Item.SubItems.Add(StringReplace(Dependencies.Text, LineEnding, ', ', [rfReplaceAll]));
    Item.SubItems.Add(StringReplace(Description.Text, LineEnding, ', ', [rfReplaceAll]));
  end;
end;

procedure TMainForm.ListViewModulesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  Installed: Boolean;
  Running: Boolean;
begin
  if Assigned(ListViewModules.Selected) then Installed := TModule(ListViewModules.Selected.Data).Installed;
  if Assigned(ListViewModules.Selected) then Running := TModule(ListViewModules.Selected.Data).Running;
  AModuleInstall.Enabled := Assigned(ListViewModules.Selected) and not Installed;
  AModuleUninstall.Enabled := Assigned(ListViewModules.Selected) and Installed;
  AModuleUpdate.Enabled := Assigned(ListViewModules.Selected) and Installed;
  AModuleStart.Enabled := Assigned(ListViewModules.Selected) and not Running;
  AModuleStop.Enabled := Assigned(ListViewModules.Selected) and Running;
end;

procedure TMainForm.RegisterModules;
begin
  ModuleManager.RegisterModule(TModuleUser.Create(nil));
  ModuleManager.RegisterModule(TModuleBase.Create(nil));
  ModuleManager.RegisterModule(TModuleACL.Create(nil));
end;

procedure TMainForm.Log(Text: string);
begin
  LogForm.Memo1.Lines.Add(Text);
end;

procedure TMainForm.RefreshList;
begin
  ListViewModules.Items.Count := ModuleManager.Modules.Count;
  ListViewModules.Refresh;
  ListViewModulesSelectItem(ListViewModules, ListViewModules.Selected,
    Assigned(ListViewModules.Selected));
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  RegisterModules;
end;

procedure TMainForm.ButtonInstallClick(Sender: TObject);
var
  ModuleList: TStringList;
begin
  if Assigned(ListViewModules.Selected) then begin
    try
      ModuleList := TStringList.Create;
      TModule(ListViewModules.Selected.Data).EnumModulesInstall(ModuleList);
      if ModuleList.Count > 0 then begin
        if MessageDlg('These modules will be installed in addition to ' +
          TModule(ListViewModules.Selected.Data).Name + ': ' +
          StringReplace(ModuleList.Text, LineEnding, ', ', [rfReplaceAll]),
          mtConfirmation, [mbYes, mbNo], 0) = mrYes then
           TModule(ListViewModules.Selected.Data).Install;
      end else TModule(ListViewModules.Selected.Data).Install;
    finally
      ModuleList.Free;
    end;
    RefreshList;
  end;
end;

procedure TMainForm.AModuleStartExecute(Sender: TObject);
var
  ModuleList: TStringList;
begin
  if Assigned(ListViewModules.Selected) then begin
    try
      ModuleList := TStringList.Create;
      TModule(ListViewModules.Selected.Data).EnumModulesStart(ModuleList);
      if ModuleList.Count > 0 then begin
        if MessageDlg('These modules will be started in addition to ' +
          TModule(ListViewModules.Selected.Data).Name + ': ' +
          StringReplace(ModuleList.Text, LineEnding, ', ', [rfReplaceAll]),
          mtConfirmation, [mbYes, mbNo], 0) = mrYes then
           TModule(ListViewModules.Selected.Data).Start;
      end else TModule(ListViewModules.Selected.Data).Start;
    finally
      ModuleList.Free;
    end;
    RefreshList;
  end;
end;

procedure TMainForm.AModuleStopExecute(Sender: TObject);
var
  ModuleList: TStringList;
begin
  if Assigned(ListViewModules.Selected) then begin
    try
      ModuleList := TStringList.Create;
      TModule(ListViewModules.Selected.Data).EnumModulesStop(ModuleList);
      if ModuleList.Count > 0 then begin
        if MessageDlg('These modules will be stopped in addition to ' +
          TModule(ListViewModules.Selected.Data).Name + ': ' +
          StringReplace(ModuleList.Text, LineEnding, ', ', [rfReplaceAll]),
          mtConfirmation, [mbYes, mbNo], 0) = mrYes then
            TModule(ListViewModules.Selected.Data).Stop;
      end else TModule(ListViewModules.Selected.Data).Stop;
    finally
      ModuleList.Free;
    end;

    RefreshList;
  end;
end;

procedure TMainForm.ButtonUninstallClick(Sender: TObject);
var
  ModuleList: TStringList;
begin
  if Assigned(ListViewModules.Selected) then begin
    try
      ModuleList := TStringList.Create;
      TModule(ListViewModules.Selected.Data).EnumModulesUninstall(ModuleList);
      if ModuleList.Count > 0 then begin
        if MessageDlg('These modules will be uninstalled in addition to ' +
          TModule(ListViewModules.Selected.Data).Name + ': ' +
          StringReplace(ModuleList.Text, LineEnding, ', ', [rfReplaceAll]),
          mtConfirmation, [mbYes, mbNo], 0) = mrYes then
            TModule(ListViewModules.Selected.Data).Uninstall;
      end else TModule(ListViewModules.Selected.Data).Uninstall;
    finally
      ModuleList.Free;
    end;

    RefreshList;
  end;
end;

procedure TMainForm.ButtonUpdateClick(Sender: TObject);
begin
  if Assigned(ListViewModules.Selected) then begin
    TModule(ListViewModules.Selected.Data).Upgrade;
    RefreshList;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  RefreshList;
  LogForm.Show;
end;

end.

