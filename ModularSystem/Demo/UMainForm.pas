unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Menus, ActnList, UModularSystem;

type

  { TMainForm }

  TMainForm = class(TForm)
  published
    AModuleInstall: TAction;
    AModuleUninstall: TAction;
    AModuleUpdate: TAction;
    ActionList1: TActionList;
    ButtonUpdate: TButton;
    ButtonUninstall: TButton;
    ButtonInstall: TButton;
    ListViewModules: TListView;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    PopupMenu1: TPopupMenu;
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
    ModuleManager: TModuleManager;
    procedure RefreshList;
  end;

const
  InstalledText: array[Boolean] of string = ('Not installed', 'Installed');

var
  MainForm: TMainForm;


implementation

{$R *.lfm}

uses
  UModuleUser, UModuleBase, UModuleACL;


{ TMainForm }

procedure TMainForm.ListViewModulesData(Sender: TObject; Item: TListItem);
begin
  if (Item.Index >= 0) and (Item.Index < ModuleManager.Modules.Count) then
  with TModule(ModuleManager.Modules[Item.Index]) do begin
    Item.Caption := Title;
    Item.Data := ModuleManager.Modules[Item.Index];
    Item.SubItems.Add(Name);
    Item.SubItems.Add(Version);
    Item.SubItems.Add(InstalledText[Installed]);
    Item.SubItems.Add(License);
    Item.SubItems.Add(StringReplace(Dependencies.Text, LineEnding, ', ', [rfReplaceAll]));
    Item.SubItems.Add(StringReplace(Description.Text, LineEnding, ', ', [rfReplaceAll]));
  end;
end;

procedure TMainForm.ListViewModulesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  Installed: Boolean;
begin
  if Assigned(ListViewModules.Selected) then Installed := TModule(ListViewModules.Selected.Data).Installed;
  AModuleInstall.Enabled := Assigned(ListViewModules.Selected) and not Installed;
  AModuleUninstall.Enabled := Assigned(ListViewModules.Selected) and Installed;
  AModuleUpdate.Enabled := Assigned(ListViewModules.Selected) and Installed;
end;

procedure TMainForm.RegisterModules;
begin
  ModuleManager.RegisterModule(TModuleUser.Create);
  ModuleManager.RegisterModule(TModuleBase.Create);
  ModuleManager.RegisterModule(TModuleACL.Create);
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
  ModuleManager := TModuleManager.Create(nil);
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
    TModule(ListViewModules.Selected.Data).Update;
    RefreshList;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ModuleManager.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  RefreshList;
end;

end.
