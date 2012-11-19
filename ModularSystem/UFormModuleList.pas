unit UFormModuleList;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, Menus, ActnList, StdCtrls, SpecializedList, DateUtils,
  UListViewSort, UModularSystem;

type
  TModuleListOption = (mloShowVersion, mloShowAuthor, mloShowFileName,
    mloShowIdentification, mloShowLicense, mloShowEnable, mloShowRunning,
    mloShowDependencies, mloShowInstalled, mloShowInfoBar, mloShowDescription,
    mloShowStartUpTime, mloAllowInstall, mloAllowEnable, mloAllowRegister, mloAllowStart);
  TModuleListOptions = set of TModuleListOption;

  { TFormModuleList }

  TFormModuleList = class(TForm)
    ARestart: TAction;
    ARegister: TAction;
    AEnable: TAction;
    ADisable: TAction;
    AStart: TAction;
    AStop: TAction;
    AInstall: TAction;
    AUninstall: TAction;
    AUnregister: TAction;
    ActionList1: TActionList;
    ImageList1: TImageList;
    ListViewModules: TListView;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    PopupMenu1: TPopupMenu;
    Splitter1: TSplitter;
    TimerRedraw: TTimer;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    procedure ADisableExecute(Sender: TObject);
    procedure AEnableExecute(Sender: TObject);
    procedure AInstallExecute(Sender: TObject);
    procedure AUninstallExecute(Sender: TObject);
    procedure AUnregisterExecute(Sender: TObject);
    procedure AStartExecute(Sender: TObject);
    procedure AStopExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListViewModulesData(Sender: TObject; Item: TListItem);
    procedure ListViewModulesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure TimerRedrawTimer(Sender: TObject);
  private
    FModuleManager: TModuleManager;
    FOptions: TModuleListOptions;
    ListViewSort: TListViewSort;
    function ListViewModulesCompare(Item1, Item2: TObject): Integer;
    procedure ModulesFilterExecute(ListViewSort: TListViewSort);
    procedure SetModuleManager(AValue: TModuleManager);
    procedure SetOptions(AValue: TModuleListOptions);
  public
    procedure Reload;
    procedure UpdateInterface;
    property Manager: TModuleManager read FModuleManager write SetModuleManager;
    property Options: TModuleListOptions read FOptions write SetOptions;
  end;

function ModuleToStr(Module: TObject): string;

implementation

resourcestring
  SYes = 'Yes';
  SNo = 'No';
  SAdditionalModulesInstall = 'In addition to "%0:s" module also dependent modules will be installed: "%1:s"';
  SAdditionalModulesUninstall = 'In addition to "%0:s" module alse dependent modules will be uninstalled: "%1:s"';
  SAdditionalModulesStart = 'In addition to "%0:s" module also dependent modules will be started: "%1:s"';
  SAdditionalModulesStop = 'In addition to "%0:s" module also dependent modules will be stopped: "%1:s"';
  SIdentification = 'Identification';
  SName = 'Name';
  SVersion = 'Version';
  SLicense = 'License';
  SDescription = 'Description';
  SDependencies = 'Dependencies';
  SAuthor = 'Author';

function ModuleToStr(Module: TObject): string;
begin
  Result := TModule(Module).Title;
end;

{ TFormModuleList }

procedure TFormModuleList.ListViewModulesData(Sender: TObject; Item: TListItem);
begin
  if (Item.Index >= 0) and (Item.Index < ListViewSort.List.Count) then
  with TModule(ListViewSort.List[Item.Index]) do begin
    Item.Caption := Identification;
    Item.Data := ListViewSort.List[Item.Index];
    Item.SubItems.Add(Title);
    if Enabled then Item.SubItems.Add(SYes)
      else Item.SubItems.Add(SNo);
    if Installed then Item.SubItems.Add(SYes)
      else Item.SubItems.Add(SNo);
    if Running then Item.SubItems.Add(SYes)
      else Item.SubItems.Add(SNo);
    if Author <> '' then Item.SubItems.Add(Author)
      else Item.SubItems.Add(' ');
    if License <> '' then Item.SubItems.Add(License)
      else Item.SubItems.Add(' ');
    if Version <> '' then Item.SubItems.Add(Version)
      else Item.SubItems.Add(' ');
    Item.SubItems.Add(Dependencies.Implode(',', StrToStr));
    if FileName <> '' then Item.SubItems.Add(FileName)
      else Item.SubItems.Add(' ');
    Item.SubItems.Add(FloatToStr(Trunc(StartUpTime / OneMillisecond)));
  end;
end;

procedure TFormModuleList.ListViewModulesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  UpdateInterface;
  Memo1.Clear;
  if Assigned(ListViewModules.Selected) then
  with TModule(ListViewModules.Selected.Data) do begin
    Memo1.Lines.Add(SName + ': ' + Title);
    if (mloShowIdentification in FOptions) then Memo1.Lines.Add(SIdentification + ': ' + Identification);
    if (mloShowAuthor in FOptions) and (Author <> '') then Memo1.Lines.Add(SAuthor + ': ' + Author);
    if (mloShowVersion in FOptions) and (Version <> '') then Memo1.Lines.Add(SVersion + ': ' + Version);
    if (mloShowLicense in FOptions) and (License <> '') then Memo1.Lines.Add(SLicense + ': ' + License);
    if (mloShowDependencies in FOptions) and (Dependencies.Count > 0) then
      Memo1.Lines.Add(SDependencies + ': ' + Dependencies.Implode(', ', StrToStr));
    if (mloShowDescription in FOptions) and (Description.Count > 0) then
      Memo1.Lines.Add(SDescription + ': ' + Description.Implode(', ', StrToStr));
  end;
end;

procedure TFormModuleList.FormCreate(Sender: TObject);
begin
  ListViewSort := TListViewSort.Create;
  ListViewSort.ListView := ListViewModules;
  ListViewSort.Column := 0;
  ListViewSort.Order := soDown;
  ListViewSort.OnCompareItem := ListViewModulesCompare;
  ListViewSort.OnFilter := ModulesFilterExecute;

  DoubleBuffered := True;
  ControlStyle := ControlStyle + [csOpaque];
  ListViewModules.DoubleBuffered := True;
  ListViewModules.ControlStyle := ListViewModules.ControlStyle + [csOpaque];
end;

procedure TFormModuleList.FormDestroy(Sender: TObject);
begin
  ListViewModules.Clear;
  FreeAndNil(ListViewSort);
end;

procedure TFormModuleList.AUnregisterExecute(Sender: TObject);
var
  I: Integer;
begin
  for I := ListViewModules.Items.Count - 1 downto 0 do
  if ListViewModules.Items[I].Selected then
  with TModule(ListViewModules.Items[I].Data) do begin
    FModuleManager.UnregisterModule(TModule(ListViewModules.Items[I].Data));
  end;
  UpdateInterface;
end;

procedure TFormModuleList.AStartExecute(Sender: TObject);
var
  Modules: TListModule;
  I: Integer;
begin
  for I := 0 to ListViewModules.Items.Count - 1 do
  if ListViewModules.Items[I].Selected then
  with TModule(ListViewModules.Items[I].Data) do
  if not Running then
  try
    Modules := TListModule.Create;
    Modules.OwnsObjects := False;
    EnumDependenciesCascade(Modules, [mcNotRunning]);
    if Modules.Count > 0 then begin
      if MessageDlg(Format(SAdditionalModulesStart, [
      Identification, Modules.Implode(',', ModuleToStr)]),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        Start;
    end else Start;
  finally
    Modules.Free;
  end;
  UpdateInterface;
end;

procedure TFormModuleList.AStopExecute(Sender: TObject);
var
  Modules: TListModule;
  I: Integer;
begin
  for I := 0 to ListViewModules.Items.Count - 1 do
  if ListViewModules.Items[I].Selected then
  with TModule(ListViewModules.Items[I].Data) do
  if Running then
  try
    Modules := TListModule.Create;
    Modules.OwnsObjects := False;
    EnumSuperiorDependenciesCascade(Modules, [mcRunning]);
    if Modules.Count > 0 then begin
      if MessageDlg(Format(SAdditionalModulesStop, [
      Identification,
      Modules.Implode(',', ModuleToStr)]),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        Stop;
    end else Stop;
  finally
    FreeAndNil(Modules);
  end;
  UpdateInterface;
end;

procedure TFormModuleList.AUninstallExecute(Sender: TObject);
var
  Modules: TListModule;
  I: Integer;
begin
  for I := 0 to ListViewModules.Items.Count - 1 do
  if ListViewModules.Items[I].Selected then
  with TModule(ListViewModules.Items[I].Data) do
  if Installed then
  try
    Modules := TListModule.Create;
    Modules.OwnsObjects := False;
    EnumSuperiorDependenciesCascade(Modules, [mcInstalled]);
    if Modules.Count > 0 then begin
      if MessageDlg(Format(SAdditionalModulesUninstall, [
      Identification,
      Modules.Implode(',', ModuleToStr)]),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        Uninstall;
    end else Uninstall;
  finally
    Modules.Free;
  end;
  UpdateInterface;
end;

procedure TFormModuleList.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  TimerRedraw.Enabled := False;
  //Core.PersistentForm.Save(Self);
end;

procedure TFormModuleList.AInstallExecute(Sender: TObject);
var
  Modules: TListModule;
  I: Integer;
begin
  for I := 0 to ListViewModules.Items.Count - 1 do
  if ListViewModules.Items[I].Selected then
  with TModule(ListViewModules.Items[I].Data) do
  if not Installed then
  try
    Modules := TListModule.Create;
    Modules.OwnsObjects := False;
    EnumDependenciesCascade(Modules, [mcNotInstalled]);
    if Modules.Count > 0 then begin
      if MessageDlg(Format(SAdditionalModulesInstall, [
      Identification,
      Modules.Implode(',', ModuleToStr)]),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        Install;
    end else Install;
  finally
    Modules.Free;
  end;
  UpdateInterface;
end;

procedure TFormModuleList.AEnableExecute(Sender: TObject);
var
  Modules: TListModule;
  I: Integer;
begin
  for I := 0 to ListViewModules.Items.Count - 1 do
  if ListViewModules.Items[I].Selected then
  with TModule(ListViewModules.Items[I].Data) do
  if not Enabled then
  try
    Modules := TListModule.Create;
    Modules.OwnsObjects := False;
    EnumDependenciesCascade(Modules, [mcNotRunning]);
    if Modules.Count > 0 then begin
      if MessageDlg(Format(SAdditionalModulesStart, [
      Identification, Modules.Implode(',', ModuleToStr)]),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
        Enable;
        Start;
      end;
    end else begin
      Enable;
      Start;
    end;
  finally
    Modules.Free;
  end;
  UpdateInterface;
end;

procedure TFormModuleList.ADisableExecute(Sender: TObject);
var
  Modules: TListModule;
  I: Integer;
begin
  for I := 0 to ListViewModules.Items.Count - 1 do
  if ListViewModules.Items[I].Selected then
  with TModule(ListViewModules.Items[I].Data) do
  if Enabled then
  try
    Modules := TListModule.Create;
    Modules.OwnsObjects := False;
    EnumSuperiorDependenciesCascade(Modules, [mcInstalled]);
    if Modules.Count > 0 then begin
      if MessageDlg(Format(SAdditionalModulesUninstall, [
      Identification,
      Modules.Implode(',', ModuleToStr)]),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
        Stop;
        Disable;
      end;
    end else begin
      TModule(ListViewModules.Selected.Data).Stop;
      TModule(ListViewModules.Selected.Data).Disable;
    end;
  finally
    Modules.Free;
  end;
  UpdateInterface;
end;

procedure TFormModuleList.FormHide(Sender: TObject);
begin
  TimerRedraw.Enabled := False;
end;

procedure TFormModuleList.FormShow(Sender: TObject);
begin
  Reload;
  UpdateInterface;
  //TimerRedraw.Enabled := True;
  //Core.PersistentForm.Load(Self);
end;

procedure TFormModuleList.TimerRedrawTimer(Sender: TObject);
begin
  Reload;
end;

function TFormModuleList.ListViewModulesCompare(Item1, Item2: TObject): Integer;
begin
  Result := 0;
  if Assigned(Item1) and Assigned(Item2) and (ListViewSort.Order <> soNone) then begin
    with ListViewSort do
    case Column of
      0: Result := CompareString(TModule(Item1).Identification,
        TModule(Item2).Identification);
      1: Result := CompareString(TModule(Item1).Title,
        TModule(Item2).Title);
      2: Result := CompareBoolean(TModule(Item1).Enabled,
        TModule(Item2).Enabled);
      3: Result := CompareBoolean(TModule(Item1).Installed,
        TModule(Item2).Installed);
      4: Result := CompareBoolean(TModule(Item1).Running,
        TModule(Item2).Running);
      5: Result := CompareString(TModule(Item1).Author,
        TModule(Item2).Author);
      6: Result := CompareString(TModule(Item1).License, TModule(
        Item2).License);
      7: Result := CompareString(TModule(Item1).Version, TModule(
        Item2).Version);
      8: Result := CompareString(TModule(Item1).Dependencies.Implode(',', StrToStr),
        TModule(Item2).Dependencies.Implode(',', StrToStr));
      9: Result := CompareString(TModule(Item1).FileName,
        TModule(Item2).FileName);
      10: Result := CompareTime(TModule(Item1).StartUpTime,
        TModule(Item2).StartUpTime);
    end;
    if ListViewSort.Order = soDown then Result := -Result;
  end else Result := 0;
end;

procedure TFormModuleList.ModulesFilterExecute(ListViewSort: TListViewSort);
var
  I: Integer;
begin
  if Assigned(FModuleManager) then
  begin
    ListViewSort.Source := nil;
    ListViewSort.List.Clear;
    //ListViewSort.List.Assign(FModuleManager.Modules);
    for I := 0 to FModuleManager.Modules.Count - 1 do
    with TModule(FModuleManager.Modules[I]) do begin
      ListViewSort.List.Add(FModuleManager.Modules[I]);
    end;
  end else begin
    ListViewSort.Source := nil;
    ListViewSort.List.Clear;
  end;
end;

procedure TFormModuleList.SetModuleManager(AValue: TModuleManager);
begin
  if FModuleManager = AValue then Exit;
  FModuleManager := AValue;
  if not (csDestroying in ComponentState) then Reload;
end;

procedure TFormModuleList.SetOptions(AValue: TModuleListOptions);
begin
  if FOptions = AValue then Exit;
  FOptions := AValue;
  UpdateInterface;
end;

procedure TFormModuleList.Reload;
begin
  if Assigned(ListViewSort) then
    ListViewSort.Refresh;
end;

procedure TFormModuleList.UpdateInterface;
begin
  AUnregister.Enabled := Assigned(ListViewModules.Selected) and
    (mloAllowRegister in FOptions);
  AUnregister.Visible := (mloAllowRegister in FOptions);
  ARegister.Enabled := Assigned(ListViewModules.Selected) and
    (mloAllowRegister in FOptions);
  ARegister.Visible := (mloAllowRegister in FOptions);
  AInstall.Enabled := Assigned(ListViewModules.Selected) and
    not TModule(ListViewModules.Selected.Data).Installed and
    (mloAllowInstall in FOptions) and
    TModule(ListViewModules.Selected.Data).Enabled;
  AInstall.Visible := (mloAllowInstall in FOptions);
  AUninstall.Enabled := Assigned(ListViewModules.Selected) and
    TModule(ListViewModules.Selected.Data).Installed and
    (mloAllowInstall in FOptions);
  AUninstall.Visible := (mloAllowInstall in FOptions);
  AStart.Enabled := Assigned(ListViewModules.Selected) and
    not TModule(ListViewModules.Selected.Data).Running and
    (mloAllowStart in FOptions);
  AStart.Visible := (mloAllowStart in FOptions);
  AStop.Enabled := Assigned(ListViewModules.Selected) and
    TModule(ListViewModules.Selected.Data).Running and
    TModule(ListViewModules.Selected.Data).Installed and
    (mloAllowStart in FOptions);
  AStop.Visible := (mloAllowStart in FOptions);
  AEnable.Enabled := Assigned(ListViewModules.Selected) and
    not TModule(ListViewModules.Selected.Data).Enabled and
    (mloAllowEnable in FOptions);
  AEnable.Visible := (mloAllowEnable in FOptions);
  ADisable.Enabled := Assigned(ListViewModules.Selected) and
    TModule(ListViewModules.Selected.Data).Enabled and
    (mloAllowEnable in FOptions);
  ADisable.Visible := (mloAllowEnable in FOptions);
  ListViewModules.Column[0].Visible := (mloShowIdentification in FOptions);
  ListViewModules.Column[1].Visible := True;
  ListViewModules.Column[2].Visible := (mloShowEnable in FOptions);
  ListViewModules.Column[3].Visible := (mloShowInstalled in FOptions);
  ListViewModules.Column[4].Visible := (mloShowRunning in FOptions);
  ListViewModules.Column[5].Visible := (mloShowAuthor in FOptions);
  ListViewModules.Column[6].Visible := (mloShowLicense in FOptions);
  ListViewModules.Column[7].Visible := (mloShowVersion in FOptions);
  ListViewModules.Column[8].Visible := (mloShowDependencies in FOptions);
  ListViewModules.Column[9].Visible := (mloShowFileName in FOptions);
  ListViewModules.Column[10].Visible := (mloShowStartUpTime in FOptions);
  Memo1.Visible := (mloShowInfoBar in FOptions);
  Splitter1.Visible := (mloShowInfoBar in FOptions);
end;

initialization
  {$I UFormModuleList.lrs}

end.

