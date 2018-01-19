unit UFormSettings;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, Spin, ExtCtrls, ULanguages;

type

  { TFormSettings }

  TFormSettings = class(TForm)
    Bevel1: TBevel;
    ButtonOk: TButton;
    ButtonCancel: TButton;
    CheckBoxReopenLastFileOnStart: TCheckBox;
    CheckBoxAutomaticDPI: TCheckBox;
    ComboBoxLanguage: TComboBox;
    Label1: TLabel;
    LabelDPI: TLabel;
    SpinEditDPI: TSpinEdit;
    procedure ButtonOkClick(Sender: TObject);
    procedure CheckBoxAutomaticDPIChange(Sender: TObject);
    procedure CheckBoxStartOnLogonChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    procedure LoadData;
    procedure SaveData;
    procedure UpdateInterface;
  end;

var
  FormSettings: TFormSettings;

implementation

{$R *.lfm}

uses
  UCore;

{ TFormSettings }

procedure TFormSettings.FormShow(Sender: TObject);
begin
  Core.CoolTranslator1.LanguageListToStrings(ComboBoxLanguage.Items);
  ComboBoxLanguage.ItemIndex := ComboBoxLanguage.Items.IndexOfObject(Core.CoolTranslator1.Language);
  if ComboBoxLanguage.ItemIndex = -1 then ComboBoxLanguage.ItemIndex := 0;
end;

procedure TFormSettings.ButtonOkClick(Sender: TObject);
begin
  if ComboBoxLanguage.ItemIndex <> -1 then
    Core.CoolTranslator1.Language := TLanguage(ComboBoxLanguage.Items.Objects[ComboBoxLanguage.ItemIndex]);
end;

procedure TFormSettings.CheckBoxAutomaticDPIChange(Sender: TObject);
begin
  UpdateInterface;
end;

procedure TFormSettings.CheckBoxStartOnLogonChange(Sender: TObject);
begin
  UpdateInterface;
end;

procedure TFormSettings.FormCreate(Sender: TObject);
begin
  Core.CoolTranslator1.TranslateComponentRecursive(Self);
end;

procedure TFormSettings.LoadData;
begin
  CheckBoxAutomaticDPI.Checked := Core.ScaleDPI1.AutoDetect;
  SpinEditDPI.Value := Core.ScaleDPI1.DPI.X;
  CheckBoxReopenLastFileOnStart.Checked := Core.ReopenLastFileOnStart;
  UpdateInterface;
end;

procedure TFormSettings.SaveData;
begin
  Core.ScaleDPI1.AutoDetect := CheckBoxAutomaticDPI.Checked;
  Core.ScaleDPI1.DPI := Point(SpinEditDPI.Value, SpinEditDPI.Value);
  Core.ReopenLastFileOnStart := CheckBoxReopenLastFileOnStart.Checked;
end;

procedure TFormSettings.UpdateInterface;
begin
  SpinEditDPI.Enabled := not CheckBoxAutomaticDPI.Checked;
  LabelDPI.Enabled := not CheckBoxAutomaticDPI.Checked;
end;

end.

