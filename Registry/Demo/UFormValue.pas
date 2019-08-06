unit UFormValue;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Spin, UGeneralRegistry;

type

  { TFormValue }

  TFormValue = class(TForm)
    ButtonOk: TButton;
    ButtonCancel: TButton;
    CheckBoxValue: TCheckBox;
    ComboBoxType: TComboBox;
    EditName: TEdit;
    EditValue: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    PageControl1: TPageControl;
    SpinEditValue: TSpinEdit;
    TabSheetBoolean: TTabSheet;
    TabSheetString: TTabSheet;
    TabSheetInteger: TTabSheet;
    procedure ComboBoxTypeSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    function GetValueType: TRegValueType;
    procedure SetValueType(ValueType: TRegValueType);
  end;

var
  FormValue: TFormValue;


implementation

{$R *.lfm}


{ TFormValue }

procedure TFormValue.FormShow(Sender: TObject);
begin
end;

function TFormValue.GetValueType: TRegValueType;
begin
  Result := TRegValueType(ComboBoxType.ItemIndex + 1);
end;

procedure TFormValue.SetValueType(ValueType: TRegValueType);
begin
  ComboBoxType.ItemIndex := Integer(ValueType) - 1;
  ComboBoxTypeSelect(nil);
end;

procedure TFormValue.ComboBoxTypeSelect(Sender: TObject);
begin
  if GetValueType = vtString then TabSheetString.Show
  else if GetValueType = vtInteger then TabSheetInteger.Show
  else if GetValueType = vtBoolean then TabSheetBoolean.Show;
end;

procedure TFormValue.FormCreate(Sender: TObject);
var
  I: TRegValueType;
begin
  ComboBoxType.Items.BeginUpdate;
  ComboBoxType.Items.Clear;
  for I := Succ(Low(TRegValueType)) to High(TRegValueType) do
    ComboBoxType.Items.Add(RegValueTypeName[I]);
  ComboBoxType.Items.EndUpdate;
  if (ComboBoxType.ItemIndex = -1) and (ComboBoxType.Items.Count > 0) then
    ComboBoxType.ItemIndex := 0;
end;

end.

