unit UCDCustomize;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, Spin, UCDLayout, UCDCommon, UCDManager;

type

  { TCDCustomizeForm }

  TCDCustomizeForm = class(TForm)
  published
    ButtonLayoutDelete: TButton;
    ButtonLayoutApply: TButton;
    ButtonLayoutRename: TButton;
    ButtonLayoutNew: TButton;
    ButtonLayoutSave: TButton;
    ButtonClose: TButton;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ListBox1: TListBox;
    PageControl1: TPageControl;
    SpinEdit1: TSpinEdit;
    TabSheetSetting: TTabSheet;
    TabSheetLayouts: TTabSheet;
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonLayoutApplyClick(Sender: TObject);
    procedure ButtonLayoutDeleteClick(Sender: TObject);
    procedure ButtonLayoutNewClick(Sender: TObject);
    procedure ButtonLayoutRenameClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
  private
    { private declarations }
  public
    LayoutList: TCDLayoutList;
  end;

  { TCDCustomize }

  TCDCustomize = class(TCDCustomizeBase)
  private
    FLayoutList: TCDLayoutList;
    Form: TCDCustomizeForm;
    procedure SetLayoutList(const AValue: TCDLayoutList);
  public
    function Execute: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LayoutList: TCDLayoutList read FLayoutList write SetLayoutList;
  end;


implementation

uses
  UCDClient, UCDMaster;

resourcestring
  SNewLayout = 'New Layout';
  SEnterNewName = 'Enter new name';

{ TCDCustomizeForm }

procedure TCDCustomizeForm.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TCDCustomizeForm.ButtonLayoutApplyClick(Sender: TObject);
begin
  if ListBox1.ItemIndex <> - 1 then
    TCDLayout(LayoutList.Items[ListBox1.ItemIndex]).Restore;
end;

procedure TCDCustomizeForm.ButtonLayoutDeleteClick(Sender: TObject);
begin
  if ListBox1.ItemIndex <> - 1 then begin
    LayoutList.Items.Delete(ListBox1.ItemIndex);
    LayoutList.PopulateStringList(ListBox1.Items);
  end;
end;

procedure TCDCustomizeForm.ButtonLayoutNewClick(Sender: TObject);
var
  NewLayout: TCDLayout;
  NewName: string;
begin
  NewName := SNewLayout;
  if InputQuery(SNewLayout, SEnterNewName, NewName) then
  if not Assigned(LayoutList.FindByName(NewName)) then begin
    NewLayout := TCDLayout.Create;
    NewLayout.Name := NewName;
    NewLayout.Store;
    LayoutList.Items.Add(NewLayout);
    LayoutList.PopulateStringList(ListBox1.Items);
  end;
end;

procedure TCDCustomizeForm.ButtonLayoutRenameClick(Sender: TObject);
var
  NewName: string;
begin
  NewName := TCDLayout(LayoutList.Items[ListBox1.ItemIndex]).Name;
  if InputQuery(SNewLayout, SEnterNewName, NewName) then begin
    TCDLayout(LayoutList.Items[ListBox1.ItemIndex]).Name := NewName;
    LayoutList.PopulateStringList(ListBox1.Items);
  end;
end;

procedure TCDCustomizeForm.FormShow(Sender: TObject);
begin
  if Assigned(LayoutList) then begin
    LayoutList.PopulateStringList(ListBox1.Items);
  end;
end;

procedure TCDCustomizeForm.ListBox1SelectionChange(Sender: TObject;
  User: boolean);
begin
  ButtonLayoutRename.Enabled := ListBox1.ItemIndex <> -1;
  ButtonLayoutDelete.Enabled := ListBox1.ItemIndex <> -1;
  ButtonLayoutApply.Enabled := ListBox1.ItemIndex <> -1;
  ButtonLayoutSave.Enabled := ListBox1.ItemIndex <> -1;
end;


{ TCDCustomize }

procedure TCDCustomize.SetLayoutList(const AValue: TCDLayoutList);
begin
  if FLayoutList=AValue then exit;
  FLayoutList:=AValue;
end;

function TCDCustomize.Execute: Boolean;
begin
  Form := TCDCustomizeForm.Create(Self);
  if Assigned(Master) then begin
    Form.SpinEdit1.Value := TCDMaster(Master).DefaultMoveSpeed;
    Form.ComboBox1.ItemIndex := Integer(TCDMaster(Master).DefaultTabsPos);
    Form.ComboBox2.ItemIndex := Integer(TCDMaster(Master).DefaultHeaderPos);
    Form.LayoutList := FLayoutList;
  end;
  Form.ShowModal;
  if Assigned(Master) then begin
    TCDMaster(Master).DefaultMoveSpeed := Form.SpinEdit1.Value;
    TCDMaster(Master).DefaultTabsPos := THeaderPos(Form.ComboBox1.ItemIndex);
    TCDMaster(Master).DefaultHeaderPos := THeaderPos(Form.ComboBox2.ItemIndex);
  end;
  Form.Free;
  Result := True;
end;

constructor TCDCustomize.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCDCustomize.Destroy;
begin
  Master := nil;
  inherited Destroy;
end;


initialization
  {$I UCDCustomize.lrs}

end.

