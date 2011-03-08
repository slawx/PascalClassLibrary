unit UCoolDockCustomize;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, Spin, UCoolDockLayout;

type

  { TCoolDockCustomizeForm }

  TCoolDockCustomizeForm = class(TForm)
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
    LayoutList: TCoolDockLayoutList;
  end;

implementation

resourcestring
  SNewLayout = 'New Layout';
  SEnterNewName = 'Enter new name';

{ TCoolDockCustomizeForm }

procedure TCoolDockCustomizeForm.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TCoolDockCustomizeForm.ButtonLayoutApplyClick(Sender: TObject);
begin
  if ListBox1.ItemIndex <> - 1 then
    TCoolDockLayout(LayoutList.Items[ListBox1.ItemIndex]).Restore;
end;

procedure TCoolDockCustomizeForm.ButtonLayoutDeleteClick(Sender: TObject);
begin
  if ListBox1.ItemIndex <> - 1 then begin
    LayoutList.Items.Delete(ListBox1.ItemIndex);
    LayoutList.PopulateStringList(ListBox1.Items);
  end;
end;

procedure TCoolDockCustomizeForm.ButtonLayoutNewClick(Sender: TObject);
var
  NewLayout: TCoolDockLayout;
  NewName: string;
begin
  NewName := SNewLayout;
  if InputQuery(SNewLayout, SEnterNewName, NewName) then
  if not Assigned(LayoutList.FindByName(NewName)) then begin
    NewLayout := TCoolDockLayout.Create;
    NewLayout.Name := NewName;
    NewLayout.Store;
    LayoutList.Items.Add(NewLayout);
    LayoutList.PopulateStringList(ListBox1.Items);
  end;
end;

procedure TCoolDockCustomizeForm.ButtonLayoutRenameClick(Sender: TObject);
var
  NewName: string;
begin
  NewName := TCoolDockLayout(LayoutList.Items[ListBox1.ItemIndex]).Name;
  if InputQuery(SNewLayout, SEnterNewName, NewName) then begin
    TCoolDockLayout(LayoutList.Items[ListBox1.ItemIndex]).Name := NewName;
    LayoutList.PopulateStringList(ListBox1.Items);
  end;
end;

procedure TCoolDockCustomizeForm.FormShow(Sender: TObject);
begin
  if Assigned(LayoutList) then begin
    LayoutList.PopulateStringList(ListBox1.Items);
  end;
end;

procedure TCoolDockCustomizeForm.ListBox1SelectionChange(Sender: TObject;
  User: boolean);
begin
  ButtonLayoutRename.Enabled := ListBox1.ItemIndex <> -1;
  ButtonLayoutDelete.Enabled := ListBox1.ItemIndex <> -1;
  ButtonLayoutApply.Enabled := ListBox1.ItemIndex <> -1;
  ButtonLayoutSave.Enabled := ListBox1.ItemIndex <> -1;
end;

initialization
  {$I UCoolDockCustomize.lrs}

end.

