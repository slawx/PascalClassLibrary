unit UCoolDockWindowList; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls;

type

  { TCoolDockWindowListForm }

  TCoolDockWindowListForm = class(TForm)
    ButtonFocus: TButton;
    ButtonCancel: TButton;
    ImageList1: TImageList;
    ListView1: TListView;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonFocusClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure ListView1KeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
  public
    procedure LoadList;
  end; 

var
  CoolDockWindowListForm: TCoolDockWindowListForm;

implementation

resourcestring
  SStateFloating = 'Floating';
  SStateDocked = 'Docked';
  SStateVisible = 'Visible';
  SStateHidden = 'Hidden';

{ TCoolDockWindowListForm }

procedure TCoolDockWindowListForm.ButtonCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TCoolDockWindowListForm.ButtonFocusClick(Sender: TObject);
begin
  if Assigned(ListView1.Selected) then
    TForm(ListView1.Selected.Data).Show;
  Close;
end;

procedure TCoolDockWindowListForm.FormShow(Sender: TObject);
begin
  LoadList;
end;

procedure TCoolDockWindowListForm.ListView1DblClick(Sender: TObject);
begin
  ButtonFocusClick(Self);
end;

procedure TCoolDockWindowListForm.ListView1KeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then ButtonFocusClick(Self);
end;

procedure TCoolDockWindowListForm.LoadList;
var
  I: Integer;
  NewItem: TListItem;
  Form: TForm;
  DockState: string;
begin
  with ListView1, Items do begin
    BeginUpdate;
    Clear;
    ImageList1.Clear;
    for I := 0 to Application.ComponentCount - 1 do begin
      if Application.Components[I] is TForm then begin
        Form := (Application.Components[I] as TForm);
        NewItem := Add;
        NewItem.Caption := Form.Caption;
        NewItem.Data := Form;
        if Assigned(Form.HostDockSite) then DockState := SStateDocked
          else DockState := SStateFloating;
        NewItem.SubItems.Add(DockState);
        if Form.Visible then DockState := SStateVisible
          else DockState := SStateHidden;
        NewItem.SubItems.Add(DockState);
        ImageList1.AddIcon(Form.Icon);
      end;
    end;
    EndUpdate;
  end;
end;

initialization
  {$I UCoolDockWindowList.lrs}

end.

