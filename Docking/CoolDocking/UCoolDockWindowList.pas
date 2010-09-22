unit UCoolDockWindowList; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, Menus;

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
    procedure LoadToMenuItem(MenuItem: TMenuItem);
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
  IconBitmap: TBitmap;
  Mask: TBitmap;
begin
  with ListView1, Items do begin
    BeginUpdate;
    Clear;
    ImageList1.Clear;
    for I := 0 to Application.ComponentCount - 1 do begin
      if (Application.Components[I] is TForm) then begin
        Form := (Application.Components[I] as TForm);
        if Form.DragKind = dkDock then begin
          NewItem := Add;
          NewItem.Caption := Form.Caption;
          NewItem.Data := Form;
          if Assigned(Form.HostDockSite) then DockState := SStateDocked
            else DockState := SStateFloating;
          NewItem.SubItems.Add(DockState);
          if Form.Visible then DockState := SStateVisible
            else DockState := SStateHidden;
          NewItem.SubItems.Add(DockState);

          try
            Mask := TBitmap.Create;
            IconBitmap := TBitmap.Create;
            //IconBitmap.SetSize(Form.Icon.Width, Form.Icon.Height);
            //ShowMessage(IntToStr(Integer(Form.Icon.TransparentColor)));
            IconBitmap.Assign(Form.Icon);
          //IconBitmap.Canvas.Draw(0, 0, Form.Icon);

            Mask.Assign(Form.Icon);
            Mask.Canvas.Brush.Color := Form.Icon.TransparentColor;
            Mask.Monochrome := True;
            ImageList1.BkColor := clBlack;
            ImageList1.Add(IconBitmap, Mask);
          finally
            Mask.Free;
            IconBitmap.Free;
          end;

          NewItem.ImageIndex := ImageList1.Count - 1;
        end;
      end;
    end;
    EndUpdate;
  end;
end;

procedure TCoolDockWindowListForm.LoadToMenuItem(MenuItem: TMenuItem);
var
  NewMenuItem: TMenuItem;
  I: Integer;
  Form: TForm;
begin
  with MenuItem do begin
    Clear;
    for I := 0 to Application.ComponentCount - 1 do begin
      if Application.Components[I] is TForm then begin
        Form := (Application.Components[I] as TForm);
        NewMenuItem := TMenuItem.Create(MenuItem);
        NewMenuItem.Caption := Form.Caption;
        MenuItem.Add(NewMenuItem);
      end;
    end;
  end;
end;

initialization
  {$I UCoolDockWindowList.lrs}

end.

