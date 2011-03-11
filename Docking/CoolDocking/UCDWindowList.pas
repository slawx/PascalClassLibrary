unit UCDWindowList; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, Menus, UCDLayout;

type

  { TCDWindowListForm }

  TCDWindowListForm = class(TForm)
    ButtonFocus: TButton;
    ButtonHide: TButton;
    ButtonShow: TButton;
    ImageList1: TImageList;
    ListView1: TListView;
    procedure ButtonFocusClick(Sender: TObject);
    procedure ButtonHideClick(Sender: TObject);
    procedure ButtonShowClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure ListView1KeyPress(Sender: TObject; var Key: char);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    { private declarations }
  public
    procedure LoadList;
    procedure LoadToMenuItem(MenuItem: TMenuItem);
  end; 

  TCDWindowList = class(TComponent)
  private
    FLayoutList: TCDLayoutList;
    Form: TCDWindowListForm;
    procedure SetLayoutList(const AValue: TCDLayoutList);
  public
    function Execute: Boolean;
    constructor Create(AOwner: TComponent); override;
  published
  end;

procedure Register;

implementation

resourcestring
  SStateFloating = 'Floating';
  SStateDocked = 'Docked';
  SStateVisible = 'Visible';
  SStateHidden = 'Hidden';

procedure Register;
begin
  RegisterComponents('CoolDocking', [TCDWindowList]);
end;

{ TCDWindowList }

function TCDWindowList.Execute: Boolean;
begin
  Form := TCDWindowListForm.Create(Self);
  Form.ShowModal;
  Form.Free;
  Result := True;
end;

constructor TCDWindowList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TCDWindowList.SetLayoutList(const AValue: TCDLayoutList);
begin
  if FLayoutList = AValue then Exit;
  FLayoutList := AValue;
end;

{ TCDWindowListForm }

procedure TCDWindowListForm.ButtonFocusClick(Sender: TObject);
begin
  if Assigned(ListView1.Selected) then
    TForm(ListView1.Selected.Data).Show;
  Close;
end;

procedure TCDWindowListForm.ButtonHideClick(Sender: TObject);
begin
  if Assigned(ListView1.Selected) then
    TForm(ListView1.Selected.Data).Close;
  LoadList;
end;

procedure TCDWindowListForm.ButtonShowClick(Sender: TObject);
begin
  if Assigned(ListView1.Selected) then
    TForm(ListView1.Selected.Data).Show;
  LoadList;
end;

procedure TCDWindowListForm.FormShow(Sender: TObject);
begin
  LoadList;
end;

procedure TCDWindowListForm.ListView1DblClick(Sender: TObject);
begin
  ButtonFocusClick(Self);
end;

procedure TCDWindowListForm.ListView1KeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then ButtonFocusClick(Self);
end;

procedure TCDWindowListForm.ListView1SelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  ButtonFocus.Enabled := Selected;
  ButtonHide.Enabled := Selected;
  ButtonShow.Enabled := Selected;
end;

procedure TCDWindowListForm.LoadList;
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

            //Mask.Assign(Form.Icon);
            //Mask.Canvas.Brush.Color := Form.Icon.TransparentColor;
            //Mask.Monochrome := True;
            //ImageList1.BkColor := clBlack;
            ImageList1.Add(IconBitmap, nil);
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

procedure TCDWindowListForm.LoadToMenuItem(MenuItem: TMenuItem);
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
  {$I UCDWindowList.lrs}

end.

