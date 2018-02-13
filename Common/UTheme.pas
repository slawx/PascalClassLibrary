unit UTheme;

interface

uses
  Classes, SysUtils, Graphics, ComCtrls, Controls, ExtCtrls, Menus, StdCtrls,
  Spin, Forms;

type
  TTheme = class(TComponent)
  private
    FThemeName: string;
    procedure SetThemeName(AValue: string);
  public
    ColorWindow: TColor;
    ColorWindowText: TColor;
    ColorControl: TColor;
    ColorControlText: TColor;
    ColorControlSelected: TColor;
    Used: Boolean;
    procedure ApplyTheme(Component: TComponent);
    constructor Create(AOwner: TComponent); override;
    procedure UseTheme(Form: TForm);
    property ThemeName: string read FThemeName write SetThemeName;
  end;


implementation

procedure TTheme.SetThemeName(AValue: string);
begin
  if FThemeName = AValue then Exit;
  FThemeName := AValue;
  if AValue = 'Dark' then begin
    ColorWindow := RGBToColor($20, $20, $20);
    ColorWindowText := clWhite;
    ColorControl := RGBToColor($40, $40, $40);
    ColorControlText := clWhite;
    ColorControlSelected := RGBToColor(96, 125, 155);
  end else
  if Avalue = 'Light' then begin
    ColorWindow := clWhite;
    ColorWindowText := clBlack;
    ColorControl := RGBToColor($e0, $e0, $e0);
    ColorControlText := clBlack;
    ColorControlSelected := RGBToColor(196, 225, 255);
  end else begin
    FThemeName := 'System';

    ColorWindow := clBlack;
    ColorWindowText := clWhite;
    ColorControl := clBlack;
    ColorControlText := clWhite;
    ColorControlSelected := clWhite;
  end;
end;

constructor TTheme.Create(AOwner: TComponent);
begin
  inherited;
  ThemeName := 'System';
end;

procedure TTheme.ApplyTheme(Component: TComponent);
var
  Control: TControl;
  I: Integer;
begin
  for I := 0 to Component.ComponentCount - 1 do
    ApplyTheme(Component.Components[I]);

  if Component is TControl then begin
    Control := (Component as TControl);
    if (Control is TEdit) or (Control is TSpinEdit) or (Control is TComboBox) and
    (Control is TMemo) or (Control is TListView) then begin
      Control.Color := ColorWindow;
      Control.Font.Color := ColorWindowText;
    end else begin
      Control.Color := ColorControl;
      Control.Font.Color := ColorControlText;
    end;
{      if ((Component as Control).ContextMenuStrip != null)
      begin
                    ApplyTheme((component as Control).ContextMenuStrip);
                end;
            end;

            if (component is TButton) and (Name = 'System') then
            begin
                (component as TButton).UseVisualStyleBackColor := true;
            end else
            if component is LinkLabel then
            begin
                (component as LinkLabel).LinkColor = ColorControlText;
                (component as LinkLabel).ActiveLinkColor = ColorControlText;
                (component as LinkLabel).VisitedLinkColor = ColorControlText;
            end
            else
            if component is TabControl then
            begin
                foreach (TabPage tabPage in (component as TabControl).TabPages)
                begin
                    ApplyTheme(tabPage);
                end;
            end else
            if component is TToolStrip then
            begin
                (component as ToolStrip).RenderMode = ToolStripRenderMode.Professional;
                (component as ToolStrip).Renderer = new ToolStripProfessionalRenderer(new ThemeColorTable());
                foreach (ToolStripItem item in (component as ToolStrip).Items)
                begin
                    if item is TToolStripMenuItem then
                    begin

                        if ((item as ToolStripMenuItem).HasDropDownItems)
                            foreach (ToolStripItem dropDownItem in (item as ToolStripMenuItem).DropDownItems)
                            begin
                                ApplyTheme(dropDownItem);
                            end;
                    end;
                    ApplyTheme(item);
                end;
            end else
            if component is TToolStripItem then
            begin
                (component as ToolStripItem).ForeColor = ColorControlText;
                (component as ToolStripItem).BackColor = ColorControl;
            end else
            if component is TDataGridView then
            begin
                (component as DataGridView).BackgroundColor = ColorWindow;
                (component as DataGridView).DefaultCellStyle.BackColor = ColorWindow;
                (component as DataGridView).DefaultCellStyle.ForeColor = ColorWindowText;
                (component as DataGridView).ColumnHeadersDefaultCellStyle.BackColor = ColorWindow;
                (component as DataGridView).ColumnHeadersDefaultCellStyle.ForeColor = ColorWindowText;
            end;
        end;
        }
  end;
end;

procedure TTheme.UseTheme(Form: TForm);
begin
  if not Used and (FThemeName = 'System') then Exit;
  ApplyTheme(Form);
  Used := True;
end;


end.
