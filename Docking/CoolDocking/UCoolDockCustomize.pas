unit UCoolDockCustomize;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls;

type

  { TCoolDockCustomizeForm }

  TCoolDockCustomizeForm = class(TForm)
    ButtonClose: TButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    PageControl1: TPageControl;
    TabSheetSetting: TTabSheet;
    TabSheetLayouts: TTabSheet;
  private
    { private declarations }
  public
  end;

  TCoolDockCustomize = class(TComponent)
    Form: TCoolDockCustomizeForm;
    function Execute: Boolean;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TCoolDockCustomize }

function TCoolDockCustomize.Execute: Boolean;
begin
  Form.ShowModal;
  Result := True;
end;

constructor TCoolDockCustomize.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Form := TCoolDockCustomizeForm.Create(Self);
end;

initialization
  {$I UCoolDockCustomize.lrs}

end.

