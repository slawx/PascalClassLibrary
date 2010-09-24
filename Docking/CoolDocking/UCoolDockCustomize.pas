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
    ComboBox2: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    ListBox1: TListBox;
    PageControl1: TPageControl;
    TabSheetSetting: TTabSheet;
    TabSheetLayouts: TTabSheet;
    procedure ButtonCloseClick(Sender: TObject);
  private
    { private declarations }
  public
  end;

implementation


{ TCoolDockCustomizeForm }

procedure TCoolDockCustomizeForm.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

initialization
  {$I UCoolDockCustomize.lrs}

end.

