unit SampleDockableForm;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, UCoolDocking;

type

  { TDockableForm }

  TDockableForm = class(TForm)
    CoolDockClient1: TCoolDockClient;
    ImageList1: TImageList;
    Memo1: TMemo;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  DockableForm: TDockableForm;

implementation

initialization
  {$I SampleDockableForm.lrs}

end.

