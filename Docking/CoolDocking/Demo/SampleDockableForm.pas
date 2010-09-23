unit SampleDockableForm;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, UCoolDocking;

type

  { TForm1 }

  TForm1 = class(TForm)
    CoolDockClient1: TCoolDockClient;
    ImageList1: TImageList;
    Memo1: TMemo;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

initialization
  {$I SampleDockableForm.lrs}

end.

