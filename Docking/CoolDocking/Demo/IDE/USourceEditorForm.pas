unit USourceEditorForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, UCoolDocking,
  SynHighlighterPas, SynEdit, SynHighlighterMulti;

type

  { TSourceEditorForm }

  TSourceEditorForm = class(TForm)
  published
    CoolDockClient1: TCoolDockClient;
    SynMultiSyn1: TSynMultiSyn;
    SynEdit1: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    { private declarations }
  public
    { public declarations }
  end; 

var
  SourceEditorForm: TSourceEditorForm;

implementation

{$R *.lfm}

end.

