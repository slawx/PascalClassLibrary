program testbgracontrols;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Forms,
  Interfaces,
  umain,
  uflashplayersetup,
  uwin7toolbar,
  uscaledpi,
  uglassmenu,
  uribbon,
  umetroui,
  umathgame,
  unice,
  bgrabutton,
  bgrawin7toolbar,
  bgrasamples,
  uioselements,
  ubgraimagebuttontest,
  uanim,
  uallofthem;

{$R *.res}

begin
  Application.Title := 'Test BGRA Controls';
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmFlashPlayerSetup, frmFlashPlayerSetup);
  Application.CreateForm(TfrmWin7ToolBar, frmWin7ToolBar);
  Application.CreateForm(TfrmRibbon, frmRibbon);
  Application.CreateForm(TfrmMetroUI, frmMetroUI);
  Application.CreateForm(TfrmGlassMenu, frmGlassMenu);
  Application.CreateForm(TfrmJuego, frmJuego);
  Application.CreateForm(TfrmNice, frmNice);
  Application.CreateForm(TfrmiOSElements, frmiOSElements);
  Application.CreateForm(TfrmAnimation, frmAnimation);
  Application.CreateForm(TfrmAllOfThem, frmAllOfThem);
  {$IFDEF WINDOWS}
  HighDPI(96);
  SetAllFormsDoubleBuffered;
  {$ENDIF}
  Application.CreateForm(Tfrmibtest, frmibtest); // it's already a big form
  Application.Run;
end.
