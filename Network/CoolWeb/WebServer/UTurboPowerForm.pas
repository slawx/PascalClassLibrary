unit UTurboPowerForm;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, IpHtml, Ipfilebroker, LResources, Forms,
  Controls, Graphics, Dialogs, ComCtrls, StdCtrls, ActnList;

type

  { TFormWebBrowser }

  TFormWebBrowser = class(TForm)
    AReloadPage: TAction;
    AShowSource: TAction;
    ActionList1: TActionList;
    EditAddress: TEdit;
    IpFileDataProvider1: TIpFileDataProvider;
    IpHtmlPanel1: TIpHtmlPanel;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure AShowSourceExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
  public
    HtmlSource: string;
  end;

var
  FormWebBrowser: TFormWebBrowser;

implementation

{$R *.lfm}

{ TFormWebBrowser }

procedure TFormWebBrowser.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Application.Terminate;
end;

procedure TFormWebBrowser.AShowSourceExecute(Sender: TObject);
begin
  ShowMessage(HtmlSource);
end;

initialization

end.

