unit UFormNewVersionOffer;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type

  { TFormNewVersionOffer }

  TFormNewVersionOffer = class(TForm)
    BitBtnNo: TBitBtn;
    BitBtnWhatsNew: TBitBtn;
    BitBtnYes: TBitBtn;
    Image1: TImage;
    LabelChanges: TLabel;
    LabelQuestion: TLabel;
    MemoReleaseNotes: TMemo;
    procedure BitBtnWhatsNewClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
  end;

var
  FormNewVersionOffer: TFormNewVersionOffer;

implementation

uses
  UUpdateChecker;

{ TFormNewVersionOffer }

procedure TFormNewVersionOffer.FormCreate(Sender: TObject);
begin
end;

procedure TFormNewVersionOffer.BitBtnWhatsNewClick(Sender: TObject);
begin
  MemoReleaseNotes.Visible := True;
  LabelChanges.Visible := True;
  Height := 300;
end;

procedure TFormNewVersionOffer.FormShow(Sender: TObject);
begin
  Caption := SCheckUpdates;
  LabelChanges.Caption := SChangesInNewVersion;
  BitBtnYes.Caption := SYes;
  BitBtnNo.Caption := SNo;
  BitBtnWhatsNew.Caption := SWhatsNew;
end;


initialization
  {$I UFormNewVersionOffer.lrs}

end.

