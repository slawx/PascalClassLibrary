unit UCDConjoinForm;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, UCDCommon;

type
  { TCDConjoinForm }

  TCDConjoinForm = class(TCDConjoinFormBase)
  public
    CoolDockClient: TCDClientBase;
    procedure FormShow(Sender : TObject);
    procedure FormHide(Sender : TObject);
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  private
    procedure SetName(const NewName: TComponentName); override;
  end;


implementation

uses
  UCDManager, UCDClient;

{ TCDConjoinForm }

procedure TCDConjoinForm.FormShow(Sender: TObject);
begin
  TCDManager(DockManager).Visible := True;
end;

procedure TCDConjoinForm.FormHide(Sender: TObject);
var
  I: Integer;
begin
  TCDManager(DockManager).Visible := False;
end;

constructor TCDConjoinForm.Create(TheOwner: TComponent);
begin
  inherited CreateNew(TheOwner);
  CoolDockClient := TCDClient.Create(Self);
  with CoolDockClient do begin
  end;
  OnShow := FormShow;
  OnHide := FormHide;
end;

destructor TCDConjoinForm.Destroy;
begin
  inherited;
end;

procedure TCDConjoinForm.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  CoolDockClient.Name := Name + 'CoolDockClient';
end;

initialization

RegisterClass(TCDConjoinForm);


finalization

UnRegisterClass(TCDConjoinForm);



end.

