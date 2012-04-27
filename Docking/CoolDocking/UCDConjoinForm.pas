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
    UpdateCaptionEnable: Boolean;
    procedure UpdateCaption;
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

procedure TCDConjoinForm.UpdateCaption;
var
  NewCaption: string;
  I: Integer;
begin
  if UpdateCaptionEnable then begin
    NewCaption := '';
    for I := 0 to DockClientCount - 1 do begin
      //if DockClients[I] is TCDConjoinForm then
      //  TCDConjoinForm(DockClients[I]).UpdateCaption;
      NewCaption := NewCaption + DockClients[I].Caption + ', ';
    end;
    Caption := Copy(NewCaption, 1, Length(NewCaption) - 2);

    if Assigned(HostDockSite) and (HostDockSite is TCDConjoinForm) then
      TCDConjoinForm(HostDockSite).UpdateCaption;
  end;
end;

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
  with TCDClient(CoolDockClient) do begin
    Dockable := True;
  end;
  OnShow := FormShow;
  OnHide := FormHide;
  UpdateCaptionEnable := True;
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

