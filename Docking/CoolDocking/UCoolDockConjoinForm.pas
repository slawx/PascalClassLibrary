unit UCoolDockConjoinForm;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, UCoolDockCommon, UCoolDockClientPanel;

type
  { TCoolDockConjoinForm }

  TCoolDockConjoinForm = class(TCoolDockConjoinFormBase)
  public
    CoolDockClient: TCoolDockClientBase;
    procedure FormShow(Sender : TObject);
    procedure FormHide(Sender : TObject);
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  private
    procedure SetName(const NewName: TComponentName); override;
  end;


implementation

uses
  UCoolDockManager, UCoolDocking;

{ TCoolDockConjoinForm }

procedure TCoolDockConjoinForm.FormShow(Sender: TObject);
begin
  TCoolDockManager(DockManager).Visible := True;
end;

procedure TCoolDockConjoinForm.FormHide(Sender: TObject);
var
  I: Integer;
begin
  TCoolDockManager(DockManager).Visible := False;
  // Hide all docked childs
  with TCoolDockManager(DockManager) do
  for I := 0 to DockPanels.Count - 1 do
    if Assigned(TCoolDockClientPanel(DockPanels[I]).Control) then begin
      TCoolDockClientPanel(DockPanels[I]).Control.Tag := Integer(dhtTemporal);
      TCoolDockClientPanel(DockPanels[I]).Control.Hide;
    end;
end;

constructor TCoolDockConjoinForm.Create(TheOwner: TComponent);
begin
  inherited CreateNew(TheOwner);
  CoolDockClient := TCoolDockClient.Create(Self);
  with CoolDockClient do begin
  end;
  OnShow := FormShow;
  OnHide := FormHide;
end;

destructor TCoolDockConjoinForm.Destroy;
begin
  inherited;
end;

procedure TCoolDockConjoinForm.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  CoolDockClient.Name := Name + 'CoolDockClient';
end;

initialization

RegisterClass(TCoolDockConjoinForm);


finalization

UnRegisterClass(TCoolDockConjoinForm);



end.

