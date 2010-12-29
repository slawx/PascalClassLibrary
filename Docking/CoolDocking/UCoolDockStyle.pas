unit UCoolDockStyle;

{$mode Delphi}{$H+}

interface

uses
  Classes, Controls, SysUtils, UCoolDockClientPanel;

type
  { TCoolDockStyle }

  TCoolDockStyle = class
  private
  public
    Manager: TObject; // TCoolDockManager;
    function GetHeaderPos: THeaderPos; virtual;
    procedure SetHeaderPos(const AValue: THeaderPos); virtual;
    constructor Create(AManager: TObject);
    procedure InsertControl(NewPanel: TCoolDockClientPanel; AControl: TControl;
      InsertAt: TAlign); virtual;
    procedure UpdateClientSize; virtual;
    procedure Switch(Index: Integer); virtual;
    procedure ChangeVisible(Control: TWinControl; Visible: Boolean); virtual;
    property HeaderPos: THeaderPos read GetHeaderPos write SetHeaderPos;
  end;

implementation

uses
  UCoolDocking;

{ TCoolDockStyle }

function TCoolDockStyle.GetHeaderPos: THeaderPos;
begin

end;

procedure TCoolDockStyle.SetHeaderPos(const AValue: THeaderPos);
begin

end;

constructor TCoolDockStyle.Create(AManager: TObject);
begin
  Manager := AManager;
end;

procedure TCoolDockStyle.InsertControl(NewPanel: TCoolDockClientPanel;
  AControl: TControl; InsertAt: TAlign);
begin

end;

procedure TCoolDockStyle.UpdateClientSize;
begin

end;

procedure TCoolDockStyle.Switch(Index: Integer);
begin

end;

procedure TCoolDockStyle.ChangeVisible(Control: TWinControl; Visible: Boolean);
begin

end;

end.

