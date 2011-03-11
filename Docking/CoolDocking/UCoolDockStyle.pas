unit UCoolDockStyle;

{$mode Delphi}{$H+}

interface

uses
  Classes, Controls, SysUtils, UCoolDockClientPanel, UCoolDockCommon;

type
  { TCoolDockStyle }

  TCoolDockStyle = class
  private
  public
    Manager: TCoolDockManagerBase;
    function GetHeaderPos: THeaderPos; virtual;
    procedure SetHeaderPos(const AValue: THeaderPos); virtual;
    constructor Create(AManager: TCoolDockManagerBase);
    procedure InsertControl(AControl: TControl;
      InsertAt: TAlign); virtual;
    procedure RemoveControl(Control: TControl); virtual;
    procedure UpdateClientSize; virtual;
    procedure Switch(Index: Integer); virtual;
    procedure ChangeVisible(Control: TWinControl; Visible: Boolean); virtual;
    procedure SetVisible(const AValue: Boolean); virtual;
    property HeaderPos: THeaderPos read GetHeaderPos write SetHeaderPos;
    property Visible: Boolean write SetVisible;
  end;

implementation

uses
  UCoolDockClient;

{ TCoolDockStyle }

procedure TCoolDockStyle.SetVisible(const AValue: Boolean);
begin

end;

function TCoolDockStyle.GetHeaderPos: THeaderPos;
begin

end;

procedure TCoolDockStyle.SetHeaderPos(const AValue: THeaderPos);
begin

end;

constructor TCoolDockStyle.Create(AManager: TCoolDockManagerBase);
begin
  Manager := AManager;
end;

procedure TCoolDockStyle.InsertControl(AControl: TControl; InsertAt: TAlign);
begin

end;

procedure TCoolDockStyle.RemoveControl(Control: TControl);
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

