unit UCDStyle;

{$mode Delphi}{$H+}

interface

uses
  Classes, Controls, SysUtils, UCDClientPanel, UCDCommon;

type
  { TCDStyle }

  TCDStyle = class
  private
  public
    Manager: TCDManagerBase;
    function GetHeaderPos: THeaderPos; virtual;
    procedure SetHeaderPos(const AValue: THeaderPos); virtual;
    constructor Create(AManager: TCDManagerBase);
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
  UCDClient;

{ TCDStyle }

procedure TCDStyle.SetVisible(const AValue: Boolean);
begin

end;

function TCDStyle.GetHeaderPos: THeaderPos;
begin

end;

procedure TCDStyle.SetHeaderPos(const AValue: THeaderPos);
begin

end;

constructor TCDStyle.Create(AManager: TCDManagerBase);
begin
  Manager := AManager;
end;

procedure TCDStyle.InsertControl(AControl: TControl; InsertAt: TAlign);
begin

end;

procedure TCDStyle.RemoveControl(Control: TControl);
begin

end;

procedure TCDStyle.UpdateClientSize;
begin

end;

procedure TCDStyle.Switch(Index: Integer);
begin

end;

procedure TCDStyle.ChangeVisible(Control: TWinControl; Visible: Boolean);
begin

end;

end.

