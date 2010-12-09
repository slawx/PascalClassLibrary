unit UCoolDockStyle;

{$mode Delphi}{$H+}

interface

uses
  Classes, Controls, SysUtils, UCoolDockClientPanel;

type

  { TCoolDockStyle }

  TCoolDockStyle = class
    Manager: TObject; // TCoolDockManager;
    constructor Create(AManager: TObject);
    procedure InsertControl(NewPanel: TCoolDockClientPanel; AControl: TControl;
      InsertAt: TAlign); virtual;
    procedure UpdateClientSize; virtual;
  end;

implementation

uses
  UCoolDocking;

{ TCoolDockStyle }

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

end.

