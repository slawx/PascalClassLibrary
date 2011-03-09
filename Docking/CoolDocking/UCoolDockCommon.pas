unit UCoolDockCommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;

type
  TDockStyle = (dsList, dsTabs, dsPopupTabs, dsPopupList);

  TCoolDockMasterBase = class;


  TCoolDockConjoinFormBase = class(TForm)
    constructor Create(TheOwner: TComponent); override;
  end;

  TCoolDockCustomizeBase = class(TComponent)
  private
    FMaster: TCoolDockMasterBase;
    procedure SetMaster(const AValue: TCoolDockMasterBase);
  published
    property Master: TCoolDockMasterBase read FMaster write SetMaster;
  end;

  TCoolDockMasterBase = class(TComponent)
  private
    FCoolDockCustomize: TCoolDockCustomizeBase;
    procedure SetCustomize(const AValue: TCoolDockCustomizeBase);
  published
    property Customize: TCoolDockCustomizeBase read FCoolDockCustomize
      write SetCustomize;
  end;

implementation

{ TCoolDockConjoinFormBase }

constructor TCoolDockConjoinFormBase.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

procedure TCoolDockCustomizeBase.SetMaster(const AValue: TCoolDockMasterBase);
var
  OldMaster: TCoolDockMasterBase;
begin
  if FMaster = AValue then Exit;
  OldMaster := FMaster;
  FMaster := AValue;
  if Assigned(AValue) then begin
    FMaster.Customize := Self;
  end else begin
    OldMaster.Customize := nil;
  end;
end;

procedure TCoolDockMasterBase.SetCustomize(const AValue: TCoolDockCustomizeBase
  );
var
  OldCustomize: TCoolDockCustomizeBase;
begin
  if FCoolDockCustomize = AValue then Exit;
  OldCustomize := FCoolDockCustomize;
  FCoolDockCustomize := AValue;
  if Assigned(AValue) then begin
    FCoolDockCustomize.Master := Self;
  end else begin
    OldCustomize.Master := nil;
  end;
end;


end.

