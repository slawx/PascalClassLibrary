unit UCDManagerRegionsPopup;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, UCDManagerRegions, UCDCommon, Controls;

type

  { TCDManagerPopupRegionsItem }

  TCDManagerPopupRegionsItem = class(TCDManagerRegionsItem)
  private
    FAutoHide: Boolean;
    function GetAutoHideEnabled: Boolean;
    procedure SetAutoHide(const AValue: Boolean);
    procedure SetAutoHideEnabled(const AValue: Boolean);
  public
    property AutoHideEnabled: Boolean read GetAutoHideEnabled
      write SetAutoHideEnabled;
  end;

  { TCDManagerPopupRegions }

  TCDManagerPopupRegions = class(TCDManagerRegions)
    constructor Create(ADockSite: TWinControl);
  end;


implementation

uses
  UCDClient;

{ TCDManagerPopupRegionsItem }

function TCDManagerPopupRegionsItem.GetAutoHideEnabled: Boolean;
begin

end;

procedure TCDManagerPopupRegionsItem.SetAutoHide(const AValue: Boolean);
begin

end;

procedure TCDManagerPopupRegionsItem.SetAutoHideEnabled(const AValue: Boolean);
begin

end;

{ TCDManagerPopupRegions }

constructor TCDManagerPopupRegions.Create(ADockSite: TWinControl);
begin
  inherited;
  FDockStyle := dsPopupList;
end;

end.

