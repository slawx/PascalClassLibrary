unit UCDManagerRegionsPopup;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, UCDManagerRegions, UCDCommon, Controls;

type

  { TCDStylePopupRegions }

  TCDStylePopupRegions = class(TCDManagerRegions)
    constructor Create(ADockSite: TWinControl);
  end;


implementation

uses
  UCDClient;

{ TCDStylePopupRegions }

constructor TCDStylePopupRegions.Create(ADockSite: TWinControl);
begin
  inherited;
  FDockStyle := dsPopupList;
end;

end.

