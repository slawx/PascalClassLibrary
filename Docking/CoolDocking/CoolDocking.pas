{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CoolDocking; 

interface

uses
  UCDClient, UCDCustomize, UCDWindowList, UCDManagerTabs, UCDManagerRegions, 
  UCDManagerTabsPopup, UCDManagerRegionsPopup, UCDClientPanel, UCDPopupMenu, 
  UCDLayout, URectangle, UCDCommon, UCDManager, UCDConjoinForm, UCDMaster, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('UCDClient', @UCDClient.Register); 
  RegisterUnit('UCDWindowList', @UCDWindowList.Register); 
  RegisterUnit('UCDLayout', @UCDLayout.Register); 
  RegisterUnit('UCDMaster', @UCDMaster.Register); 
end; 

initialization
  RegisterPackage('CoolDocking', @Register); 
end.
