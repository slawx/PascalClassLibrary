{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CoolDocking; 

interface

uses
  UCoolDockClient, UCoolDockCustomize, UCoolDockWindowList, 
  UCoolDockStyleTabs, UCoolDockStyleRegions, UCoolDockStylePopupTabs, 
  UCoolDockStylePopupRegions, UCoolDockStyle, UCoolDockClientPanel, 
  UCoolDockPopupMenu, UCoolDockLayout, URectangle, UCoolDockCommon, 
  UCoolDockManager, UCoolDockConjoinForm, UCoolDockMaster, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('UCoolDockClient', @UCoolDockClient.Register); 
  RegisterUnit('UCoolDockWindowList', @UCoolDockWindowList.Register); 
  RegisterUnit('UCoolDockLayout', @UCoolDockLayout.Register); 
end; 

initialization
  RegisterPackage('CoolDocking', @Register); 
end.
