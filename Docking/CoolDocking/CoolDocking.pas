{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CoolDocking; 

interface

uses
  UCoolDocking, UCoolDockCustomize, UCoolDockWindowList, UCoolDockStyleTabs, 
  UCoolDockStyleRegions, UCoolDockStylePopupTabs, UCoolDockStylePopupRegions, 
  UCoolDockStyle, UCoolDockClientPanel, UCoolDockPopupMenu, UCoolDockLayout, 
  URectangle, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('UCoolDocking', @UCoolDocking.Register); 
  RegisterUnit('UCoolDockLayout', @UCoolDockLayout.Register); 
end; 

initialization
  RegisterPackage('CoolDocking', @Register); 
end.
