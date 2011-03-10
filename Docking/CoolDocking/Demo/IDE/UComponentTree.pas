unit UComponentTree;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls;

type

  { TComponentTree }

  TComponentTree = class(TForm)
    TreeView1: TTreeView;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    procedure ReloadNode(Node: TTreeNode; Component: TComponent);
    procedure Reload;
  end; 

var
  ComponentTree: TComponentTree;

implementation

uses
  UMainForm;

{ TComponentTree }

procedure TComponentTree.FormShow(Sender: TObject);
begin
  Reload;
end;

procedure TComponentTree.ReloadNode(Node: TTreeNode; Component: TComponent);
var
  I: Integer;
  NewNode: TTreeNode;
begin
  NewNode := Node.TreeNodes.AddChild(Node, Component.Name + ': ' + Component.ClassName);
  for I := 0 to Component.ComponentCount - 1 do
    ReloadNode(NewNode, Component.Components[I]);
end;

procedure TComponentTree.Reload;
begin
  with TreeView1, Items do begin
    BeginUpdate;
    Clear;
    AddChild(nil, 'Root');
    ReloadNode(TopItem, Application);
    TopItem.Expand(True);
    EndUpdate;
  end;
end;

initialization
  {$I UComponentTree.lrs}

end.

