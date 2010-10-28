
{$INCLUDE 'ListImplementation.tpl'}

{ TGTreeNode }

procedure TGTreeNode.Clear;
begin
  Childs.Clear;
end;

{ TGTree }

procedure TGTree.Clear;
begin
  if Assigned(TopItem) then TopItem.Clear;
end;

