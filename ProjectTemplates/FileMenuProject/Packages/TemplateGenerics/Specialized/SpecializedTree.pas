unit SpecializedTree;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
{$MACRO ON}
// TTreeInteger<Integer, Integer>
{$DEFINE TGTreeIndex := Integer}
{$DEFINE TGTreeItem := Integer}
{$DEFINE TGTree := TTreeInteger}
{$DEFINE TGTreeNode := TTreeNodeInteger}
{$DEFINE TGTreeNodeList := TTreeNodeListInteger}
{$DEFINE INTERFACE}
{$I 'GenericTree.inc'}

// TTreeString<Integer, string>
{$DEFINE TGTreeIndex := Integer}
{$DEFINE TGTreeItem := string}
{$DEFINE TGTree := TTreeString}
{$DEFINE TGTreeNode := TTreeNodeString}
{$DEFINE TGTreeNodeList := TTreeNodeListString}
{$DEFINE INTERFACE}
{$I 'GenericTree.inc'}

// TTreePointer<Integer, Pointer>
{$DEFINE TGTreeIndex := Integer}
{$DEFINE TGTreeItem := Pointer}
{$DEFINE TGTree := TTreePointer}
{$DEFINE TGTreeNode := TTreeNodePointer}
{$DEFINE TGTreeNodeList := TTreeNodeListPointer}
{$DEFINE INTERFACE}
{$I 'GenericTree.inc'}

implementation

{$DEFINE IMPLEMENTATION_USES}
{$I 'GenericTree.inc'}

// TTreeInteger<Integer, Integer>
{$DEFINE TGTreeIndex := Integer}
{$DEFINE TGTreeItem := Integer}
{$DEFINE TGTree := TTreeInteger}
{$DEFINE TGTreeNode := TTreeNodeInteger}
{$DEFINE TGTreeNodeList := TTreeNodeListInteger}
{$DEFINE IMPLEMENTATION}
{$I 'GenericTree.inc'}

// TTreeString<Integer, string>
{$DEFINE TGTreeIndex := Integer}
{$DEFINE TGTreeItem := string}
{$DEFINE TGTree := TTreeString}
{$DEFINE TGTreeNode := TTreeNodeString}
{$DEFINE TGTreeNodeList := TTreeNodeListString}
{$DEFINE IMPLEMENTATION}
{$I 'GenericTree.inc'}

// TTreePointer<Integer, Pointer>
{$DEFINE TGTreeIndex := Integer}
{$DEFINE TGTreeItem := Pointer}
{$DEFINE TGTree := TTreePointer}
{$DEFINE TGTreeNode := TTreeNodePointer}
{$DEFINE TGTreeNodeList := TTreeNodeListPointer}
{$DEFINE IMPLEMENTATION}
{$I 'GenericTree.inc'}

end.
