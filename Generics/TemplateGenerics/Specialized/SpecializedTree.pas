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
{$DEFINE TGTreeSortCompare := TTreeSortCompareInteger}
{$DEFINE TGTreeStringConverter := TTreeStringConverterInteger}
{$DEFINE INTERFACE}
{$I 'GenericTree.inc'}

// TTreeString<Integer, string>
{$DEFINE TGTreeIndex := Integer}
{$DEFINE TGTreeItem := string}
{$DEFINE TGTree := TTreeString}
{$DEFINE TGTreeNode := TTreeNodeString}
{$DEFINE TGTreeNodeList := TTreeNodeListString}
{$DEFINE TGTreeSortCompare := TTreeSortCompareString}
{$DEFINE TGTreeStringConverter := TTreeStringConverterString}
{$DEFINE INTERFACE}
{$I 'GenericTree.inc'}

// TTreePointer<Integer, Pointer>
{$DEFINE TGTreeIndex := Integer}
{$DEFINE TGTreeItem := Pointer}
{$DEFINE TGTree := TTreePointer}
{$DEFINE TGTreeNode := TTreeNodePointer}
{$DEFINE TGTreeNodeList := TTreeNodeListPointer}
{$DEFINE TGTreeSortCompare := TTreeSortComparePointer}
{$DEFINE TGTreeStringConverter := TTreeStringConverterPointer}
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
{$DEFINE TGTreeSortCompare := TTreeSortCompareInteger}
{$DEFINE TGTreeStringConverter := TTreeStringConverterInteger}
{$DEFINE IMPLEMENTATION}
{$I 'GenericTree.inc'}

// TTreeString<Integer, string>
{$DEFINE TGTreeIndex := Integer}
{$DEFINE TGTreeItem := string}
{$DEFINE TGTree := TTreeString}
{$DEFINE TGTreeNode := TTreeNodeString}
{$DEFINE TGTreeNodeList := TTreeNodeListString}
{$DEFINE TGTreeSortCompare := TTreeSortCompareString}
{$DEFINE TGTreeStringConverter := TTreeStringConverterString}
{$DEFINE IMPLEMENTATION}
{$I 'GenericTree.inc'}

// TTreePointer<Integer, Pointer>
{$DEFINE TGTreeIndex := Integer}
{$DEFINE TGTreeItem := Pointer}
{$DEFINE TGTree := TTreePointer}
{$DEFINE TGTreeNode := TTreeNodePointer}
{$DEFINE TGTreeNodeList := TTreeNodeListPointer}
{$DEFINE TGTreeSortCompare := TTreeSortComparePointer}
{$DEFINE TGTreeStringConverter := TTreeStringConverterPointer}
{$DEFINE IMPLEMENTATION}
{$I 'GenericTree.inc'}

end.
