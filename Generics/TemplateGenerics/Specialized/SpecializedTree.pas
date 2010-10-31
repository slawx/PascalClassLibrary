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
{$INCLUDE '..\Generic\GenericTree.inc'}

// TTreeString<Integer, string>
{$DEFINE TGTreeIndex := Integer}
{$DEFINE TGTreeItem := string}
{$DEFINE TGTree := TTreeString}
{$DEFINE TGTreeNode := TTreeNodeString}
{$DEFINE TGTreeNodeList := TTreeNodeListString}
{$DEFINE TGTreeSortCompare := TTreeSortCompareString}
{$DEFINE TGTreeStringConverter := TTreeStringConverterString}
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericTree.inc'}

// TTreePointer<Integer, Pointer>
{$DEFINE TGTreeIndex := Integer}
{$DEFINE TGTreeItem := Pointer}
{$DEFINE TGTree := TTreePointer}
{$DEFINE TGTreeNode := TTreeNodePointer}
{$DEFINE TGTreeNodeList := TTreeNodeListPointer}
{$DEFINE TGTreeSortCompare := TTreeSortComparePointer}
{$DEFINE TGTreeStringConverter := TTreeStringConverterPointer}
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericTree.inc'}

implementation

{$DEFINE IMPLEMENTATION_USES}
{$INCLUDE '..\Generic\GenericTree.inc'}

// TTreeInteger<Integer, Integer>
{$DEFINE TGTreeIndex := Integer}
{$DEFINE TGTreeItem := Integer}
{$DEFINE TGTree := TTreeInteger}
{$DEFINE TGTreeNode := TTreeNodeInteger}
{$DEFINE TGTreeNodeList := TTreeNodeListInteger}
{$DEFINE TGTreeSortCompare := TTreeSortCompareInteger}
{$DEFINE TGTreeStringConverter := TTreeStringConverterInteger}
{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericTree.inc'}

// TTreeString<Integer, string>
{$DEFINE TGTreeIndex := Integer}
{$DEFINE TGTreeItem := string}
{$DEFINE TGTree := TTreeString}
{$DEFINE TGTreeNode := TTreeNodeString}
{$DEFINE TGTreeNodeList := TTreeNodeListString}
{$DEFINE TGTreeSortCompare := TTreeSortCompareString}
{$DEFINE TGTreeStringConverter := TTreeStringConverterString}
{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericTree.inc'}

// TTreePointer<Integer, Pointer>
{$DEFINE TGTreeIndex := Integer}
{$DEFINE TGTreeItem := Pointer}
{$DEFINE TGTree := TTreePointer}
{$DEFINE TGTreeNode := TTreeNodePointer}
{$DEFINE TGTreeNodeList := TTreeNodeListPointer}
{$DEFINE TGTreeSortCompare := TTreeSortComparePointer}
{$DEFINE TGTreeStringConverter := TTreeStringConverterPointer}
{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericTree.inc'}

end.
