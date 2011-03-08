unit UCoolDockLayout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Contnrs, URectangle, UCoolDocking, Forms,
  DOM, XMLWrite, XMLRead;

type

  { TCoolDockLayoutItem }

  TCoolDockLayoutItem = class
    Name: string;
    ParentName: string;
    Caption: string;
    Visible: Boolean;
    Rect: TRectangle;
    FormState: TFormState;
    UndockSize: TPoint;
    DockStyle: TDockStyle;
    procedure SaveToNode(Node: TDOMNode);
    procedure LoadFromNode(Node: TDOMNode);
  end;

  { TCoolDockLayout }

  TCoolDockLayout = class
    Items: TObjectList; // TList<TCoolDockLayoutItem>
    Name: string;
    procedure SaveToNode(Node: TDOMNode);
    procedure LoadFromNode(Node: TDOMNode);
    constructor Create;
    destructor Destroy; override;
  end;

  { TCoolDockLayoutList }

  TCoolDockLayoutList = class(TComponent)
  public
    Items: TObjectList; // TList<TCoolDockLayout>
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CoolDocking', [TCoolDockLayoutList]);
end;

{ TCoolDockLayoutList }


constructor TCoolDockLayoutList.Create(AOwner: TComponent);
begin
  inherited;
  Items := TObjectList.Create;
end;

destructor TCoolDockLayoutList.Destroy;
begin
  Items.Free;
  inherited Destroy;
end;

procedure TCoolDockLayoutList.LoadFromStream(Stream: TStream);
var
  Doc: TXMLDocument;
  Child: TDOMNode;
  NewItem: TCoolDockLayoutItem;
  NewNode: TDOMNode;
begin
  try
    ReadXMLFile(Doc, Stream);
    Items.Clear;
    with Doc.DocumentElement do begin
      NewNode := FindNode('Items');
      if Assigned(NewNode) then
      with NewNode do begin
        Child := Doc.DocumentElement.FirstChild;
        while Assigned(Child) do begin
          NewItem := TCoolDockLayoutItem.Create;
          NewItem.LoadFromNode(Child);
          Items.Add(NewItem);
          Child := Child.NextSibling;
        end;
      end;
    end;
  finally
    Doc.Free;
  end;
end;

procedure TCoolDockLayoutList.SaveToStream(Stream: TStream);
var
  Doc: TXMLDocument;
  RootNode: TDOMNode;
  I: Integer;
  NewNode: TDOMNode;
  NewNode2: TDOMNode;
begin
  try
    Doc := TXMLDocument.Create;
    with Doc do begin
      RootNode := CreateElement('CoolDockLayout');
      AppendChild(RootNode);
      with RootNode do begin
        NewNode := OwnerDocument.CreateElement('Items');
        for I := 0 to Items.Count - 1 do begin
          NewNode2 := OwnerDocument.CreateElement('Layout');
          TCoolDockLayout(Items[I]).SaveToNode(NewNode2);
          AppendChild(NewNode2);
         end;
        AppendChild(NewNode);
      end;
    end;
    WriteXMLFile(Doc, Stream);
  finally
    Doc.Free;
  end;
end;

procedure TCoolDockLayoutList.LoadFromFile(FileName: string);
var
  Stream: TFileStream;
begin
  try
    Stream := TFileStream.Create(FileName, fmOpenRead);
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCoolDockLayoutList.SaveToFile(FileName: string);
var
  Stream: TFileStream;
begin
  try
    if FileExistsUTF8(FileName) then Stream := TFileStream.Create(FileName, fmOpenReadWrite)
      else Stream := TFileStream.Create(FileName, fmCreate);
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

{ TCoolDockLayoutItem }

procedure TCoolDockLayoutItem.SaveToNode(Node: TDOMNode);
var
  NewNode: TDOMNode;
begin
  with Node do begin
    NewNode := OwnerDocument.CreateElement('Name');
    NewNode.TextContent := UTF8Decode(Name);
    AppendChild(NewNode);
    NewNode := OwnerDocument.CreateElement('ParentName');
    NewNode.TextContent := UTF8Decode(ParentName);
    AppendChild(NewNode);
    NewNode := OwnerDocument.CreateElement('Caption');
    NewNode.TextContent := UTF8Decode(Caption);
    AppendChild(NewNode);
    NewNode := OwnerDocument.CreateElement('FormState');
    NewNode.TextContent := IntToStr(Integer(FormState));
    AppendChild(NewNode);
    NewNode := OwnerDocument.CreateElement('UndockWidth');
    NewNode.TextContent := IntToStr(UndockSize.X);
    AppendChild(NewNode);
    NewNode := OwnerDocument.CreateElement('UndockHeight');
    NewNode.TextContent := IntToStr(UndockSize.Y);
    AppendChild(NewNode);
    NewNode := OwnerDocument.CreateElement('Width');
    NewNode.TextContent := IntToStr(Rect.Width);
    AppendChild(NewNode);
    NewNode := OwnerDocument.CreateElement('Height');
    NewNode.TextContent := IntToStr(Rect.Height);
    AppendChild(NewNode);
    NewNode := OwnerDocument.CreateElement('Top');
    NewNode.TextContent := IntToStr(Rect.Top);
    AppendChild(NewNode);
    NewNode := OwnerDocument.CreateElement('Left');
    NewNode.TextContent := IntToStr(Rect.Left);
    AppendChild(NewNode);
    NewNode := OwnerDocument.CreateElement('Visible');
    NewNode.TextContent := BoolToStr(Visible);
    AppendChild(NewNode);
    NewNode := OwnerDocument.CreateElement('DockStyle');
    NewNode.TextContent := IntToStr(Integer(DockStyle));
    AppendChild(NewNode);
  end;
end;

procedure TCoolDockLayoutItem.LoadFromNode(Node: TDOMNode);
var
  NewNode: TDOMNode;
begin
  with TDOMElement(Node) do begin
    NewNode := FindNode('Name');
    if Assigned(NewNode) then
      Name := UTF8Encode(NewNode.TextContent);
    NewNode := FindNode('ParentName');
    if Assigned(NewNode) then
      ParentName := UTF8Encode(NewNode.TextContent);
    NewNode := FindNode('Caption');
    if Assigned(NewNode) then
      Caption := UTF8Encode(NewNode.TextContent);
    NewNode := FindNode('FormState');
    if Assigned(NewNode) then
      FormState := TFormState(StrToInt(NewNode.TextContent));
    NewNode := FindNode('UndockWidth');
    if Assigned(NewNode) then
      UndockSize.X := StrToInt(NewNode.TextContent);
    NewNode := FindNode('UndockHeight');
    if Assigned(NewNode) then
      UndockSize.Y := StrToInt(NewNode.TextContent);
    NewNode := FindNode('Top');
    if Assigned(NewNode) then
      Rect.Top := StrToInt(NewNode.TextContent);
    NewNode := FindNode('Left');
    if Assigned(NewNode) then
      Rect.Left := StrToInt(NewNode.TextContent);
    NewNode := FindNode('Width');
    if Assigned(NewNode) then
      Rect.Width := StrToInt(NewNode.TextContent);
    NewNode := FindNode('Height');
    if Assigned(NewNode) then
      Rect.Height := StrToInt(NewNode.TextContent);
    NewNode := FindNode('Visible');
    if Assigned(NewNode) then
      Visible := StrToBool(NewNode.TextContent);
    NewNode := FindNode('DockStyle');
    if Assigned(NewNode) then
      DockStyle := TDockStyle(StrToInt(NewNode.TextContent));
  end;
end;

{ TCoolDockLayout }

procedure TCoolDockLayout.SaveToNode(Node: TDOMNode);
var
  NewNode: TDOMNode;
  NewNode2: TDOMNode;
  I: Integer;
begin
  with Node do begin
    NewNode := OwnerDocument.CreateElement('Name');
    NewNode.TextContent := UTF8Decode(Name);
    AppendChild(NewNode);
    NewNode := OwnerDocument.CreateElement('Items');
    for I := 0 to Items.Count - 1 do begin
      NewNode2 := OwnerDocument.CreateElement('Form');
      TCoolDockLayoutItem(Items[I]).SaveToNode(NewNode2);
      AppendChild(NewNode2);
    end;
    AppendChild(NewNode);
  end;
end;

procedure TCoolDockLayout.LoadFromNode(Node: TDOMNode);
var
  NewNode: TDOMNode;
  Child: TDOMNode;
  NewItem: TCoolDockLayoutItem;
begin
  with Node do begin
    NewNode := FindNode('Name');
    if Assigned(NewNode) then
      Name := UTF8Encode(NewNode.TextContent);
    NewNode := FindNode('Items');
    if Assigned(NewNode) then
    with NewNode do begin
      Child := FirstChild;
      while Assigned(Child) do begin
        NewItem := TCoolDockLayoutItem.Create;
        NewItem.LoadFromNode(Child);
        Items.Add(NewItem);
        Child := Child.NextSibling;
      end;
    end;
  end;
end;

constructor TCoolDockLayout.Create;
begin
  Items := TObjectList.Create;
end;

destructor TCoolDockLayout.Destroy;
begin
  Items.Free;
  inherited Destroy;
end;

end.

