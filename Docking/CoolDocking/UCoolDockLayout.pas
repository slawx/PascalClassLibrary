unit UCoolDockLayout;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Contnrs, URectangle, Forms, UCoolDockCommon,
  DOM, XMLWrite, XMLRead;

type

  { TCoolDockLayoutItem }

  TCoolDockLayoutItem = class
    Name: string;
    ParentName: string;
    Caption: string;
    Visible: Boolean;
    Rect: TRectangle;
    RestoredRect: TRectangle;
    WindowState: TWindowState;
    UndockSize: TPoint;
    DockStyle: TDockStyle;
    procedure SaveToNode(Node: TDOMNode);
    procedure LoadFromNode(Node: TDOMNode);
    constructor Create;
    destructor Destroy; override;
  end;

  { TCoolDockLayout }

  TCoolDockLayout = class
    Items: TObjectList; // TList<TCoolDockLayoutItem>
    Name: string;
    procedure SaveToNode(Node: TDOMNode);
    procedure LoadFromNode(Node: TDOMNode);
    constructor Create;
    destructor Destroy; override;
    procedure Store;
    procedure Restore;
  end;

  { TCoolDockLayoutList }

  TCoolDockLayoutList = class(TComponent)
  public
    Items: TObjectList; // TList<TCoolDockLayout>
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string);
    procedure PopulateStringList(List: TStrings);
    function FindByName(Name: string): TCoolDockLayout;
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
  NewItem: TCoolDockLayout;
  NewNode: TDOMNode;
begin
  try
    ReadXMLFile(Doc, Stream);
    Items.Clear;
    with Doc.DocumentElement do begin
      NewNode := FindNode('Items');
      if Assigned(NewNode) then
      with NewNode do begin
        Child := FirstChild;
        while Assigned(Child) do begin
          NewItem := TCoolDockLayout.Create;
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
        with NewNode do
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
    Stream.Size := 0;
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCoolDockLayoutList.PopulateStringList(List: TStrings);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to Items.Count - 1 do
    List.AddObject(TCoolDockLayout(Items[I]).Name, TCoolDockLayout(Items[I]));
end;

function TCoolDockLayoutList.FindByName(Name: string): TCoolDockLayout;
var
  I: Integer;
begin
  I := 0;
  while (I < Items.Count) and (TCoolDockLayout(Items[I]).Name <> Name) do Inc(I);
  if I < Items.Count then Result := TCoolDockLayout(Items[I]) else Result := nil;
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
    NewNode := OwnerDocument.CreateElement('WindowState');
    NewNode.TextContent := IntToStr(Integer(WindowState));
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
    NewNode := OwnerDocument.CreateElement('RestoredWidth');
    NewNode.TextContent := IntToStr(RestoredRect.Width);
    AppendChild(NewNode);
    NewNode := OwnerDocument.CreateElement('RestoredHeight');
    NewNode.TextContent := IntToStr(RestoredRect.Height);
    AppendChild(NewNode);
    NewNode := OwnerDocument.CreateElement('RestoredTop');
    NewNode.TextContent := IntToStr(RestoredRect.Top);
    AppendChild(NewNode);
    NewNode := OwnerDocument.CreateElement('RestoredLeft');
    NewNode.TextContent := IntToStr(RestoredRect.Left);
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
    NewNode := FindNode('WindowState');
    if Assigned(NewNode) then
      WindowState := TWindowState(StrToInt(NewNode.TextContent));
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
    NewNode := FindNode('RestoredTop');
    if Assigned(NewNode) then
      RestoredRect.Top := StrToInt(NewNode.TextContent);
    NewNode := FindNode('RestoredLeft');
    if Assigned(NewNode) then
      RestoredRect.Left := StrToInt(NewNode.TextContent);
    NewNode := FindNode('RestoredWidth');
    if Assigned(NewNode) then
      RestoredRect.Width := StrToInt(NewNode.TextContent);
    NewNode := FindNode('RestoredHeight');
    if Assigned(NewNode) then
      RestoredRect.Height := StrToInt(NewNode.TextContent);
  end;
end;

constructor TCoolDockLayoutItem.Create;
begin
  Rect := TRectangle.Create;
  RestoredRect := TRectangle.Create;
end;

destructor TCoolDockLayoutItem.Destroy;
begin
  Rect.Free;
  RestoredRect.Free;
  inherited Destroy;
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
    with NewNode do
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

procedure TCoolDockLayout.Store;
var
  I: Integer;
  Form: TForm;
  NewItem: TCoolDockLayoutItem;
begin
  Items.Clear;
  for I := 0 to Application.ComponentCount - 1 do
  if (Application.Components[I] is TForm) then begin
    Form := (Application.Components[I] as TForm);
    NewItem := TCoolDockLayoutItem.Create;
    NewItem.Name := Form.Name;
    NewItem.Caption := Form.Caption;
    NewItem.UndockSize.X := Form.UndockWidth;
    NewItem.UndockSize.Y := Form.UndockHeight;
    NewItem.Visible := Form.Visible;
    NewItem.Rect.Left := Form.Left;
    NewItem.Rect.Top := Form.Top;
    NewItem.Rect.Width := Form.Width;
    NewItem.Rect.Height := Form.Height;
    NewItem.RestoredRect.Left := Form.RestoredLeft;
    NewItem.RestoredRect.Top := Form.RestoredTop;
    NewItem.RestoredRect.Width := Form.RestoredWidth;
    NewItem.RestoredRect.Height := Form.RestoredHeight;
    NewItem.WindowState := Form.WindowState;
    Items.Add(NewItem);
  end;
end;

procedure TCoolDockLayout.Restore;
var
  Form: TForm;
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
  with TCoolDockLayoutItem(Items[I]) do begin
    Form := TForm(Application.FindComponent(Name));
    if WindowState = wsMaximized then begin
      Form.SetRestoredBounds(RestoredRect.Left, RestoredRect.Top,
        RestoredRect.Width, RestoredRect.Height);
      Form.WindowState := WindowState;
    end else begin
      Form.WindowState := WindowState;
      Form.SetRestoredBounds(RestoredRect.Left, RestoredRect.Top,
        RestoredRect.Width, RestoredRect.Height);
    end;
    Form.Caption := Caption;
    Form.SetBounds(Rect.Left, Rect.Top, Rect.Width, Rect.Height);
    Form.UndockWidth := UndockSize.X;
    Form.UndockHeight := UndockSize.Y;
    Form.Visible := Visible;
  end;
end;

end.

