unit UCoolDockLayout;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Contnrs, URectangle, Forms, UCoolDockCommon,
  DOM, XMLWrite, XMLRead, Controls;

type
  TCoolDockLayout = class;

  { TCoolDockLayoutItem }

  TCoolDockLayoutItem = class
    Parent: TCoolDockLayout;
    Name: string;
    StoredClassName: string;
    ParentName: string;
    HostDockSiteName: string;
    Caption: string;
    Visible: Boolean;
    Rect: TRectangle;
    RestoredRect: TRectangle;
    WindowState: TWindowState;
    UndockSize: TPoint;
    DockStyle: TDockStyle;
    procedure SaveToNode(Node: TDOMNode);
    procedure LoadFromNode(Node: TDOMNode);
    procedure Store(Form: TWinControl);
    procedure Restore(Form: TWinControl);
    constructor Create;
    destructor Destroy; override;
  end;

  { TCoolDockLayout }

  TCoolDockLayout = class
    Items: TObjectList; // TList<TCoolDockLayoutItem>
    Name: string;
    procedure SaveToNode(Node: TDOMNode);
    procedure LoadFromNode(Node: TDOMNode);
    function FindByName(Name: string): TCoolDockLayoutItem;
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

uses
  UCoolDocking;

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
  if I < Items.Count then Result := TCoolDockLayout(Items[I])
    else Result := nil;
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
    NewNode := OwnerDocument.CreateElement('StoredClassName');
    NewNode.TextContent := UTF8Decode(StoredClassName);
    AppendChild(NewNode);
    NewNode := OwnerDocument.CreateElement('HostDockSiteName');
    NewNode.TextContent := UTF8Decode(HostDockSiteName);
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
    NewNode := FindNode('StoredClassName');
    if Assigned(NewNode) then
      StoredClassName := UTF8Encode(NewNode.TextContent);
    NewNode := FindNode('HostDockSiteName');
    if Assigned(NewNode) then
      HostDockSiteName := UTF8Encode(NewNode.TextContent);
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

procedure TCoolDockLayoutItem.Store(Form: TWinControl);
var
  NewItem: TCoolDockLayoutItem;
begin
  Name := Form.Name;
  StoredClassName := Form.ClassName;
  Caption := Form.Caption;
  UndockSize.X := Form.UndockWidth;
  UndockSize.Y := Form.UndockHeight;
  Visible := Form.Visible;
  Rect.Left := Form.Left;
  Rect.Top := Form.Top;
  Rect.Width := Form.Width;
  Rect.Height := Form.Height;
  if Form is TForm then begin
    RestoredRect.Left := TForm(Form).RestoredLeft;
    RestoredRect.Top := TForm(Form).RestoredTop;
    RestoredRect.Width := TForm(Form).RestoredWidth;
    RestoredRect.Height := TForm(Form).RestoredHeight;
    WindowState := TForm(Form).WindowState;
  end;
  if Assigned(Form.Parent) then
    ParentName := Form.Parent.Name
    else ParentName := '';
  if Assigned(Form.HostDockSite) then begin
    if Assigned(Form.HostDockSite.Parent) and (Form.HostDockSite.Parent is TForm) then
    begin
      HostDockSiteName := Form.HostDockSite.Parent.Name;
      if not Assigned(Parent.FindByName(HostDockSiteName)) then begin
        NewItem := TCoolDockLayoutItem.Create;
        NewItem.Parent := Parent;
        NewItem.DockStyle := TCoolDockManager(Form.HostDockSite.Parent.DockManager).DockStyle;
        Parent.Items.Add(NewItem);
        NewItem.Store(Form.HostDockSite.Parent);
      end;
    end;
  end else HostDockSiteName := '';
end;

procedure TCoolDockLayoutItem.Restore(Form: TWinControl);
var
  ParentComponent: TComponent;
  ParentLayoutItem: TCoolDockLayoutItem;
  FormClass: TFormClass;
begin
  if Form is TForm then
  if WindowState = wsMaximized then begin
    TForm(Form).SetRestoredBounds(RestoredRect.Left, RestoredRect.Top,
      RestoredRect.Width, RestoredRect.Height);
    TForm(Form).WindowState := WindowState;
  end else begin
    TForm(Form).WindowState := WindowState;
    TForm(Form).SetRestoredBounds(RestoredRect.Left, RestoredRect.Top,
      RestoredRect.Width, RestoredRect.Height);
  end;
  Form.Name := Name;
  Form.Caption := Caption;
  Form.SetBounds(Rect.Left, Rect.Top, Rect.Width, Rect.Height);
  Form.UndockWidth := UndockSize.X;
  Form.UndockHeight := UndockSize.Y;
  Form.Visible := Visible;
  if HostDockSiteName <> '' then begin
    ParentComponent := FindGlobalComponent(HostDockSiteName);
    if not Assigned(ParentComponent) then begin
      ParentLayoutItem := Parent.FindByName(HostDockSiteName);
      if Assigned(ParentLayoutItem) then begin
        if ParentLayoutItem.StoredClassName <> '' then begin
          //ParentComponent := TComponent(FindClass(ParentLayoutItem.StoredClassName).Create);
          if (ParentLayoutItem.StoredClassName = 'TCoolDockConjoinForm') then begin
            FormClass := TFormClass(FindClass('TCoolDockConjoinForm'));
            if FormClass = TCoolDockConjoinForm then begin
              ParentComponent := TCoolDockConjoinForm.Create(Application);
              TCoolDockManager(TCoolDockConjoinForm(ParentComponent).Panel.DockManager).DockStyle := ParentLayoutItem.DockStyle;
              ParentLayoutItem.Restore(TWinControl(ParentComponent));
            end;
          end;
        end;
      end;
    end;
    if Assigned(ParentComponent) and (ParentComponent is TCoolDockConjoinForm) then
      Form.ManualDock(TCoolDockConjoinForm(ParentComponent).Panel);
  end else
  if Assigned(Form.HostDockSite) then Form.ManualFloat(Rect.AsTRect);
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
        NewItem.Parent := Self;
        NewItem.LoadFromNode(Child);
        Items.Add(NewItem);
        Child := Child.NextSibling;
      end;
    end;
  end;
end;

function TCoolDockLayout.FindByName(Name: string): TCoolDockLayoutItem;
var
  I: Integer;
begin
  I := 0;
  while (I < Items.Count) and (TCoolDockLayoutItem(Items[I]).Name <> Name) do Inc(I);
  if I < Items.Count then Result := TCoolDockLayoutItem(Items[I])
    else Result := nil;
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
    NewItem.Parent := Self;
    NewItem.Store(Form);
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
    if Assigned(Form) then Restore(Form);
  end;
end;

end.

