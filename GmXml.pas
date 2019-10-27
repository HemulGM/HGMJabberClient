unit GmXml;

interface

uses
  Classes, SysUtils;

const
  COMP_VERSION = 0.13;
  XML_SPECIFICATION = '<?xml version="1.0"%s?>';

type
  TGmXmlNode = class;

  TGmXmlNodeList = class;

  TGmXmlEnumNodeEvent = procedure(Sender: TObject; ANode: TGmXmlNode) of object;

  // *** TGmXmlNodeElement ***

  TGmXmlNodeAttribute = class
  private
    FName: string;
    FValue: string;
    procedure SetName(AValue: string);
    procedure SetValue(AValue: string);
  public
    property Name: string read FName write SetName;
    property Value: string read FValue write SetValue;
  end;

  // *** TGmXmlNode ***

  TGmXmlNode = class(TPersistent)
  private
    FChildren: TGmXmlNodeList;
    FElement: TGmXmlNodeAttribute;
    FName: string;
    FParent: TGmXmlNode;
    FValue: string;
    FParams: TStringList;
    FSingleClose: Boolean;
    // events...
    function GetAsDisplayString: string;
    function GetIsLeafNode: Boolean;
    function GetAsBoolean: Boolean;
    function GetAsFloat: Extended;
    function GetAsInteger: integer;
    function GetAsString: string;
    function GetLevel: integer;
    function CloseTag: string;
    function OpenTag: string;
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsFloat(const Value: Extended);
    procedure SetAsInteger(const Value: integer);
    procedure SetAsString(const Value: string);
    procedure SetAsDisplayString(const Value: string);
    procedure SetName(Value: string); virtual;
    function GetParams: TStrings;
    procedure SetParams(const Value: TStrings);
    function ReplaceStrToXML(Value: string): string;
    procedure SetSingleClose(const Value: Boolean);
  public
    constructor Create(AParentNode: TGmXmlNode); virtual;
    destructor Destroy; override;
    procedure EnumerateNodes(ACallback: TGmXmlEnumNodeEvent);
    property AsDisplayString: string read GetAsDisplayString write SetAsDisplayString;
    property AsString: string read GetAsString write SetAsString;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsFloat: Extended read GetAsFloat write SetAsFloat;
    property AsInteger: integer read GetAsInteger write SetAsInteger;
    property Attribute: TGmXmlNodeAttribute read FElement write FElement;
    property Children: TGmXmlNodeList read FChildren;
    property IsLeafNode: Boolean read GetIsLeafNode;
    property Level: integer read GetLevel;
    property Name: string read FName write SetName;
    property Parent: TGmXmlNode read FParent;
    property Params: TStrings read GetParams write SetParams;
    property SingleClose: Boolean read FSingleClose write SetSingleClose;
  end;

  // *** TGmXmlNodeList ***

  TGmXmlNodeList = class
  private
    FCurrentNode: TGmXmlNode;
    FList: TList;
    function GetCount: integer;
    function GetNode(index: integer): TGmXmlNode;
    function GetNodeByName(AName: string): TGmXmlNode;
    procedure SetNodeByName(AName: string; ANode: TGmXmlNode);
    function GetRoot: TGmXmlNode;
    procedure AddNode(ANode: TGmXmlNode);
    procedure SetNode(index: integer; const Value: TGmXmlNode);
  public
    constructor Create(AParent: TGmXmlNode);
    destructor Destroy; override;
    function AddLeaf(AName: string): TGmXmlNode;
    function AddOpenTag(AName: string; SingleClose: Boolean = False): TGmXmlNode;
    procedure AddCloseTag;
    //procedure NextNode;
    procedure Clear;
    property Count: integer read GetCount;
    property CurrentNode: TGmXmlNode read FCurrentNode write FCurrentNode;
    property Node[index: integer]: TGmXmlNode read GetNode write SetNode; default;
    property NodeByName[AName: string]: TGmXmlNode read GetNodeByName write SetNodeByName;
    property Root: TGmXmlNode read GetRoot;
  end;

  // *** TGmXML ***

  TGmXML = class
  private
    FAutoIndent: Boolean;
    FEncoding: string;
    FIncludeHeader: Boolean;
    FNodes: TGmXmlNodeList;
    FStrings: TStringList;
    function GetDisplayText: string;
    function GetEncodingStr: string;
    function GetText(ReplaceEscapeChars: Boolean): string;
    function GetXmlText: string;
    procedure SetAsText(Value: string);
    procedure SetAutoIndent(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(AFileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(AFilename: string);
    procedure SaveToStream(Stream: TStream);
    property DisplayText: string read GetDisplayText;
    property Nodes: TGmXmlNodeList read FNodes;
    property Text: string read GetXmlText write SetAsText;
  published
    property AutoIndent: Boolean read FAutoIndent write SetAutoIndent default True;
    property Encoding: string read FEncoding write FEncoding;
    property IncludeHeader: Boolean read FIncludeHeader write FIncludeHeader default True;
  end;

implementation

//------------------------------------------------------------------------------

// *** Unit Functions ***

function StrPos(const SubStr, S: string; Offset: Cardinal = 1): Integer;
var
  I, X: Integer;
  Len, LenSubStr: Integer;
begin
  if Offset = 1 then
    Result := Pos(SubStr, S)
  else
  begin
    I := Offset;
    LenSubStr := Length(SubStr);
    Len := Length(S) - LenSubStr + 1;
    while I <= Len do
    begin
      if S[I] = SubStr[1] then
      begin
        X := 1;
        while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
          Inc(X);
        if (X = LenSubStr) then
        begin
          Result := I;
          exit;
        end;
      end;
      Inc(I);
    end;
    Result := 0;
  end;
end;

procedure ReplaceText(var AText: string; AFind, AReplace: string);
var
  Index: integer;
begin
  Index := 1;
  while StrPos(AFind, AText, Index) <> 0 do
  begin
    Index := StrPos(AFind, AText, Index);
    Delete(AText, Index, Length(AFind));
    Insert(AReplace, AText, Index);
    Inc(Index, Length(AReplace));
  end;
end;

//------------------------------------------------------------------------------

// *** TGmXmlNodeElement ***

procedure TGmXmlNodeAttribute.SetName(AValue: string);
begin
  FName := Trim(AValue);
end;

procedure TGmXmlNodeAttribute.SetValue(AValue: string);
begin
  FValue := Trim(AValue);
  ReplaceText(FValue, '"', '');
end;

//------------------------------------------------------------------------------

// *** TGmXmlNode ***

constructor TGmXmlNode.Create(AParentNode: TGmXmlNode);
begin
  inherited Create;
  FSingleClose := False;
  FChildren := TGmXmlNodeList.Create(Self);
  FElement := TGmXmlNodeAttribute.Create;
  FParams := TStringList.Create;
  FParams.Clear;
  FParent := AParentNode;
  FElement.Name := '';
  FElement.Value := '';
end;

destructor TGmXmlNode.Destroy;
begin
  FElement.Free;
  FChildren.Free;
  FParams.Free;
  inherited Destroy;
end;

function TGmXmlNode.CloseTag: string;
begin
  Result := '</' + FName + '>';
end;

function TGmXmlNode.OpenTag: string;
var
  ICount: Integer;
  IParams: string;
begin
{ DONE : Добавляем обработку атрибутов }
  IParams := '';
  if FParams.Count < 1 then
    Result := Format('<%s>', [Name])
  else
  begin
    for ICount := 0 to FParams.Count - 1 do
    begin
      IParams := IParams + FParams.Names[ICount] + '="' + ReplaceStrToXML(FParams.Values[FParams.Names[ICount]]) + '" ';
    end;
    IParams := Trim(IParams);
    Result := Format('<%s %s>', [Name, IParams]);
  end;
end;

procedure TGmXmlNode.EnumerateNodes(ACallback: TGmXmlEnumNodeEvent);
var
  ICount: integer;
begin
  for ICount := 0 to FChildren.Count - 1 do
  begin
    if Assigned(ACallback) then
      ACallback(Self, FChildren[ICount]);
  end;
end;

function TGmXmlNode.GetAsBoolean: Boolean;
begin
  Result := Boolean(StrToInt(FValue));
end;

function TGmXmlNode.GetAsFloat: Extended;
begin
  Result := StrToFloat(FValue);
end;

function TGmXmlNode.GetAsInteger: integer;
begin
  Result := StrToInt(FValue);
end;

function TGmXmlNode.GetAsString: string;
begin
  Result := FValue;
end;

function TGmXmlNode.GetLevel: integer;
var
  AParent: TGmXmlNode;
begin
  AParent := Parent;
  Result := 0;
  while AParent <> nil do
  begin
    AParent := AParent.Parent;
    Inc(Result);
  end;
end;

procedure TGmXmlNode.SetAsBoolean(const Value: Boolean);
begin
  FValue := IntToStr(Ord(Value));
end;

procedure TGmXmlNode.SetAsDisplayString(const Value: string);
begin
//
{ TODO : Возможна пробелма из-за парсера }
  FValue := Value;
  // replace any illegal characters...
  ReplaceText(FValue, '&', '&amp;');
  ReplaceText(FValue, '<', '&lt;');
  ReplaceText(FValue, '>', '&gt;');
  ReplaceText(FValue, '''', '&apos;');
  ReplaceText(FValue, '"', '&quot;');
end;

procedure TGmXmlNode.SetAsFloat(const Value: Extended);
begin
  FValue := FloatToStr(Value);
end;

procedure TGmXmlNode.SetAsInteger(const Value: integer);
begin
  FValue := IntToStr(Value);
end;

procedure TGmXmlNode.SetAsString(const Value: string);
begin
{ TODO : Возможна пробелма из-за парсера }
  FValue := Value;
  // replace any illegal characters...
//  ReplaceText(FValue, '&', '&amp;');
//  ReplaceText(FValue, '<', '&lt;');
//  ReplaceText(FValue, '>', '&gt;');
//  ReplaceText(FValue, '''', '&apos;');
//  ReplaceText(FValue, '"', '&quot;');
end;

function TGmXmlNode.GetAsDisplayString: string;
begin
  Result := FValue;
  // replace any illegal characters...
  ReplaceText(Result, '&amp;', '&');
  ReplaceText(Result, '&lt;', '<');
  ReplaceText(Result, '&gt;', '>');
  ReplaceText(Result, '&apos;', '''');
  ReplaceText(Result, '&quot;', '"');
end;

function TGmXmlNode.GetIsLeafNode: Boolean;
begin
  Result := FChildren.Count = 0;
end;

procedure TGmXmlNode.SetName(Value: string);
var
  AElement: string;
  AName: string;
  AFull: string;
begin
  FName := Value;
  if FName[1] = '<' then
    Delete(FName, 1, 1);
  if (FName[Length(FName)] = '>') and (FName[Length(FName) - 1] = '/') then
    Delete(FName, Length(FName) - 1, 2)
  else if FName[Length(FName)] = '>' then
    Delete(FName, Length(FName), 1);

  Trim(FName);
{ DONE : Добавить нормальную обработку добавления атрибутов }
  // extract element if one exists...
  if Pos('=', FName) <> 0 then
  begin
//    for ACount := 0
//    W
    AFull := Copy(FName, Pos(' ', FName), Length(FName));
    FName := Copy(FName, 1, Pos(' ', FName) - 1);
    AFull := Trim(AFull);
    AElement := AFull;
    while Length(AFull) > 0 do
    begin
      // получаем имя атрибута
      AName := copy(AFull, 1, pos('=', AFull) - 1);
      AName := Trim(AName);
      // получаем содержимое атрибута
      AElement := copy(AFull, pos('=', AFull) + 1, Length(AFull));
      AElement := Trim(AElement);
      AElement := copy(AFull, pos('"', AFull) + 1, Length(AFull));
      AFull := copy(AElement, pos('"', AElement) + 1, Length(AElement));
      AElement := copy(AElement, 1, pos('"', AElement) - 1);
      AFull := Trim(AFull);
      FParams.Values[AName] := AElement;

    end;

//    FElement.Name := Copy(AElement, 0, Pos('=', AElement)-1);
//    AElement := Copy(AElement, Pos('"', AElement), Length(AElement));
//    ReplaceText(AElement, '"', '');
//    FElement.Value := AElement;

//    FParams.Values[FElement.Name] := AElement;
  end;
end;

//------------------------------------------------------------------------------

// *** TGmXmlNodeList ***

constructor TGmXmlNodeList.Create(AParent: TGmXmlNode);
begin
  inherited Create;
  FList := TList.Create;
  FCurrentNode := AParent;
end;

destructor TGmXmlNodeList.Destroy;
var
  ICount: integer;
begin
  for ICount := Count - 1 downto 0 do
    Node[ICount].Free;
  FList.Free;
  inherited Destroy;
end;

function TGmXmlNodeList.AddLeaf(AName: string): TGmXmlNode;
begin
  Result := AddOpenTag(AName);
  AddCloseTag;
end;

function TGmXmlNodeList.AddOpenTag(AName: string; SingleClose: Boolean): TGmXmlNode;
begin
  Result := TGmXmlNode.Create(FCurrentNode);
  Result.SingleClose := SingleClose;
  Result.Name := AName;
  if FCurrentNode = nil then
    AddNode(Result)
  else
    FCurrentNode.Children.AddNode(Result);

  FCurrentNode := Result;
end;

procedure TGmXmlNodeList.AddCloseTag;
begin
  if Assigned(FCurrentNode) then
    FCurrentNode := FCurrentNode.Parent;
end;

{procedure TGmXmlNodeList.NextNode;
var
  AIndex: integer;
begin
  AIndex := FList.IndexOf(FCurrentNode);
  if AIndex < FList.Count then
  FCurrentNode := TGmXmlNode(FList[AIndex]);
end;}

procedure TGmXmlNodeList.Clear;
var
  ICount: integer;
begin
  for ICount := 0 to FList.Count - 1 do
  begin
    Node[ICount].Free;
    Node[ICount] := nil;
  end;
  FList.Clear;
  FCurrentNode := nil;
end;

function TGmXmlNodeList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TGmXmlNodeList.GetNode(index: integer): TGmXmlNode;
begin
  Result := TGmXmlNode(FList[index]);
end;

function TGmXmlNodeList.GetNodeByName(AName: string): TGmXmlNode;
var
  ICount: integer;
begin
  Result := nil;
  for ICount := 0 to Count - 1 do
  begin
    if Node[ICount].Name = AName then
    begin
      Result := Node[ICount];
      Exit;
    end;
  end;
end;

procedure TGmXmlNodeList.SetNodeByName(AName: string; ANode: TGmXmlNode);
var
  ICount: integer;
begin
  for ICount := 0 to Count - 1 do
  begin
    if Node[ICount].Name = AName then
    begin
      Node[ICount] := ANode;
      Exit;
    end;
  end;
end;

function TGmXmlNodeList.GetRoot: TGmXmlNode;
begin
  Result := nil;
  if Count > 0 then
    Result := Node[0];
end;

procedure TGmXmlNodeList.AddNode(ANode: TGmXmlNode);
begin
  FList.Add(ANode);
end;

procedure TGmXmlNodeList.SetNode(index: integer; const Value: TGmXmlNode);
begin
  FList[index] := Value;
end;

//------------------------------------------------------------------------------

// *** TGmXml ***

constructor TGmXml.Create;
begin
  inherited Create;
  FStrings := TStringList.Create;
  FNodes := TGmXmlNodeList.Create(nil);
  FIncludeHeader := False;
  FAutoIndent := True;
  FEncoding := '';
end;

destructor TGmXml.Destroy;
begin
  FNodes.Free;
  FStrings.Free;
  inherited Destroy;
end;

function TGmXml.GetDisplayText: string;
begin
  Result := GetText(True);
end;

function TGmXml.GetEncodingStr: string;
begin
  Result := '';
  if FEncoding <> '' then
    Result := Format(' encoding="%s"', [FEncoding]);
end;

function TGmXml.GetText(ReplaceEscapeChars: Boolean): string;

  procedure NodeToStringList(var AXml: TStringList; ANode: TGmXmlNode; AReplaceChars: Boolean);
  var
    ICount: integer;
    AValue: string;
    AOpenTag: string;
    ACloseTag: string;
  begin
    if ANode.IsLeafNode then
    begin
      if AReplaceChars then
        AValue := ANode.AsDisplayString
      else
        AValue := ANode.AsString;
      if AValue = '' then
      begin
        // если значение равно пустоте то в конец добавляем не цельную а только завершающую строфу.
        AOpenTag := ANode.OpenTag;
        ACloseTag := ANode.CloseTag;
        AOpenTag := Copy(AOpenTag, 1, length(AOpenTag) - 1);
        if ANode.SingleClose then
          ACloseTag := '>'
        else
          ACloseTag := '/>';
        AXml.Add(AOpenTag + ACloseTag);
      end
      else
        AXml.Add(ANode.OpenTag + AValue + ANode.CloseTag);
    end
    else
    begin
      AXml.Add(ANode.OpenTag);
      for ICount := 0 to ANode.FChildren.Count - 1 do
        NodeToStringList(AXml, ANode.Children.Node[ICount], AReplaceChars);
      AXml.Add(ANode.CloseTag);
    end;
  end;

var
  ICount: integer;
begin
  FStrings.Clear;
  if FNodes.Count = 0 then
    Exit;
  if FIncludeHeader then
    FStrings.Add(Format(XML_SPECIFICATION, [GetEncodingStr]));
  for ICount := 0 to FNodes.Count - 1 do
    NodeToStringList(FStrings, FNodes.Node[ICount], ReplaceEscapeChars);
  Result := FStrings.Text;
end;

function TGmXml.GetXmlText: string;
begin
  Result := GetText(False);
end;

procedure TGmXml.SetAsText(Value: string);
var
  ACursor: integer;
  AText: string;
  ATag: string;
  AValue: string;
  ATags: string;
begin
  AText := Value;
  ACursor := 1;
  ATags := '';
  while ACursor < Length(Value) do
  begin
    AValue := '';
    if Value[ACursor] = '<' then
    begin
      // reading a tag
      ATag := '<';
      while Value[ACursor] <> '>' do
      begin
        Inc(ACursor);
        ATag := ATag + Value[ACursor];
      end;
      if ATag[2] = '/' then
        Nodes.AddCloseTag
      else if (ATag[2] <> '?') then
        if (ATag[Length(ATag) - 1] = '/') then
        begin
          Nodes.AddOpenTag(ATag);
          Nodes.AddCloseTag;
        end
        else
          Nodes.AddOpenTag(ATag);
    end
    else
    begin
      // reading a value...
      while (Value[ACursor] <> '<') and (ACursor < Length(Value)) do
      begin
        AValue := AValue + Value[ACursor];
        Inc(ACursor);
      end;
      if Assigned(Nodes.CurrentNode) then
        Nodes.CurrentNode.AsString := AValue;
      Dec(ACursor);
    end;
    Inc(ACursor);
  end;
end;

procedure TGmXML.SetAutoIndent(const Value: Boolean);
begin
  FAutoIndent := Value;
end;

procedure TGmXml.LoadFromFile(AFileName: string);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    AStream.LoadFromFile(AFileName);
    AStream.Seek(0, soFromBeginning);
    LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TGmXml.LoadFromStream(Stream: TStream);
var
  ALines: TStringList;
begin
  if Stream.Size = 0 then
    Exit;
  ALines := TStringList.Create;
  try
    Stream.Position := 0;
    ALines.LoadFromStream(Stream);
    Text := ALines.Text;
  finally
    ALines.Free;
  end;
end;

procedure TGmXml.SaveToFile(AFilename: string);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    SaveToStream(AStream);
    AStream.SaveToFile(AFilename);
  finally
    AStream.Free;
  end;
end;

procedure TGmXml.SaveToStream(Stream: TStream);
begin
  GetText(False);
  FStrings.SaveToStream(Stream);
end;

function TGmXmlNode.GetParams: TStrings;
begin
  Result := FParams;
end;

procedure TGmXmlNode.SetParams(const Value: TStrings);
begin
  FParams.Assign(Value);
end;

procedure TGmXmlNode.SetSingleClose(const Value: Boolean);
begin
  FSingleClose := Value;
end;

function TGmXmlNode.ReplaceStrToXML(Value: string): string;
var
  AValue: string;
begin
  AValue := Value;
  ReplaceText(AValue, '&', '&amp;');
  ReplaceText(AValue, '<', '&lt;');
  ReplaceText(AValue, '>', '&gt;');
  ReplaceText(AValue, '''', '&apos;');
  ReplaceText(AValue, '"', '&quot;');
  Result := AValue;
end;

end.

