unit Jabber.Actions;

interface

uses
  Jabber, Jabber.Types, GmXml, System.SysUtils, Vcl.Dialogs;

type
  //Общие классы

  TXMPPActionIQ = class(TXMPPAction)
  private
    FID: string;
  public
    constructor Create(AOwner: TXMPPActions);
    property ID: string read FID write FID;
  end;

  TXMPPActionPresence = class(TXMPPAction)
  public
    constructor Create(AOwner: TXMPPActions);
  end;

  TXMPPActionStreamError = class(TXMPPAction)
  public
    constructor Create(AOwner: TXMPPActions);
  end;

  TActionIQWating = class(TXMPPActionIQ)
  private
    FExecuted: Boolean;
    FSuccess: Boolean;
  public
    constructor Create(AOwner: TXMPPActions);
    property Executed: Boolean read FExecuted;
    property Success: Boolean read FSuccess;
  end;

  TActionPresenceWating = class(TXMPPActionPresence)
  private
    FExecuted: Boolean;
    FSuccess: Boolean;
  public
    constructor Create(AOwner: TXMPPActions);
    property Executed: Boolean read FExecuted;
    property Success: Boolean read FSuccess;
  end;

  TXMPPActionIQResponse = class(TXMPPAction)
  public
    constructor Create(AOwner: TXMPPActions);
  end;

  //Конкретные классы

  TActionIQContactRename = class(TActionIQWating)
  private
    FStatus: Boolean;
  public
    constructor Create(AOwner: TXMPPActions; Item: TRosterItem);
    function Execute(Node: TGmXmlNode): Boolean; override;
    property Status: Boolean read FStatus;
  end;

  TActionIQContactAdd = class(TActionIQWating)
  private
    FStatus: Boolean;
  public
    constructor Create(AOwner: TXMPPActions; Item: TRosterItem);
    function Execute(Node: TGmXmlNode): Boolean; override;
    property Status: Boolean read FStatus;
  end;

  TActionIQSetVCard = class(TXMPPActionIQ)
  private
    FImageBin: string;
  public
    constructor Create(AOwner: TXMPPActions; VCard: TVCard);
    function Execute(Node: TGmXmlNode): Boolean; override;
  end;

  TActionIQContactDelete = class(TXMPPActionIQ)
  private
    FJID: string;
  public
    constructor Create(AOwner: TXMPPActions; JID: string);
    function Execute(Node: TGmXmlNode): Boolean; override;
  end;

  TActionIQSetBind = class(TXMPPActionIQ)
  public
    constructor Create(AOwner: TXMPPActions);
    function Execute(Node: TGmXmlNode): Boolean; override;
  end;

  TActionIQGetRoster = class(TXMPPActionIQ)
  public
    constructor Create(AOwner: TXMPPActions);
    function Execute(Node: TGmXmlNode): Boolean; override;
  end;

  TActionIQGetBookmarks = class(TXMPPActionIQ)
  public
    constructor Create(AOwner: TXMPPActions);
    function Execute(Node: TGmXmlNode): Boolean; override;
  end;

  TActionIQSetSession = class(TXMPPActionIQ)
  public
    constructor Create(AOwner: TXMPPActions);
    function Execute(Node: TGmXmlNode): Boolean; override;
  end;

  TActionPresenceSubscribe = class(TXMPPActionPresence)
  public
    function Execute(Node: TGmXmlNode): Boolean; override;
  end;

  TActionStreamError = class(TXMPPActionStreamError)
  public
    function Execute(Node: TGmXmlNode): Boolean; override;
  end;

  TActionFailure = class(TXMPPAction)
  public
    constructor Create(AOwner: TXMPPActions);
    function Execute(Node: TGmXmlNode): Boolean; override;
  end;

  TActionStreamFeatures = class(TXMPPAction)
  public
    constructor Create(AOwner: TXMPPActions);
    function Execute(Node: TGmXmlNode): Boolean; override;
  end;

  TActionChallenge = class(TXMPPAction)
  public
    constructor Create(AOwner: TXMPPActions);
    function Execute(Node: TGmXmlNode): Boolean; override;
  end;

  TActionSuccess = class(TXMPPAction)
  public
    constructor Create(AOwner: TXMPPActions);
    function Execute(Node: TGmXmlNode): Boolean; override;
  end;

  TActionMessage = class(TXMPPAction)
  public
    constructor Create(AOwner: TXMPPActions);
    function Execute(Node: TGmXmlNode): Boolean; override;
  end;

  TActionIQVersion = class(TActionIQWating)
  private
    FVersion: TJabberVersion;
  public
    constructor Create(AOwner: TXMPPActions; JID: string);
    function Execute(Node: TGmXmlNode): Boolean; override;
    property Version: TJabberVersion read FVersion;
  end;

  TActionIQConfList = class(TActionIQWating)
  private
    FList: TConfList;
  public
    constructor Create(AOwner: TXMPPActions; Server: string);
    function Execute(Node: TGmXmlNode): Boolean; override;
    property Result: TConfList read FList;
  end;

  TActionPresenceToConf = class(TActionPresenceWating)
  private
    FTo, FID, FNick: string;
    FResult: TConfPresence;
  public
    constructor Create(AOwner: TXMPPActions; ATo, ANick: string);
    function Execute(Node: TGmXmlNode): Boolean; override;
    property Result: TConfPresence read FResult;
  end;

  TActionIQVCard = class(TActionIQWating)
  private
    FVCard: TVCard;
  public
    constructor Create(AOwner: TXMPPActions; JID: string);
    function Execute(Node: TGmXmlNode): Boolean; override;
    property VCard: TVCard read FVCard;
  end;

  TActionIQResponseVersion = class(TXMPPActionIQResponse)
  public
    function Execute(Node: TGmXmlNode): Boolean; override;
  end;

  TActionIQResponseDiscoInfo = class(TXMPPActionIQResponse)
  public
    function Execute(Node: TGmXmlNode): Boolean; override;
  end;

  TActionIQResponseDiscoItems = class(TXMPPActionIQResponse)
  public
    function Execute(Node: TGmXmlNode): Boolean; override;
  end;

  TActionIQResponsePing = class(TXMPPActionIQResponse)
  public
    function Execute(Node: TGmXmlNode): Boolean; override;
  end;

  TActionIQResponseTime = class(TXMPPActionIQResponse)
  public
    function Execute(Node: TGmXmlNode): Boolean; override;
  end;

  TActionIQResponseLast = class(TXMPPActionIQResponse)
  public
    function Execute(Node: TGmXmlNode): Boolean; override;
  end;

  TActionIQRosterSet = class(TXMPPActionIQResponse)
  public
    function Execute(Node: TGmXmlNode): Boolean; override;
  end;

implementation

uses
  System.DateUtils, Xml.xmlutil;

{ TXMPPIQAction }

constructor TXMPPActionIQ.Create(AOwner: TXMPPActions);
begin
  inherited;
  Item := XMLNS_IQUERY;
end;

{ TXMPPPresenceAction }

constructor TXMPPActionPresence.Create(AOwner: TXMPPActions);
begin
  inherited;
  Item := XMLNS_ITEMPRESENCE;
end;

{ TXMPPActionStreamError }

constructor TXMPPActionStreamError.Create(AOwner: TXMPPActions);
begin
  inherited;
  Item := XMLNS_STREAMERROR;
end;

{ TXMPPActionIQResponse }

constructor TXMPPActionIQResponse.Create(AOwner: TXMPPActions);
begin
  inherited;
  Item := XMLNS_IQUERY;
end;

{ TActionIQWating }

constructor TActionIQWating.Create(AOwner: TXMPPActions);
begin
  inherited Create(AOwner);
  Timeout := 10 * 1000;
  FreeAfterTimeout := True;
  FSuccess := False;
  FExecuted := False;
end;

{ TActionPresenceWating }

constructor TActionPresenceWating.Create(AOwner: TXMPPActions);
begin
  inherited Create(AOwner);
  Timeout := 10 * 1000;
  FreeAfterTimeout := True;
  FSuccess := False;
  FExecuted := False;
end;

{ TIQActionContactRename }

constructor TActionIQContactRename.Create(AOwner: TXMPPActions; Item: TRosterItem);
begin
  inherited Create(AOwner);
  ID := Owner.Jabber.SendAddSetContact(Item);
end;

function TActionIQContactRename.Execute(Node: TGmXmlNode): Boolean;
begin
  if Node.Params.Values['id'] <> FID then
    Exit(False);
  Result := True;
  FExecuted := True;
  if (Node.Params.Values['type'] = 'result') then
  begin
    FStatus := True;
  end
  else
  begin
    FStatus := False;
  end;
end;

{ TIQActionPresenceSubscribe }

function TActionPresenceSubscribe.Execute(Node: TGmXmlNode): Boolean;
var
  From, Nick: string;
  NickNode: TGmXmlNode;
begin
  if LowerCase(Node.Params.Values['type']) <> 'subscribe' then
    Exit(False);
  Result := True;
  From := Node.Params.Values['from'];
  NickNode := Node.Children.NodeByName['nick'];
  if Assigned(NickNode) then
    Nick := NickNode.AsString
  else
    Nick := LoginFromJID(From);
  Owner.Jabber.DoGetSubscribe(From, Nick);
end;

{ TIQActionContactAdd }

constructor TActionIQContactAdd.Create(AOwner: TXMPPActions; Item: TRosterItem);
begin
  inherited Create(AOwner);
  ID := Owner.Jabber.SendAddSetContact(Item);
end;

function TActionIQContactAdd.Execute(Node: TGmXmlNode): Boolean;
begin
  if Node.Params.Values['id'] <> FID then
    Exit(False);
  Result := True;
  FExecuted := True;
  if (Node.Params.Values['type'] = 'result') then
  begin
    FStatus := True;
    ShowMessage('Контакт добавлен');
  end
  else
  begin
    FStatus := False;
    ShowMessage('Контакт НЕ добавлен');
  end;
end;

{ TIQActionContactDelete }

constructor TActionIQContactDelete.Create(AOwner: TXMPPActions; JID: string);
begin
  inherited Create(AOwner);
  FreeAfterExecute := True;
  FreeAfterTimeout := True;
  FJID := JID;
  ID := Owner.Jabber.SendDeleteContact(JID);
end;

function TActionIQContactDelete.Execute(Node: TGmXmlNode): Boolean;
begin
  if Node.Params.Values['id'] <> FID then
    Exit(False);
  Result := True;
  if (Node.Params.Values['type'] = 'result') then
  begin
    ShowMessage('Контакт удалён');
  end
  else
  begin
    ShowMessage('Контакт НЕ удалён');
  end;
end;

{ TActionStreamError }

function TActionStreamError.Execute(Node: TGmXmlNode): Boolean;
begin
  Result := True;
  Owner.Jabber.DoError(Owner.Jabber, Node.AsDisplayString);
end;

{ TActionFailure }

constructor TActionFailure.Create(AOwner: TXMPPActions);
begin
  inherited;
  Item := XMLNS_ITEMFAILURE;
end;

function TActionFailure.Execute(Node: TGmXmlNode): Boolean;
begin
  Result := True;
  Owner.Jabber.DoLoginError(Owner.Jabber, Node.AsDisplayString);
end;

{ TXMPPActionStreamFeatures }

constructor TActionStreamFeatures.Create(AOwner: TXMPPActions);
begin
  inherited;
  Item := XMLNS_STREAMFEATURES;
end;

function TActionStreamFeatures.Execute(Node: TGmXmlNode): Boolean;
var
  FeatureItem: TGmXmlNode;
  AuthType: TMechanisms;
begin
  Result := True;
  // MECHANISM
  FeatureItem := Node.Children.NodeByName['mechanisms'];
  if Assigned(FeatureItem) then
  begin
    AuthType := GetMechainsms(FeatureItem);
    if AuthType <> mecNONE then
      Owner.Jabber.SendAuthType(AuthType)
    else
      raise Exception.Create(MSG_StreamError);
  end;
  // BIND
  FeatureItem := Node.Children.NodeByName['bind'];
  if Assigned(FeatureItem) then
  begin
    if FeatureItem.Params.Values['xmlns'] = XMLNS_XMPP_BIND then
      Owner.Jabber.SendBind;
  end;
end;

{ TActionIQSetBind }

constructor TActionIQSetBind.Create(AOwner: TXMPPActions);
begin
  inherited Create(AOwner);
  ID := Owner.Jabber.SendSetBind;
end;

function TActionIQSetBind.Execute(Node: TGmXmlNode): Boolean;
begin
  if Node.Params.Values['id'] <> FID then
    Exit(False);
  Result := True;
  if Node.Params.Values['type'] = 'result' then
  begin
    Owner.Jabber.SendSession;
  end
  else
  begin
    ShowMessage('TActionIQSetBind fail');
  end;
end;

{ TActionIQSetSession }

constructor TActionIQSetSession.Create(AOwner: TXMPPActions);
begin
  inherited Create(AOwner);
  FreeAfterExecute := True;
  FreeAfterTimeout := True;
  ID := Owner.Jabber.SendSetSession;
end;

function TActionIQSetSession.Execute(Node: TGmXmlNode): Boolean;
begin
  if Node.Params.Values['id'] <> FID then
    Exit(False);
  Result := True;
  if (Node.Params.Values['type'] = 'result') then
  begin
    Owner.Jabber.DoJabberOnline(Owner.Jabber);
  end
  else
  begin
    ShowMessage('TActionIQSetSession fail');
  end;
end;

{ TActionChallenge }

constructor TActionChallenge.Create(AOwner: TXMPPActions);
begin
  inherited;
  Item := XMLNS_ITEMCHALLENGE;
end;

function TActionChallenge.Execute(Node: TGmXmlNode): Boolean;
begin
  Result := True;
  Owner.Jabber.SendSASLResponse(Node.AsString);
end;

{ TActionSuccess }

constructor TActionSuccess.Create(AOwner: TXMPPActions);
begin
  inherited;
  Item := XMLNS_ITEMSUCCESS;
end;

function TActionSuccess.Execute(Node: TGmXmlNode): Boolean;
begin
  Result := False;
  if Node.Params.Values['xmlns'] = XMLNS_XMPP_SASL then
  begin
    Result := True;
    Owner.Jabber.SendStreamStart;
  end;
end;

{ TActionIQGetRoster }

constructor TActionIQGetRoster.Create(AOwner: TXMPPActions);
begin
  inherited Create(AOwner);
  FreeAfterExecute := True;
  FreeAfterTimeout := True;
  ID := Owner.Jabber.SendGetRoster;
end;

function TActionIQGetRoster.Execute(Node: TGmXmlNode): Boolean;
begin
  if Node.Params.Values['id'] <> FID then
    Exit(False);
  Result := True;
  if (Node.Params.Values['type'] = 'result') then
  begin
    Owner.Jabber.DoGetRoster(Owner.Jabber, Node.Children.NodeByName['query']);
  end
  else
  begin
    ShowMessage('TActionIQGetRoster fail');
  end;
end;

{ TActionIQGetBookmarks }

constructor TActionIQGetBookmarks.Create(AOwner: TXMPPActions);
begin
  inherited Create(AOwner);
  FreeAfterExecute := True;
  FreeAfterTimeout := True;
  ID := Owner.Jabber.SendGetBookmarks;
end;

function TActionIQGetBookmarks.Execute(Node: TGmXmlNode): Boolean;
begin
  if Node.Params.Values['id'] <> FID then
    Exit(False);
  Result := True;
  if (Node.Params.Values['type'] = 'result') then
  begin
    Owner.Jabber.DoGetBookMarks(Owner.Jabber, Node.Children.NodeByName['query']);
  end
  else
  begin
    ShowMessage('TActionIQGetBookmarks fail');
  end;
end;

{ TActionMessage }

constructor TActionMessage.Create(AOwner: TXMPPActions);
begin
  inherited;
  Item := XMLNS_ITEMMESSAGE;
end;

function TActionMessage.Execute(Node: TGmXmlNode): Boolean;
var
  Item: TJabberMessage;
begin
  if not Owner.Jabber.RosetReceived then
    Exit(False);
  Result := True;
  //Пока пропускаем event pub
  if Node.Children.NodeExists('event') then
    Exit;

  Item.ID := Node.Params.Values['id'];
  Item.From := Node.Params.Values['from'];
  Item.ToJID := LoginFromJID(Node.Params.Values['to']);
  Item.MessageType := Node.Params.Values['type'];
  if Item.MessageType = 'error' then
  begin
    if Node.Children.NodeExists('error') then
      with Node.Children.NodeByName['error'] do
      begin
        Item.Error.Code := Params.Values['code'];
        Item.Error.ErrorType := Params.Values['type'];
        if Children.NodeExists('text') then
          Item.Error.Text := Children.NodeByName['text'].AsString;
      end;
  end;
  if Node.Children.NodeExists('body') then
    Item.Body := FromEscaping(Node.Children.NodeByName['body'].AsString);
  if Node.Children.NodeExists('thread') then
    Item.Thread := Node.Children.NodeByName['thread'].AsString;
  if Node.Children.NodeExists('delay') then
  begin
    Item.Delay := True;
    Item.DelayDate := ISO8601ToDate(Node.Children.NodeByName['delay'].Params.Values['stamp'], False);
  end;
  if Node.Children.NodeExists('subject') then
    Item.Subject := Node.Children.NodeByName['subject'].AsString;

  if Node.Children.NodeExists('displayed') then
  begin
    Item.ID := Node.Children.NodeByName['displayed'].Params.Values['id'];
    Item.Displayed := True;
  end
  else
    Item.Displayed := False;

  if Node.Children.NodeExists('attention') then
  begin
    Item.Attention := True;
  end
  else
    Item.Attention := False;

  if Node.Children.NodeExists('received') then
  begin
    Item.ID := Node.Children.NodeByName['received'].Params.Values['id'];
    Item.Received := True;
  end
  else
    Item.Received := False;

  if Node.Children.NodeWithParamExists('x', 'xmlns', XMLNS_XOOB) then
    with Node.Children.NodeWithParam('x', 'xmlns', XMLNS_XOOB) do
    begin
      if Children.NodeExists('url') then
        Item.XMLNS_XOOB.URL := Children.NodeByName['url'].AsString;
    end;

  Owner.Jabber.DoGetMessage(Owner.Jabber, Item);
end;

{ TActionIQVersion }

constructor TActionIQVersion.Create(AOwner: TXMPPActions; JID: string);
begin
  inherited Create(AOwner);
  ID := Owner.Jabber.SendGetVersion(JID);
end;

function TActionIQVersion.Execute(Node: TGmXmlNode): Boolean;
begin
  if Node.Params.Values['id'] <> FID then
    Exit(False);
  Result := True;
  FExecuted := True;
  if (Node.Params.Values['type'] = 'result') then
  begin
    FSuccess := True;
    FVersion.Version := Node.Children.NodeByName['query'].Params.Values['version'];
    FVersion.OS := Node.Children.NodeByName['query'].Params.Values['os'];
    FVersion.Name := Node.Children.NodeByName['query'].Params.Values['name'];
  end
  else
  begin
    FSuccess := False;
    FVersion.Error := Node.Children.NodeByName['error'].Params.Values['code'];
  end;
end;

{ TActionIQResponseVersion }

function TActionIQResponseVersion.Execute(Node: TGmXmlNode): Boolean;
begin
  if not Assigned(Node.Children.NodeByName['query']) then
    Exit(False);
  if Node.Children.NodeByName['query'].Params.Values['xmlns'] <> XMLNS_VERSION then
    Exit(False);
  Result := True;
  Owner.Jabber.SendVersion(Node.Params.Values['from'], Node.Params.Values['id']);
end;

{ TActionIQResponsePing }

function TActionIQResponsePing.Execute(Node: TGmXmlNode): Boolean;
begin
  if not Assigned(Node.Children.NodeByName['query']) then
    Exit(False);
  if Node.Children.NodeByName['query'].Params.Values['xmlns'] <> XMLNS_PING then
    Exit(False);
  Result := True;
  Owner.Jabber.SendPing(Node.Params.Values['from'], Node.Params.Values['id']);
end;

{ TActionIQResponseTime }

function TActionIQResponseTime.Execute(Node: TGmXmlNode): Boolean;
begin
  Result := False;
  if Assigned(Node.Children.NodeByName['query']) then
  begin
    if Node.Children.NodeByName['query'].Params.Values['xmlns'] = XMLNS_TIME then
    begin
      Result := True;
      Owner.Jabber.SendTime(Node.Params.Values['from'], Node.Params.Values['id']);
    end
  end
  else if Assigned(Node.Children.NodeByName['time']) then
  begin
    if Node.Children.NodeByName['time'].Params.Values['xmlns'] = XMLNS_URN_TIME then
    begin
      Result := True;
      Owner.Jabber.SendTimeURN(Node.Params.Values['from'], Node.Params.Values['id']);
    end
  end;
end;

{ TActionIQRosterSet }

function TActionIQRosterSet.Execute(Node: TGmXmlNode): Boolean;
var
  Item: TRosterItem;
  ItemNode: TGmXmlNode;
  i: Integer;
begin
  if Node.Params.Values['type'] <> 'set' then
    Exit(False);
  if not Assigned(Node.Children.NodeByName['query']) then
    Exit(False);
  if Node.Children.NodeByName['query'].Params.Values['xmlns'] <> XMLNS_ROSTER then
    Exit(False);
  ItemNode := Node.Children.NodeByName['query'].Children.NodeByName['item'];
  if not Assigned(ItemNode) then
    Exit(False);
  Result := True;
  Owner.Jabber.SendIQResult(Node.Params.Values['id']);
  Item := TRosterItem.Create;
  Item.JID := ItemNode.Params.Values['jid'];
  Item.Name := ItemNode.Params.Values['name'];
  Item.Subscription := ItemNode.Params.Values['subscription'];
  for i := 0 to ItemNode.Children.Count - 1 do
  begin
    Item.Groups.Add(ItemNode.Children.Node[i].AsString);
  end;
  Owner.Jabber.DoRosterSet(Owner.Jabber, Item);
  if Assigned(Item) then
    Item.Free;
end;

{ TActionIQVCard }

constructor TActionIQVCard.Create(AOwner: TXMPPActions; JID: string);
begin
  inherited Create(AOwner);
  ID := Owner.Jabber.SendGetVCard(JID);
end;

function TActionIQVCard.Execute(Node: TGmXmlNode): Boolean;
var
  QueryNode, ItemNode: TGmXmlNode;
  i: Integer;

  function GetNodeText(Root: TGmXmlNode; NodeName: string): string;
  var
    Node: TGmXmlNode;
  begin
    Node := Root.Children.NodeByName[NodeName];
    if Assigned(Node) then
      Result := Node.AsString
    else
      Result := '';
  end;

begin
  if Node.Params.Values['id'] <> FID then
    Exit(False);
  Result := True;
  FExecuted := True;
  if (Node.Params.Values['type'] = 'result') then
  begin
    FSuccess := True;
    QueryNode := Node.Children.NodeByName['vCard'];
    if Assigned(QueryNode) then
    begin
      FVCard.FullName := GetNodeText(QueryNode, 'FN');

      ItemNode := QueryNode.Children.NodeByName['N'];
      if Assigned(ItemNode) then
      begin
        FVCard.Name.FirstName := GetNodeText(ItemNode, 'GIVEN');
        FVCard.Name.MiddleName := GetNodeText(ItemNode, 'MIDDLE');
        FVCard.Name.LastName := GetNodeText(ItemNode, 'FAMILY');
      end;

      FVCard.NickName := GetNodeText(QueryNode, 'NICKNAME');
      FVCard.BirthDay := XmlToDate(GetNodeText(QueryNode, 'BDAY'));
      FVCard.URL := GetNodeText(QueryNode, 'URL');
      FVCard.Title := GetNodeText(QueryNode, 'TITLE');
      FVCard.Role := GetNodeText(QueryNode, 'ROLE');
      FVCard.Desc := GetNodeText(QueryNode, 'DESC');

      for i := 0 to QueryNode.Children.Count - 1 do
      begin
        ItemNode := QueryNode.Children.Node[i];
        if ItemNode.Name = 'ADR' then
        begin
          SetLength(FVCard.Address, Length(FVCard.Address) + 1);
          with FVCard.Address[Length(FVCard.Address) - 1] do
          begin
            if ItemNode.Children.NodeExists('HOME') then
              Include(Flags, afHome);
            if ItemNode.Children.NodeExists('WORK') then
              Include(Flags, afWork);
            if ItemNode.Children.NodeExists('POSTAL') then
              Include(Flags, afPostal);
            if ItemNode.Children.NodeExists('PARCEL') then
              Include(Flags, afParcel);
            if ItemNode.Children.NodeExists('DOM') then
              Include(Flags, afDom);
            if ItemNode.Children.NodeExists('INTL') then
              Include(Flags, afIntl);
            if ItemNode.Children.NodeExists('PREF') then
              Include(Flags, afPref);
            ExtAdd := GetNodeText(ItemNode, 'EXTADD');
            Street := GetNodeText(ItemNode, 'STREET');
            Locality := GetNodeText(ItemNode, 'LOCALITY');
            Region := GetNodeText(ItemNode, 'REGION');
            PCode := GetNodeText(ItemNode, 'PCODE');
            Country := GetNodeText(ItemNode, 'CTRY');
          end;
        end;
      end;

      for i := 0 to QueryNode.Children.Count - 1 do
      begin
        ItemNode := QueryNode.Children.Node[i];
        if ItemNode.Name = 'TEL' then
        begin
          SetLength(FVCard.Tel, Length(FVCard.Tel) + 1);
          with FVCard.Tel[Length(FVCard.Tel) - 1] do
          begin
            if ItemNode.Children.NodeExists('HOME') then
              Include(Flags, tfHome);
            if ItemNode.Children.NodeExists('VOICE') then
              Include(Flags, tfVoice);
            if ItemNode.Children.NodeExists('WORK') then
              Include(Flags, tfWork);
            if ItemNode.Children.NodeExists('FAX') then
              Include(Flags, tfFAX);
            if ItemNode.Children.NodeExists('PAGER') then
              Include(Flags, tfPager);
            if ItemNode.Children.NodeExists('MSG') then
              Include(Flags, tfMSG);
            if ItemNode.Children.NodeExists('CELL') then
              Include(Flags, tfCell);
            if ItemNode.Children.NodeExists('VIDEO') then
              Include(Flags, tfVoice);
            if ItemNode.Children.NodeExists('BBS') then
              Include(Flags, tfBBS);
            if ItemNode.Children.NodeExists('MODEM') then
              Include(Flags, tfModem);
            if ItemNode.Children.NodeExists('ISDN') then
              Include(Flags, tfISDN);
            if ItemNode.Children.NodeExists('ISDN') then
              Include(Flags, tfISDN);
            if ItemNode.Children.NodeExists('PCS') then
              Include(Flags, tfPCS);
            if ItemNode.Children.NodeExists('PREF') then
              Include(Flags, tfPREF);
            Number := GetNodeText(ItemNode, 'NUMBER');
          end;
        end;
      end;

      for i := 0 to QueryNode.Children.Count - 1 do
      begin
        ItemNode := QueryNode.Children.Node[i];
        if ItemNode.Name = 'EMAIL' then
        begin
          SetLength(FVCard.EMail, Length(FVCard.EMail) + 1);
          with FVCard.EMail[Length(FVCard.EMail) - 1] do
          begin
            if ItemNode.Children.NodeExists('HOME') then
              Include(Flags, efHome);
            if ItemNode.Children.NodeExists('WORK') then
              Include(Flags, efWork);
            if ItemNode.Children.NodeExists('INTERNET') then
              Include(Flags, efInternet);
            if ItemNode.Children.NodeExists('PREF') then
              Include(Flags, efPREF);
            if ItemNode.Children.NodeExists('X400') then
              Include(Flags, efX400);
            UserId := GetNodeText(ItemNode, 'USERID');
          end;
        end;
      end;

      ItemNode := QueryNode.Children.NodeByName['ORG'];
      if Assigned(ItemNode) then
      begin
        FVCard.Organisation.Name := GetNodeText(ItemNode, 'ORGNAME');
        FVCard.Organisation.OrgUnit := GetNodeText(ItemNode, 'ORGUNIT');
      end;

      ItemNode := QueryNode.Children.NodeByName['PHOTO'];
      if Assigned(ItemNode) then
      begin
        FVCard.Photo.PhotoType := GetNodeText(ItemNode, 'TYPE');
        FVCard.Photo.BinVal := GetNodeText(ItemNode, 'BINVAL');
      end;
    end;
  end
  else
  begin
    FSuccess := False;
    //FVCard.Error := Node.Children.NodeByName['error'].Params.Values['code'];
  end;
end;


{ TActionIQSetVCard }

constructor TActionIQSetVCard.Create(AOwner: TXMPPActions; VCard: TVCard);
begin
  inherited Create(AOwner);
  FreeAfterExecute := True;
  FreeAfterTimeout := True;
  FImageBin := VCard.Photo.BinVal;
  ID := Owner.Jabber.SendSetVCard(VCard);
end;

function TActionIQSetVCard.Execute(Node: TGmXmlNode): Boolean;
begin
  if Node.Params.Values['id'] <> FID then
    Exit(False);
  Result := True;
  if (Node.Params.Values['type'] = 'result') then
  begin
    Owner.Jabber.UpdateImageHash(FImageBin);
    Owner.Jabber.SetPresence;
    ShowMessage('Данные обновлены');
  end
  else
  begin
    ShowMessage('Данные НЕ обновлены');
  end;
end;

{ TActionIQResponseDiscoInfo }

function TActionIQResponseDiscoInfo.Execute(Node: TGmXmlNode): Boolean;
begin
  if Node.Params.Values['type'] <> 'get' then
    Exit(False);
  if not Assigned(Node.Children.NodeByName['query']) then
    Exit(False);
  if Node.Children.NodeByName['query'].Params.Values['xmlns'] <> XMLNS_DISCOINFO then
    Exit(False);
  Result := True;
  Owner.Jabber.SendDiscoInfo(Node.Params.Values['from'], Node.Params.Values['id']);
end;

{ TActionIQResponseLast }

function TActionIQResponseLast.Execute(Node: TGmXmlNode): Boolean;
begin
  if not Assigned(Node.Children.NodeByName['query']) then
    Exit(False);
  if Node.Children.NodeByName['query'].Params.Values['xmlns'] <> XMLNS_LAST then
    Exit(False);
  Result := True;
  Owner.Jabber.SendLast(Node.Params.Values['from'], Node.Params.Values['id']);
end;

{ TActionIQResponseDiscoItems }

function TActionIQResponseDiscoItems.Execute(Node: TGmXmlNode): Boolean;
begin
  if Node.Params.Values['type'] <> 'get' then
    Exit(False);
  if not Assigned(Node.Children.NodeByName['query']) then
    Exit(False);
  if Node.Children.NodeByName['query'].Params.Values['xmlns'] <> XMLNS_DISCOITEMS then
    Exit(False);
  Result := True;
  Owner.Jabber.SendDiscoItems(Node.Params.Values['from'], Node.Params.Values['id']);
end;

{ TActionPresenceToConf }

constructor TActionPresenceToConf.Create(AOwner: TXMPPActions; ATo, ANick: string);
begin
  inherited Create(AOwner);
  FTo := ATo;
  FNick := ANick;
  FResult.Error := False;
  FID := Owner.Jabber.SendEnterChat(FTo, FNick, '');
end;

function TActionPresenceToConf.Execute(Node: TGmXmlNode): Boolean;
begin
  if Node.Params.Values['id'] <> FID then
    Exit(False);
  Result := False;
  FExecuted := True;
  if Node.Params.Values['type'] = 'error' then
  begin
    FResult.Error := True;
    if Node.Children.NodeExists('error') then
      with Node.Children.NodeByName['error'] do
      begin
        FResult.ErrorData.Code := Params.Values['code'];
        FResult.ErrorData.ErrorType := Params.Values['type'];
        if Children.NodeExists('conflict') then
          with Children.NodeByName['conflict'] do
            FResult.ErrorData.Conflict := Params.Values['xmlns'];
        if Children.NodeExists('text') then
          with Children.NodeByName['text'] do
            FResult.ErrorData.Text := AsString;
      end;
    Exit;
  end;
end;

{ TActionIQConfList }

constructor TActionIQConfList.Create(AOwner: TXMPPActions; Server: string);
begin
  inherited Create(AOwner);
  FList := TConfList.Create;
  ID := Owner.Jabber.SendGetDiscoInfo(Server);
end;

function TActionIQConfList.Execute(Node: TGmXmlNode): Boolean;
var
  i: Integer;
  Item: TConfItem;
  Query: TGmXmlNode;
begin
  if Node.Params.Values['id'] <> FID then
    Exit(False);
  Result := True;
  FExecuted := True;
  if Node.Children.NodeExists('query') then
  begin
    Query := Node.Children.NodeByName['query'];
    for i := 0 to Query.Children.Count - 1 do
      with Query.Children[i] do
        if Query.Children[i].Name = 'item' then
        begin
          Item.JID := Params.Values['jid'];
          Item.Name := Params.Values['name'];
          FList.Add(Item);
        end;
    if Query.Children.NodeExists('set') then
      with Query.Children.NodeByName['set'] do
      begin
        if Children.NodeExists('last') then
          FList.ConfLast := Children.NodeByName['last'].AsString;
        if Children.NodeExists('count') then
          FList.ConfCount := Children.NodeByName['count'].AsInteger;
      end;
  end;
end;

end.

