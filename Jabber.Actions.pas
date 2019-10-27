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

  //Конкретные классы

  TActionIQContactRename = class(TXMPPActionIQ)
  private
    FJID, FNewNick, FGroup: string;
    FOnResult: TOnActionResult;
  public
    constructor Create(AOwner: TXMPPActions; JID, NewNick, Group: string);
    function Execute(Node: TGmXmlNode): Boolean; override;
    property OnResult: TOnActionResult read FOnResult write FOnResult;
  end;

  TActionIQContactAdd = class(TXMPPActionIQ)
  private
    FJID, FNick, FGroup: string;
    FOnResult: TOnActionResult;
  public
    constructor Create(AOwner: TXMPPActions; JID, Nick, Group: string);
    function Execute(Node: TGmXmlNode): Boolean; override;
    property OnResult: TOnActionResult read FOnResult write FOnResult;
  end;

  TActionIQContactDelete = class(TXMPPActionIQ)
  private
    FJID: string;
    FOnResult: TOnActionResult;
  public
    constructor Create(AOwner: TXMPPActions; JID: string);
    function Execute(Node: TGmXmlNode): Boolean; override;
  end;

  TActionIQSetBind = class(TXMPPActionIQ)
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

  TXMPPActionStreamFeatures = class(TXMPPAction)
  public
    constructor Create(AOwner: TXMPPActions);
    function Execute(Node: TGmXmlNode): Boolean; override;
  end;

implementation

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

{ TIQActionContactRename }

constructor TActionIQContactRename.Create(AOwner: TXMPPActions; JID, NewNick, Group: string);
begin
  inherited Create(AOwner);
  Owner := AOwner;
  FreeAfterExecute := True;
  FJID := JID;
  FNewNick := NewNick;
  FGroup := Group;
  ID := Owner.Jabber.SendAddSetContact(JID, NewNick, Group);
end;

function TActionIQContactRename.Execute(Node: TGmXmlNode): Boolean;
begin
  if Node.Params.Values['id'] <> FID then
    Exit(False);
  Result := True;
  if (Node.Params.Values['type'] = 'result') then
  begin
    if Assigned(FOnResult) then
      FOnResult(Self, True);
    ShowMessage('TIQActionContactRename.Execute ok');
  end
  else
  begin
    if Assigned(FOnResult) then
      FOnResult(Self, False);
    ShowMessage('TIQActionContactRename.Execute fail');
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
  From := Node.Params.Values['from'];
  NickNode := Node.Children.NodeByName['nick'];
  if Assigned(NickNode) then
    Nick := NickNode.AsString
  else
    Nick := LoginFromJID(From);
  Owner.Jabber._OnSubscribe(From, Nick);
  Result := True;
end;

{ TIQActionContactAdd }

constructor TActionIQContactAdd.Create(AOwner: TXMPPActions; JID, Nick, Group: string);
begin
  inherited Create(AOwner);
  Owner := AOwner;
  FreeAfterExecute := True;
  FJID := JID;
  FNick := Nick;
  FGroup := Group;
  ID := Owner.Jabber.SendAddSetContact(JID, Nick, Group);
end;

function TActionIQContactAdd.Execute(Node: TGmXmlNode): Boolean;
begin
  if Node.Params.Values['id'] <> FID then
    Exit(False);
  Result := True;
  if (Node.Params.Values['type'] = 'result') then
  begin
    if Assigned(FOnResult) then
      FOnResult(Self, True);
    ShowMessage('Контакт добавлен');
  end
  else
  begin
    if Assigned(FOnResult) then
      FOnResult(Self, False);
    ShowMessage('Контакт НЕ добавлен');
  end;
end;

{ TIQActionContactDelete }

constructor TActionIQContactDelete.Create(AOwner: TXMPPActions; JID: string);
begin
  inherited Create(AOwner);
  Owner := AOwner;
  FreeAfterExecute := True;
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
    if Assigned(FOnResult) then
      FOnResult(Self, True);
    ShowMessage('Контакт удалён');
  end
  else
  begin
    if Assigned(FOnResult) then
      FOnResult(Self, False);
    ShowMessage('Контакт НЕ удалён');
  end;
end;

{ TActionStreamError }

function TActionStreamError.Execute(Node: TGmXmlNode): Boolean;
begin
  Owner.Jabber._OnError(Self, Node.AsDisplayString);
  Result := True;
end;

{ TActionFailure }

constructor TActionFailure.Create(AOwner: TXMPPActions);
begin
  inherited;
  Item := XMLNS_ITEMFAILURE;
end;

function TActionFailure.Execute(Node: TGmXmlNode): Boolean;
begin
  Owner.Jabber._OnLoginError(Self, Node.AsDisplayString);
  Result := True;
end;

{ TXMPPActionStreamFeatures }

constructor TXMPPActionStreamFeatures.Create(AOwner: TXMPPActions);
begin
  inherited;
  Item := XMLNS_STREAMFEATURES;
end;

function TXMPPActionStreamFeatures.Execute(Node: TGmXmlNode): Boolean;
var
  FeatureItem: TGmXmlNode;
  AuthType: TMechanisms;
begin
  // Секция механизм аутентификации MECHANISM
  FeatureItem := Node.Children.NodeByName['mechanisms'];
  if Assigned(FeatureItem) then
  begin
    AuthType := GetMechainsms(FeatureItem);
    // Отправляем на сервер механизм аутентификации
    if AuthType <> mecNONE then
      Owner.Jabber.SendAuthType(AuthType)
    else
      raise Exception.Create(MSG_StreamError);
  end;
  // Если секция BIND
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
  Owner := AOwner;
  FreeAfterExecute := False;
  ID := Owner.Jabber.SendSetBind;
end;

function TActionIQSetBind.Execute(Node: TGmXmlNode): Boolean;
begin
  if Node.Params.Values['id'] <> FID then
    Exit(False);
  Owner.Jabber.SendSession;
  if Node.Params.Values['type'] = 'result' then
  begin
    Owner.Jabber._OnJabberOnline(Self);
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
  Owner := AOwner;
  FreeAfterExecute := True;
  ID := Owner.Jabber.SendSetSession;
end;

function TActionIQSetSession.Execute(Node: TGmXmlNode): Boolean;
begin
  if Node.Params.Values['id'] <> FID then
    Exit(False);
  Result := True;
  if (Node.Params.Values['type'] = 'result') then
  begin
    ShowMessage('TActionIQSetSession ok');
  end
  else
  begin
    ShowMessage('TActionIQSetSession fail');
  end;
end;

end.

