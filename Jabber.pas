unit Jabber;

interface

uses
  System.SysUtils, System.Classes, IdHashMessageDigest, IdCoderMime, Vcl.Dialogs,
  Vcl.Controls, Jabber.Types, GmXML, Winapi.Windows, IdGlobal, System.StrUtils,
  System.Generics.Collections, System.Win.ScktComp, HGM.Controls.VirtualTable;

type
  TJabberClient = class;

  TXMPPItem = record
    Data: string;
    Date: TDateTime;
  end;

  TXMPPItems = class(TTableData<TXMPPItem>)
  end;

  TXMPPActions = class;

  TXMPPAction = class abstract
  private
    FOwner: TXMPPActions;
    FTimeCreate: Cardinal;
    FItem: string;
    FFreeAfterExecute: Boolean;
    FFreeAfterTimeout: Boolean;
    FTimeout: Cardinal;
    FIsTimeout: Boolean;
    procedure SetOwner(const Value: TXMPPActions);
    procedure SetFreeAfterExecute(const Value: Boolean);
    procedure SetItem(const Value: string);
    procedure SetFreeAfterTimeout(const Value: Boolean);
    procedure SetTimeout(const Value: Cardinal);
  public
    function Execute(Node: TGmXmlNode): Boolean; virtual; abstract;
    constructor Create(AOwner: TXMPPActions); virtual;
    property Item: string read FItem write SetItem;
    property Owner: TXMPPActions read FOwner write SetOwner;
    property FreeAfterExecute: Boolean read FFreeAfterExecute write SetFreeAfterExecute;
    property FreeAfterTimeout: Boolean read FFreeAfterTimeout write SetFreeAfterTimeout;
    property Timeout: Cardinal read FTimeout write SetTimeout;
    property TimeCreate: Cardinal read FTimeCreate;
    property IsTimeout: Boolean read FIsTimeout write FIsTimeout;
  end;

  TXMPPActions = class(TList<TXMPPAction>)
  private
    FJabber: TJabberClient;
    procedure SetJabber(const Value: TJabberClient);
  public
    constructor Create(Client: TJabberClient);
    function Execute(Node: TGmXmlNode): Boolean;
    function Add(Value: TXMPPAction): Integer;
    procedure Delete(Index: Integer); overload;
    procedure Delete(Action: TXMPPAction); overload;
    procedure CheckTimouts;
    procedure Clear;
    property Jabber: TJabberClient read FJabber write SetJabber;
  end;

  TJabberClient = class(TComponent)
  private
    FConnected: Boolean;
    FInReceiveProcess: Integer;
    FRosetReceived: Boolean;
    FImageVCardSHA: string;
    FJabberOnLine: Boolean;
    FJabberPort: Word;
    FOnConnect: TOnConnect;
    FOnConnectError: TOnConnectError;
    FOnConnecting: TOnConnect;
    FOnDisconnect: TOnDisconnect;
    FOnError: TOnError;
    FOnGetBookMarks: TOnGetBookMarks;
    FOnGetRoster: TOnGetRoster;
    FOnIQ: TOnIQ;
    FLoginError: Boolean;
    FOnJabberOnline: TOnJabberOnline;
    FOnLoginError: TOnLoginEror;
    FOnMessage: TOnMessage;
    FOnPresence: TOnPresence;
    FOnReceiveData: TOnReceiveData;
    FOnSendData: TOnSendData;
    FOnSubscribe: TOnSubscribe;
    FPriority: Integer;
    FResource: string;
    FSocket: TClientSocket;
    FUserName: string;
    FUserNick: string;
    FUserServer: string;
    FUserStatus: TShowType;
    FUserStatusText: string;
    FWaitSetPresence: Boolean;
    FXMPPActions: TXMPPActions;
    FJabberClientName: string;
    FJabberClientVersion: string;
    FOnRosterSet: TOnRosterSet;
    FVCard: TVCard;
    FOnWorkState: TOnWorkState;
    FInParse: Boolean;
    FAuthHash: string;
    function GetJID: string;
    procedure ParseReceive(Text: string);
    procedure SetJID(const Value: string);
    procedure SetPriority(const Value: Integer);
    procedure SetUserNick(const Value: string);
    procedure SetUserStatus(const Value: TShowType);
    procedure SetUserStatusText(const Value: string);
    procedure _OnConnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure _OnConnectError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
    procedure _OnDisconnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure _OnReceive(Sender: TObject; Socket: TCustomWinSocket);
    procedure _OnSend(Sender: TObject; StrData: string);
    procedure _OnSendError(Sender: TObject; Socket: TCustomWinSocket);
    procedure SetJabberClientName(const Value: string);
    procedure SetJabberClientVersion(const Value: string);
    procedure SetInReceiveProcess(const Value: Boolean);
    function GetReceiveProcess: Boolean;
    function ClientName: string;
    procedure SetAuthHash(const Value: string);
    property InProcess: Boolean read GetReceiveProcess write SetInReceiveProcess;
  protected
    XMLStr: string;
    FXMPPItems: TXMPPItems;
    function GetUniqueID: string;
    function GetSASLResponse(AStr: string): string;
  public
    class function GetUniq: string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetVersion(AJID: string): TJabberVersion;
    function GetVCard(AJID: string): TVCard;
    function ConferenceEnter(AConf, ANick: string): TConfPresence;
    function GetListOfConference(Server: string): TConfList;
    function CheckAccount: Boolean;
    function DeleteBadSymbols(Value: string): string;
    function SendAddSetContact(Item: TRosterItem): string;
    function SendSetVCard(Card: TVCard): string;
    function SendAuthRemove(AJID: string): string;
    function SendDeleteContact(AJID: string): string;
    function SendGetBookmarks: string;
    function SendGetVCard(AJID: string): string;
    function SendGetDiscoInfo(AServer: string): string;
    function SendGetVersion(AJID: string): string;
    function SendSetBind: string;
    function SendEnterChat(AConf, ANick, APassword: string): string;
    function SendGetRoster: string;
    function SendInvite(AJID, AConf: string): string;
    function SendSetSession: string;
    function StrForParsing(var Value: string): string;
    function AddContact(Item: TRosterItem): Boolean; overload;
    function AddContact(AJID, ANick: string): Boolean; overload;
    procedure Connect;
    procedure DeleteContact(AJID: string);
    procedure Disconnect;
    procedure DoError(Sender: TObject; Error: string);
    procedure DoGetBookMarks(Sender: TObject; QueryNode: TGmXmlNode);
    procedure DoGetIQ(Sender: TObject; QueryNode: TGmXmlNode);
    procedure DoGetMessage(Sender: TObject; Item: TJabberMessage);
    procedure DoGetPresence(Sender: TObject; QueryNode: TGmXmlNode);
    procedure DoGetRoster(Sender: TObject; QueryNode: TGmXmlNode);
    procedure DoGetSubscribe(From, Nick: string);
    procedure DoJabberOnline(Sender: TObject);
    procedure DoLoginError(Sender: TObject; Error: string);
    procedure DoRosterSet(Sender: TObject; Item: TRosterItem);
    procedure EndSetPresence;
    procedure GetBookMarks;
    procedure GetRoster;
    procedure SetVCard(Card: TVCard);
    function RenameContact(Item: TRosterItem): Boolean;
    procedure SendAuthRequest(AJID: string);
    procedure SendAuthType(AuthType: TMechanisms);
    procedure SendBind;
    procedure SendData(Str: string);
    function SendMessage(AJID, AMessage: string; MessageType: TMessageType): string;
    procedure SendIQResult(ID: string);
    procedure SendAttention(AJID: string; AMessage: string = '');
    function SendPresence(Target: string = ''): string; overload;
    procedure SendPresenceUnavailable(Target: string); overload;
    procedure SendPing(AJID, ID: string);
    procedure SendLast(AJID, ID: string);
    procedure SendReadMessage(AJID, MessageID: string);
    procedure SendResponse(XMLNX, ResponseValue: string);
    procedure SendSASLResponse(ChallengeValue: string);
    procedure SendSession;
    procedure SendStreamStart;
    procedure SendSubscribeAccept(AJID: string);
    procedure SendSubscribeCancel(AJID: string);
    procedure SendTime(AJID, ID: string);
    procedure SendTimeURN(AJID, ID: string);
    procedure SendUnsubscribe(AJID: string);
    procedure SendVersion(AJID, ID: string);
    procedure SendDiscoInfo(AJID, ID: string);
    procedure SendDiscoItems(AJID, ID: string);
    procedure SetPresence;
    procedure StartSetPresence;
    procedure UpdateImageHash(BinVal: string);
    property Actions: TXMPPActions read FXMPPActions;
    property Connected: Boolean read FConnected;
    property Online: Boolean read FJabberOnLine;
    property VCard: TVCard read FVCard;
    property PhotoHash: string read FImageVCardSHA write FImageVCardSHA;
    property XMPPItems: TXMPPItems read FXMPPItems;
    property RosetReceived: Boolean read FRosetReceived;
  published
    property JabberPort: Word read FJabberPort write FJabberPort;
    property JID: string read GetJID write SetJID;
    property OnConnect: TOnConnect read FOnConnect write FOnConnect;
    property OnConnectError: TOnConnectError read FOnConnectError write FOnConnectError;
    property OnConnecting: TOnConnect read FOnConnecting write FOnConnecting;
    property OnDisconnect: TOnDisconnect read FOnDisconnect write FOnDisconnect;
    property OnError: TOnError read FOnError write FOnError;
    property OnGetBookMarks: TOnGetBookMarks read FOnGetBookMarks write FOnGetBookMarks;
    property OnGetRoster: TOnGetRoster read FOnGetRoster write FOnGetRoster;
    property OnIQ: TOnIQ read FOnIQ write FOnIQ;
    property OnJabberOnline: TOnJabberOnline read FOnJabberOnline write FOnJabberOnline;
    property OnLoginError: TOnLoginEror read FOnLoginError write FOnLoginError;
    property OnMessage: TOnMessage read FOnMessage write FOnMessage;
    property OnPresence: TOnPresence read FOnPresence write FOnPresence;
    property OnReceiveData: TOnReceiveData read FOnReceiveData write FOnReceiveData;
    property OnRosterSet: TOnRosterSet read FOnRosterSet write FOnRosterSet;
    property OnSendData: TOnSendData read FOnSendData write FOnSendData;
    property OnSubscribe: TOnSubscribe read FOnSubscribe write FOnSubscribe;
    property OnWorkState: TOnWorkState read FOnWorkState write FOnWorkState;
    property AuthHash: string read FAuthHash write SetAuthHash;
    property Priority: Integer read FPriority write SetPriority;
    property Resource: string read FResource write FResource;
    property UserName: string read FUserName write FUserName;
    property UserNick: string read FUserNick write SetUserNick;
    property UserServer: string read FUserServer write FUserServer;
    property UserStatus: TShowType read FUserStatus write SetUserStatus;
    property UserStatusText: string read FUserStatusText write SetUserStatusText;
    property JabberClientName: string read FJabberClientName write SetJabberClientName;
    property JabberClientVersion: string read FJabberClientVersion write SetJabberClientVersion;
  end;

function LoginFromJID(JID: string): string;

function NickFromJID(JID: string): string;

function NickFromConfJID(JID: string): string;

function GetMechainsms(XMLItem: TGmXmlNode): TMechanisms;

function GetAuthHash(UserName, Server, Password: string): string;

implementation

uses
  Math, Jabber.Actions, IM.Main, Vcl.Forms, IM.Tool.Console, System.DateUtils,
  System.TimeSpan, CryptUnit;

function GetAuthHash(UserName, Server, Password: string): string;
var
  Hasher: TIdHashMessageDigest5;
begin
  Hasher := TIdHashMessageDigest5.Create;
  Result := Hasher.HashStringAsHex(UserName + ':' + Server + ':' + Password);
  Hasher.Free;
end;

function LoginFromJID(JID: string): string;
begin
  if Pos('/', JID) > 1 then
    Result := Copy(JID, 1, Pos('/', JID) - 1)
  else
    Result := JID;
end;

function NickFromJID(JID: string): string;
begin
  if Pos('@', JID) > 1 then
    Result := Copy(JID, 1, Pos('@', JID) - 1)
  else
    Result := JID;
end;

function NickFromConfJID(JID: string): string;
begin
  if Pos('/', JID) > 1 then
    Result := Copy(JID, Pos('/', JID) + 1, Length(JID))
  else
    Result := JID;
end;

function GetMechainsms(XMLItem: TGmXmlNode): TMechanisms;
var
  Child: TGmXmlNode;
  i: Integer;
begin
  Result := mecNONE;
  if XMLItem.Params.Values['xmlns'] <> XMLNS_XMPP_SASL then
    Exit;
  if Assigned(XMLItem) then
  begin
    for i := 0 to XMLItem.Children.Count - 1 do
    begin
      Child := XMLItem.Children.Node[i];
      if Child.Name = 'mechanism' then
      begin
        if Child.AsString = 'DIGEST-MD5' then
        begin
          Exit(mecDIGEST_MD5);
        end;
        if Child.AsString = 'PLAIN' then
        begin
          Result := mecPLAIN;
        end;
      end;
    end;
  end;
end;

{ TJabberClient }

procedure TJabberClient.DeleteContact(AJID: string);
begin
  FXMPPActions.Add(TActionIQContactDelete.Create(FXMPPActions, AJID));
end;

function TJabberClient.CheckAccount: Boolean;
begin
  Result := (UserServer <> '') and (UserName <> '') and (JabberPort > 0) and (AuthHash <> '') and (not FLoginError);
end;

procedure TJabberClient.Connect;
begin
  if not Connected then
  begin
    InProcess := True;
    FSocket.Host := UserServer;
    FSocket.Port := JabberPort;
    FSocket.OnError := _OnConnectError;
    FSocket.OnConnect := _OnConnect;
    FSocket.OnDisconnect := _OnDisconnect;
    FSocket.OnRead := _OnReceive;
    FSocket.Open;
    if Assigned(FOnConnecting) then
      FOnConnecting(Self);
  end
  else
    SetPresence;
end;

constructor TJabberClient.Create(AOwner: TComponent);
begin
  inherited;
  FSocket := TClientSocket.Create(nil);
  FXMPPItems := TXMPPItems.Create;
  FLoginError := False;
  FInReceiveProcess := 0;
  FInParse := False;
  FRosetReceived := False;
  FWaitSetPresence := False;
  FUserServer := 'jabber.ru';
  FUserNick := '';
  FJabberPort := 5222;
  FResource := 'jabbrel';
  FPriority := 1;
  FJabberClientName := 'IMJabber';
  FJabberClientVersion := '1.0';
  FXMPPActions := TXMPPActions.Create(Self);

  //Обработка при получении сообщений
  FXMPPActions.Add(TActionMessage.Create(FXMPPActions));
  //Обработка при получении первичных данных сервера
  FXMPPActions.Add(TActionStreamFeatures.Create(FXMPPActions));
  //Обработка при получении ошибок
  FXMPPActions.Add(TActionStreamError.Create(FXMPPActions));
  //Обработка при challenge
  FXMPPActions.Add(TActionChallenge.Create(FXMPPActions));
  //Обработка при требовании авторизации SASL
  FXMPPActions.Add(TActionSuccess.Create(FXMPPActions));
  //Обработка запросов подписки
  FXMPPActions.Add(TActionPresenceSubscribe.Create(FXMPPActions));
  //Обработка при получении ошибки при аутентификации
  FXMPPActions.Add(TActionFailure.Create(FXMPPActions));
  //Обработка при получении запроса версии
  FXMPPActions.Add(TActionIQResponseVersion.Create(FXMPPActions));
  //Обработка при получении ping
  FXMPPActions.Add(TActionIQResponsePing.Create(FXMPPActions));
  //Обработка при получении time
  FXMPPActions.Add(TActionIQResponseTime.Create(FXMPPActions));
  //Обработка при получении инф. об изменении контакта
  FXMPPActions.Add(TActionIQRosterSet.Create(FXMPPActions));
  //Обработка при запросе возможностей клиента
  FXMPPActions.Add(TActionIQResponseDiscoInfo.Create(FXMPPActions));
  //Обработка при запросе сущностей
  FXMPPActions.Add(TActionIQResponseDiscoItems.Create(FXMPPActions));
  //Обработка при запросе Last Activity
  FXMPPActions.Add(TActionIQResponseLast.Create(FXMPPActions));
end;

procedure TJabberClient.SendStreamStart;
begin
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('stream:stream', True) do
    begin
      Params.Values['xmlns:stream'] := XMLNS_STREAMS;
      Params.Values['version'] := '1.0';
      Params.Values['xmlns'] := XMLNS_CLIENT;
      Params.Values['to'] := FUserServer;
      Params.Values['xml:lang'] := 'en';
      Params.Values['xmlns:xml'] := XMLNS_XML;
    end;
    SendData(Text);
    Free;
  end;
end;

procedure TJabberClient.SendSubscribeAccept(AJID: string);
begin
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('presence') do
    begin
      Params.Values['type'] := 'subscribed';
      Params.Values['to'] := AJID;
    end;
    SendData(Text);
    Free;
  end;
end;

procedure TJabberClient.SendSubscribeCancel(AJID: string);
begin
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('presence') do
    begin
      Params.Values['type'] := 'unsubscribed';
      Params.Values['to'] := AJID;
    end;
    SendData(Text);
    Free;
  end;
end;

procedure TJabberClient.SendUnsubscribe(AJID: string);
begin
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('presence') do
    begin
      Params.Values['type'] := 'unsubscribe';
      Params.Values['to'] := AJID;
    end;
    SendData(Text);
    Free;
  end;
end;

procedure TJabberClient.SendVersion(AJID, ID: string);
var
  WinV: Word;
begin
  WinV := Winapi.Windows.GetVersion and $0000FFFF;
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('iq') do
    begin
      Params.Values['type'] := 'result';
      Params.Values['to'] := AJID;
      Params.Values['from'] := JID + '/' + Resource;
      Params.Values['id'] := ID;
      with Children.AddOpenTag('query') do
      begin
        Params.Values['xmlns'] := XMLNS_VERSION;
        Children.AddOpenTag('name').AsString := JabberClientName;
        Children.AddCloseTag;

        Children.AddOpenTag('version').AsString := JabberClientVersion;
        Children.AddCloseTag;

        Children.AddOpenTag('os').AsString := 'Windows ' + IntToStr(Lo(WinV)) + '.' + IntToStr(Hi(WinV));
        Children.AddCloseTag;
      end;
    end;
    SendData(Text);
    Free;
  end;
end;

procedure TJabberClient.SendPing(AJID, ID: string);
begin
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('iq') do
    begin
      Params.Values['type'] := 'result';
      Params.Values['to'] := AJID;
      Params.Values['from'] := JID + '/' + Resource;
      Params.Values['id'] := ID;
    end;
    SendData(Text);
    Free;
  end;
end;

procedure TJabberClient.SendTime(AJID, ID: string);
begin
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('iq') do
    begin
      Params.Values['type'] := 'result';
      Params.Values['to'] := AJID;
      Params.Values['from'] := JID + '/' + Resource;
      Params.Values['id'] := ID;
      with Children.AddOpenTag('query') do
      begin
        Params.Values['xmlns'] := XMLNS_TIME;
        Children.AddOpenTag('utc').AsString := DateToStr(Now) + ' ' + TimeToStr(Now);
        Children.AddCloseTag;

        Children.AddOpenTag('tz').AsString := 'MDT';
        Children.AddCloseTag;

        Children.AddOpenTag('display').AsString := DateToStr(Now) + ' ' + TimeToStr(Now);
        Children.AddCloseTag;
      end;
    end;
    SendData(Text);
    Free;
  end;
end;

procedure TJabberClient.SendTimeURN(AJID, ID: string);
begin
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('iq') do
    begin
      Params.Values['type'] := 'result';
      Params.Values['to'] := AJID;
      Params.Values['from'] := JID + '/' + Resource;
      Params.Values['id'] := ID;
      with Children.AddOpenTag('time') do
      begin
        Params.Values['xmlns'] := XMLNS_URN_TIME;
        Children.AddOpenTag('utc').AsString := FormatDateTime('YYYY-MM-DD''T''HH:MM:SS''Z''', TTimeZone.local.ToUniversalTime(Now));
        Children.AddCloseTag;

        Children.AddOpenTag('tzo').AsString := TTimeZone.local.UtcOffset.ToString;
        Children.AddCloseTag;
      end;
    end;
    SendData(Text);
    Free;
  end;
end;

procedure TJabberClient._OnConnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  FConnected := True;
  SendStreamStart;
  InProcess := False;
  if Assigned(FOnConnect) then
    FOnConnect(Self);
end;

procedure TJabberClient._OnDisconnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  FConnected := False;
  FRosetReceived := False;
  FJabberOnLine := False;
  InProcess := False;
  if Assigned(FOnDisconnect) then
    FOnDisconnect(Self);
end;

procedure TJabberClient.DoError(Sender: TObject; Error: string);
begin
  if Assigned(FOnError) then
    FOnError(Self, ERR_PROTOCOL, Error);
end;

procedure TJabberClient._OnConnectError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
  if Assigned(FOnConnectError) then
    FOnConnectError(Self);
end;

function TJabberClient.DeleteBadSymbols(Value: string): string;
var
  i: Integer;
begin
  Result := Value;
  for i := 0 to Length(Value) do
  begin
    if (Result[i + 1] < #$20) and (Result[i + 1] <> #$0D) and (Result[i + 1] <> #$0A) then
      Result[i + 1] := '?';
  end;
end;

destructor TJabberClient.Destroy;
begin
  FXMPPItems.Free;
  FXMPPActions.Clear;
  FXMPPActions.Free;
  FSocket.Free;
  inherited;
end;

procedure TJabberClient.Disconnect;
begin
  if FConnected then
  begin
    FSocket.Close;
    _OnDisconnect(Self, nil);
  end;
end;

procedure TJabberClient.EndSetPresence;
begin
  FWaitSetPresence := False;
  SetPresence;
end;

procedure TJabberClient.SendSession;
begin
  FXMPPActions.Add(TActionIQSetSession.Create(FXMPPActions));
end;

function TJabberClient.SendSetBind: string;
begin
  Result := GetUniqueID;
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('iq') do
    begin
      Params.Values['type'] := 'set';
      Params.Values['id'] := Result;
      with Children.AddOpenTag('bind') do
      begin
        Params.Values['xmlns'] := XMLNS_XMPP_BIND;
        with Children.AddOpenTag('resource') do
        begin
          AsString := FResource;
        end;
      end;
    end;
    SendData(Text);
    Free;
  end;
end;

procedure TJabberClient.SendSASLResponse(ChallengeValue: string);
begin
  SendResponse(XMLNS_XMPP_SASL, GetSASLResponse(ChallengeValue));
end;

function TJabberClient.SendSetSession: string;
begin
  Result := GetUniqueID;
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('iq') do
    begin
      Params.Values['type'] := 'set';
      Params.Values['id'] := Result;
      with Children.AddOpenTag('session') do
      begin
        Params.Values['xmlns'] := XMLNS_XMPP_SESSION;
      end;
    end;
    SendData(Text);
    Free;
  end;
end;

function TJabberClient.SendSetVCard(Card: TVCard): string;
var
  i: Integer;
  FlagAddr: TAddressFlag;
  FlagTel: TTelFlag;
  FlagEmail: TEmailFlag;
begin
  Result := GetUniqueID;
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('iq') do
    begin
      Params.Values['type'] := 'set';
      Params.Values['id'] := Result;
      with Children.AddOpenTag('vCard') do
      begin
        Params.Values['xmlns'] := XMLNS_VCARD;
        Children.AddTagValue('FN', Card.FullName);

        //Имя
        with Children.AddOpenTag('N') do
        begin
          Children.AddTagValue('GIVEN', Card.Name.FirstName);
          Children.AddTagValue('MIDDLE', Card.Name.MiddleName);
          Children.AddTagValue('FAMILY', Card.Name.LastName);
        end;
        Children.AddCloseTag;

        //Общая инф.
        Children.AddTagValue('NICKNAME', Card.NickName);
        Children.AddTagValue('BDAY', FormatDateTime('YYYY-MM-DD', Card.BirthDay));
        Children.AddTagValue('URL', Card.URL);
        Children.AddTagValue('TITLE', Card.Title);
        Children.AddTagValue('ROLE', Card.Role);
        Children.AddTagValue('DESC', Card.Desc);

        //Фото
        with Children.AddOpenTag('PHOTO') do
        begin
          Children.AddTagValue('TYPE', Card.Photo.PhotoType);
          Children.AddTagValue('BINVAL', Card.Photo.BinVal);
        end;
        Children.AddCloseTag;

        //Адреса
        for i := Low(Card.Address) to High(Card.Address) do
        begin
          with Children.AddOpenTag('ADR') do
          begin
            for FlagAddr in Card.Address[i].Flags do
              Children.AddTagValue(AddressFlagToStr[FlagAddr], '');
            Children.AddTagValue('EXTADD', Card.Address[i].ExtAdd);
            Children.AddTagValue('STREET', Card.Address[i].Street);
            Children.AddTagValue('LOCALITY', Card.Address[i].Locality);
            Children.AddTagValue('REGION', Card.Address[i].Region);
            Children.AddTagValue('PCODE', Card.Address[i].PCode);
            Children.AddTagValue('CTRY', Card.Address[i].Country);
          end;
          Children.AddCloseTag;
        end;

        //Телефоны
        for i := Low(Card.Tel) to High(Card.Tel) do
        begin
          with Children.AddOpenTag('TEL') do
          begin
            for FlagTel in Card.Tel[i].Flags do
              Children.AddTagValue(TelFlagToStr[FlagTel], '');
            Children.AddTagValue('NUMBER', Card.Tel[i].Number);
          end;
          Children.AddCloseTag;
        end;

        //Почта
        for i := Low(Card.EMail) to High(Card.EMail) do
        begin
          with Children.AddOpenTag('EMAIL') do
          begin
            for FlagEmail in Card.EMail[i].Flags do
              Children.AddTagValue(EmailFlagToStr[FlagEmail], '');
            Children.AddTagValue('USERID', Card.EMail[i].UserId);
          end;
          Children.AddCloseTag;
        end;

        //Организация
        with Children.AddOpenTag('ORG') do
        begin
          Children.AddTagValue('ORGNAME', Card.Organisation.Name);
          Children.AddTagValue('ORGUNIT', Card.Organisation.OrgUnit);
        end;
        Children.AddCloseTag;
      end;
      Children.AddCloseTag;
    end;
    SendData(Text);
    Free;
  end;
end;

procedure TJabberClient.SendData(Str: string);
var
  Data: UTF8String;
begin
  try
    Data := UTF8Encode(DeleteBadSymbols(Str));
    FSocket.Socket.SendBuf((@Data[1])^, Length(Data));
    _OnSend(Self, Str);
  except
    _OnSendError(Self, FSocket.Socket);
  end;
end;

procedure TJabberClient._OnReceive(Sender: TObject; Socket: TCustomWinSocket);
begin
  ParseReceive(Socket.ReceiveText);
end;

procedure TJabberClient._OnSend(Sender: TObject; StrData: string);
begin
  if Assigned(OnSendData) then
    FOnSendData(Sender, StrData);
end;

procedure TJabberClient._OnSendError(Sender: TObject; Socket: TCustomWinSocket);
begin
  //
end;

procedure TJabberClient.DoGetSubscribe(From, Nick: string);
begin
  if Assigned(FOnSubscribe) then
    FOnSubscribe(Self, From, Nick);
end;

procedure TJabberClient.ParseReceive(Text: string);
var
  CheckedStr: string;
  XMLParser: TGmXML;
  XMLItem: TGmXmlNode;
  ItemName: string;
  Handled: Boolean;
  Item: TXMPPItem;
  i: Integer;
begin
  XMLStr := XMLStr + UTF8ToString(Text);
  InProcess := True;
  while XMLStr <> '' do
  begin
    // В CheckStr вытаскиваем завершенные XML данные для дальнейшего разбора
    // В XMLStr остается продолжение если оно есть
    CheckedStr := StrForParsing(XMLStr);
    if CheckedStr = '' then
      Break;

    // Заменяем символы ' на "
    CheckedStr := StringReplace(CheckedStr, '''', '"', [rfReplaceAll]);

    // Генерируем событие что пришли данные
    if Assigned(FOnReceiveData) then
    begin
      Handled := False;
      FOnReceiveData(Self, CheckedStr, Handled);
      if Handled then
        Continue;
    end;
    //Если данные не были перехвачены, то добавляем их в очередь
    Item.Data := CheckedStr;
    Item.Date := Now;
    FXMPPItems.Add(Item);
  end;

  //Пока есть что-то в очереди, проходим по очереди и пытаемся найти обработчик данных
  //Забираем элемент из очереди
  //Если обработчик найден, то начинаем обработку и прерываем очередь
  //Если обработчик не найден, то возвращаем элемент в очередь
  //Если ни для кого обработчик не найден, значит мы что-то ждём, прерываем весь цикл обработки
  //Вернёмся сюда, когда что-то придёт
  while FXMPPItems.Count > 0 do
  begin
    Handled := False;
    XMLParser := TGmXML.Create;
    for i := 0 to FXMPPItems.Count - 1 do
    begin
      if i > FXMPPItems.Count - 1 then
        Break;

      Handled := True;
      try
        Item := FXMPPItems[i];
        FXMPPItems.Delete(i);
        XMLParser.Nodes.Clear;
        XMLParser.Text := Item.Data;
        XMLItem := XMLParser.Nodes.Root;

        //Обработчик
        if FXMPPActions.Execute(XMLItem) then
        begin
          Break;
        end;

        //Те, что не были обработаны
        ItemName := XMLItem.Name;
        if ItemName = XMLNS_IQUERY then
        begin
          DoGetIQ(Self, XMLItem);
          Break;
        end;

        if ItemName = XMLNS_STREAM then
        begin
          Break;
        end;

        if ItemName = XMLNS_ITEMPRESENCE then
        begin
          if FRosetReceived then
          begin
            DoGetPresence(Self, XMLItem);
            Break;
          end;
        end;
      except
        on E: Exception do
        begin
          DoError(Self, E.Message);
        end
      end;

        //Если ни мы, ни код выше не обработал данные (debug)
      FOnReceiveData(Self, 'Not handled :' + ItemName, Handled);
      FXMPPItems.Add(Item);
      Handled := False;
    end;
    XMLParser.Free;
    if not Handled then
      Break;
  end;
  InProcess := False;
end;

function TJabberClient.RenameContact(Item: TRosterItem): Boolean;
var
  Action: TActionIQContactRename;
begin
  Result := False;
  Action := TActionIQContactRename.Create(FXMPPActions, Item);
  FXMPPActions.Add(Action);
  while not Action.IsTimeout do
  begin
    if Action.Executed then
    begin
      Result := Action.Status;
      Break;
    end;
    FXMPPActions.CheckTimouts;
    Application.ProcessMessages;
  end;
  FXMPPActions.Delete(Action);
end;

function TJabberClient.AddContact(Item: TRosterItem): Boolean;
var
  Action: TActionIQContactAdd;
begin
  Result := False;
  Action := TActionIQContactAdd.Create(FXMPPActions, Item);
  FXMPPActions.Add(Action);
  while not Action.IsTimeout do
  begin
    if Action.Executed then
    begin
      Result := Action.Status;
      Break;
    end;
    FXMPPActions.CheckTimouts;
    Application.ProcessMessages;
  end;
  FXMPPActions.Delete(Action);
end;

function TJabberClient.AddContact(AJID, ANick: string): Boolean;
var
  Item: TRosterItem;
begin
  Item := TRosterItem.Create(AJID, ANick);
  Result := AddContact(Item);
  Item.Free;
end;

// Отправка на сервер типа аутентификации
procedure TJabberClient.SendBind;
begin
  FXMPPActions.Add(TActionIQSetBind.Create(FXMPPActions));
end;

function TJabberClient.SendDeleteContact(AJID: string): string;
begin
  Result := GetUniq;
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('iq') do
    begin
      Params.Values['type'] := 'set';
      Params.Values['id'] := Result;
      with Children.AddOpenTag('query') do
      begin
        Params.Values['xmlns'] := XMLNS_ROSTER;
        with Children.AddOpenTag('item') do
        begin
          Params.Values['subscription'] := 'remove';
          Params.Values['jid'] := AJID;
        end;
      end;
    end;
    SendData(Text);
    Free;
  end;
end;

function TJabberClient.ClientName: string;
begin
  Result := 'IMJabber';
end;

procedure TJabberClient.SendDiscoInfo(AJID, ID: string);
begin
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('iq') do
    begin
      Params.Values['type'] := 'result';
      Params.Values['to'] := AJID;
      Params.Values['from'] := JID + '/' + Resource;
      Params.Values['id'] := ID;
      with Children.AddOpenTag('query') do
      begin
        Params.Values['xmlns'] := XMLNS_DISCOINFO;
        with Children.AddOpenTag('identity') do
        begin
          Params.Values['category'] := 'client';
          Params.Values['type'] := 'pc';
          Params.Values['name'] := ClientName;
        end;
        Children.AddCloseTag;

        Children.AddTagWithParam('feature', 'var', XMLNS_DISCOINFO);
        Children.AddTagWithParam('feature', 'var', XMLNS_DISCOITEMS);
        Children.AddTagWithParam('feature', 'var', XMLNS_CHATMARKERS0);
        Children.AddTagWithParam('feature', 'var', XMLNS_VERSION);
        Children.AddTagWithParam('feature', 'var', XMLNS_PING);
        Children.AddTagWithParam('feature', 'var', XMLNS_TIME);
        Children.AddTagWithParam('feature', 'var', XMLNS_URN_TIME);
        Children.AddTagWithParam('feature', 'var', XMLNS_ATTENTION);
      end;
    end;
    SendData(Text);
    Free;
  end;
end;

procedure TJabberClient.SendDiscoItems(AJID, ID: string);
begin
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('iq') do
    begin
      Params.Values['type'] := 'result';
      Params.Values['to'] := AJID;
      Params.Values['from'] := JID + '/' + Resource;
      Params.Values['id'] := ID;
      with Children.AddOpenTag('query') do
      begin
        Params.Values['xmlns'] := XMLNS_DISCOITEMS;
        with Children.AddOpenTag('item') do
        begin
          Params.Values['jid'] := JID;
          Params.Values['name'] := UserNick;
        end;
        Children.AddCloseTag;
      end;
    end;
    SendData(Text);
    Free;
  end;
end;

function TJabberClient.SendEnterChat(AConf, ANick, APassword: string): string;
begin
  Result := GetUniqueID;
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('presence') do
    begin
      Params.Values['id'] := Result;
      Params.Values['to'] := AConf + '/' + ANick;
      if FUserStatus <> stNormal then
        Children.AddOpenTag('show').AsString := ShowTypeStr[FUserStatus];
      Children.AddCloseTag;
      Children.AddOpenTag('status').AsString := FUserStatusText;
      Children.AddCloseTag;
      Children.AddOpenTag('priority').AsInteger := FPriority;
      Children.AddCloseTag;
      with Children.AddOpenTag('x') do
      begin
        Params.Values['xmlns'] := XMLNS_VCARDUPDATE;
        Children.AddTagValue('photo', FImageVCardSHA);
      end;
      Children.AddCloseTag;
      if not APassword.IsEmpty then
      begin
        with Children.AddOpenTag('x') do
        begin
          Params.Values['xmlns'] := XMLNS_MUC;
          Children.AddTagValue('password', APassword);
        end;
        Children.AddCloseTag;
      end;
    end;
    SendData(Text);
    Free;
  end;
end;

function TJabberClient.SendAddSetContact(Item: TRosterItem): string;
var
  i: Integer;
begin
  Result := GetUniq;
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('iq') do
    begin
      Params.Values['type'] := 'set';
      Params.Values['id'] := Result;
      with Children.AddOpenTag('query') do
      begin
        Params.Values['xmlns'] := XMLNS_ROSTER;
        with Children.AddOpenTag('item') do
        begin
          Params.Values['name'] := Item.Name;
          Params.Values['jid'] := Item.JID;
          for i := 0 to Item.Groups.Count - 1 do
          begin
            Children.AddTagValue('group', Item.Groups[i]);
          end;
        end;
      end;
    end;
    SendData(Text);
    Free;
  end;
end;

function TJabberClient.SendAuthRemove(AJID: string): string;
begin
  Result := GetUniq;
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('iq') do
    begin
      Params.Values['from'] := JID;
      Params.Values['type'] := 'set';
      Params.Values['id'] := Result;
      Params.Values['to'] := JID + '/' + Resource;
      with Children.AddOpenTag('query') do
      begin
        Params.Values['xmlns'] := XMLNS_ROSTER;
        with Children.AddOpenTag('item') do
        begin
          Params.Values['subscription'] := 'to';
          Params.Values['jid'] := AJID;
        end;
      end;
    end;
    SendData(Text);
    Free;
  end;
end;

procedure TJabberClient.SendAuthRequest(AJID: string);
begin
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('presence') do
    begin
      Params.Values['type'] := 'subscribe';
      Params.Values['to'] := AJID;
      with Children.AddOpenTag('nick') do
      begin
        Params.Values['xmlns'] := XMLNS_NICK;
        AsString := UserNick;
      end;
    end;
    SendData(Text);
    Free;
  end;
end;

procedure TJabberClient.SendAuthType(AuthType: TMechanisms);
begin
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('auth') do
    begin
      Params.Values['xmlns'] := XMLNS_XMPP_SASL;
      Params.Values['mechanism'] := MechanismStr[AuthType];
    end;
    SendData(Text);
    Free;
  end;
end;

procedure TJabberClient.SendReadMessage(AJID, MessageID: string);
begin
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('message') do
    begin
      Params.Values['from'] := JID;
      Params.Values['to'] := AJID;
      Params.Values['type'] := 'chat';
      Params.Values['id'] := GetUniqueID;
    end;
    with Nodes.AddOpenTag('displayed') do
    begin
      Params.Values['xmlns'] := XMLNS_CHATMARKERS0;
      Params.Values['id'] := MessageID;
    end;
    SendData(Text);
    Free;
  end;
end;

procedure TJabberClient.SendResponse(XMLNX, ResponseValue: string);
begin
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('response') do
    begin
      Params.Values['xmlns'] := XMLNX;
      AsString := ResponseValue;
    end;
    SendData(Text);
    Free;
  end;
end;

function TJabberClient.GetReceiveProcess: Boolean;
begin
  Result := FInReceiveProcess > 0;
end;

procedure TJabberClient.GetRoster;
begin
  FXMPPActions.Add(TActionIQGetRoster.Create(FXMPPActions));
end;

procedure TJabberClient.GetBookMarks;
begin
  FXMPPActions.Add(TActionIQGetBookmarks.Create(FXMPPActions));
end;

function TJabberClient.GetJID: string;
begin
  Result := FUserName + '@' + FUserServer;
end;

procedure TJabberClient.SetAuthHash(const Value: string);
begin
  FAuthHash := Value;
end;

procedure TJabberClient.SetInReceiveProcess(const Value: Boolean);
begin
  if Value then
    Inc(FInReceiveProcess)
  else
    Dec(FInReceiveProcess);
  if Assigned(FOnWorkState) then
    FOnWorkState(Self, FInReceiveProcess > 0);
end;

procedure TJabberClient.SetJabberClientName(const Value: string);
begin
  FJabberClientName := Value;
end;

procedure TJabberClient.SetJabberClientVersion(const Value: string);
begin
  FJabberClientVersion := Value;
end;

procedure TJabberClient.SetJID(const Value: string);
begin
  FUserName := Copy(Value, 1, Pos('@', Value) - 1);
  if FUserNick.IsEmpty then
    FUserNick := FUserName;
  FUserServer := Copy(Value, Pos('@', Value) + 1, Length(Value));
end;

procedure TJabberClient.SetPresence;
begin
  //Этот флаг необходим, чтобы позволить устанавливать значения присутствия оффлайн
  //При подключении, мы установим сразу нужные значения
  if not FJabberOnLine then
    Exit;

  SendPresence;
end;

procedure TJabberClient.SendPresenceUnavailable(Target: string);
begin
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('presence') do
    begin
      Params.Values['to'] := Target;
      Params.Values['type'] := 'unavailable';
    end;
    SendData(Text);
    Free;
  end;
end;

function TJabberClient.SendPresence(Target: string): string;
begin
  Result := GetUniqueID;
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('presence') do
    begin
      Params.Values['id'] := Result;
      if not Target.IsEmpty then
        Params.Values['to'] := Target;
      if FUserStatus <> stNormal then
        Children.AddOpenTag('show').AsString := ShowTypeStr[FUserStatus];
      Children.AddCloseTag;
      Children.AddOpenTag('status').AsString := FUserStatusText;
      Children.AddCloseTag;
      Children.AddOpenTag('priority').AsInteger := FPriority;
      Children.AddCloseTag;
      with Children.AddOpenTag('x') do
      begin
        Params.Values['xmlns'] := XMLNS_VCARDUPDATE;
        Children.AddTagValue('photo', FImageVCardSHA);
      end;
      Children.AddCloseTag;
    end;
    SendData(Text);
    Free;
  end;
end;

procedure TJabberClient.SetPriority(const Value: Integer);
begin
  FPriority := Min(Max(-128, Value), 127);
  if not FWaitSetPresence then
    SetPresence;
end;

procedure TJabberClient.SetUserNick(const Value: string);
begin
  FUserNick := Value;
end;

procedure TJabberClient.SetUserStatus(const Value: TShowType);
begin
  FUserStatus := Value;
  if not FWaitSetPresence then
    SetPresence;
end;

procedure TJabberClient.SetUserStatusText(const Value: string);
begin
  FUserStatusText := ToEscaping(Value);
  if not FWaitSetPresence then
    SetPresence;
end;

procedure TJabberClient.SetVCard(Card: TVCard);
begin
  FXMPPActions.Add(TActionIQSetVCard.Create(FXMPPActions, Card));
end;

procedure TJabberClient.UpdateImageHash(BinVal: string);
begin
  FImageVCardSHA := Base64ToSHA(BinVal);
end;

procedure TJabberClient.DoJabberOnline(Sender: TObject);
begin
  if not FJabberOnLine then
  begin
    FLoginError := False;
    FJabberOnLine := True;
    // Запрашиваем свою карточку
    FVCard := GetVCard(JID);
    UpdateImageHash(FVCard.Photo.BinVal);
    // Устанавливаем состояние
    SetPresence;
    // Запрашиваем ростер
    GetRoster;
    // Запрашиваем BookMarks
    GetBookMarks;
    //FImageVCardSHA := '';
    if Assigned(OnJabberOnLine) then
      FOnJabberOnline(Self);
  end;
end;

function TJabberClient.SendGetBookmarks: string;
begin
  Result := GetUniqueID;
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('iq') do
    begin
      Params.Values['type'] := 'get';
      Params.Values['id'] := Result;
      with Nodes.AddOpenTag('query') do
      begin
        Params.Values['xmlns'] := XMLNS_PRIVATE;
        with Nodes.AddOpenTag('storage') do
        begin
          Params.Values['xmlns'] := XMLNS_BM;
        end;
      end;
    end;
    SendData(Text);
    Free;
  end;
end;

function TJabberClient.SendGetDiscoInfo(AServer: string): string;
begin
  Result := GetUniqueID;
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('iq') do
    begin
      Params.Values['type'] := 'get';
      Params.Values['id'] := Result;
      Params.Values['to'] := AServer;
      with Children.AddOpenTag('query') do
      begin
        Params.Values['xmlns'] := XMLNS_DISCOITEMS;
       { with Children.AddOpenTag('set') do
        begin
          Params.Values['xmlns'] := XMLNS_RSM;
          Children.AddTagValue('max', '300');
          Children.AddTagValue('index', First.ToString);
        end; }
      end;
    end;
    SendData(Text);
    Free;
  end;
end;

function TJabberClient.SendGetRoster: string;
begin
  Result := GetUniqueID;
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('iq') do
    begin
      Params.Values['type'] := 'get';
      Params.Values['id'] := Result;
      with Nodes.AddOpenTag('query') do
      begin
        Params.Values['xmlns'] := XMLNS_ROSTER;
      end;
    end;
    SendData(Text);
    Free;
  end;
end;

function TJabberClient.SendGetVCard(AJID: string): string;
begin
  Result := GetUniqueID;
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('iq') do
    begin
      Params.Values['type'] := 'get';
      Params.Values['to'] := AJID;
      Params.Values['id'] := Result;
      with Nodes.AddOpenTag('vCard') do
      begin
        Params.Values['xmlns'] := XMLNS_VCARD;
        Params.Values['version'] := '3.0';
      end;
    end;
    SendData(Text);
    Free;
  end;
end;

function TJabberClient.SendGetVersion(AJID: string): string;
begin
  Result := GetUniqueID;
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('iq') do
    begin
      Params.Values['type'] := 'get';
      Params.Values['from'] := JID + '/' + Resource;
      Params.Values['to'] := AJID;
      Params.Values['id'] := Result;
      with Nodes.AddOpenTag('query') do
      begin
        Params.Values['xmlns'] := XMLNS_VERSION;
      end;
    end;
    SendData(Text);
    Free;
  end;
end;

function TJabberClient.SendInvite(AJID, AConf: string): string;
begin
  Result := GetUniqueID;
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('message') do
    begin
      Params.Values['to'] := AConf;
      Params.Values['id'] := Result;
      with Children.AddOpenTag('x') do
      begin
        Params.Values['xmlns'] := XMLNS_MUCUSER;
        Children.AddTagWithParam('invite', 'to', AJID);
      end;
    end;
    SendData(Text);
    Free;
  end;
end;

procedure TJabberClient.SendIQResult(ID: string);
begin
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('iq') do
    begin
      Params.Values['type'] := 'result';
      Params.Values['to'] := JID + '/' + Resource;
      Params.Values['id'] := ID;
    end;
    SendData(Text);
    Free;
  end;
end;

procedure TJabberClient.SendLast(AJID, ID: string);
begin
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('iq') do
    begin
      Params.Values['type'] := 'result';
      Params.Values['to'] := AJID;
      Params.Values['from'] := JID + '/' + Resource;
      Params.Values['id'] := ID;
      with Children.AddOpenTag('query') do
      begin
        Params.Values['xmlns'] := XMLNS_LAST;
        Params.Values['seconds'] := '0';
      end;
    end;
    SendData(Text);
    Free;
  end;
end;

function TJabberClient.GetSASLResponse(AStr: string): string;
var
  Hasher: TIdHashMessageDigest5;
  MimeDecoder: TIdDecoderMime;
  MimeEncoder: TIdEncoderMime;
  Pairs: TStringlist;
  Mem: TMemoryStream;
  Param_nc: string;
  Param_realm: string;
  Param_nonce: string;
  Param_cnonce: string;
  Param_DigestUri: string;
  NonceAndCnonce: AnsiString;
  //UnSPwd: WideString;
  UnSPwd: TIdBytes;
  Response: string;
begin
  Result := '';
  MimeDecoder := TIdDecoderMIME.Create(nil);
  AStr := MimeDecoder.DecodeString(AStr);
  AStr := StringReplace(AStr, '"', '', [rfReplaceAll]);
  MimeDecoder.Free;

  Pairs := TStringList.Create;
  Pairs.Delimiter := ',';
  Pairs.DelimitedText := AStr;
  Param_realm := Pairs.Values['realm'];
  Param_nonce := Pairs.Values['nonce'];
  Pairs.Free;
  if Copy(AStr, 1, 7) = 'rspauth' then
    Exit;

  MimeEncoder := TIdEncoderMIME.Create(nil);
  Hasher := TIdHashMessageDigest5.Create;

  Param_nc := Format('%8.8d', [1]);
  Param_cnonce := Lowercase(Hasher.HashStringAsHex(MimeEncoder.Encode('1234567890123456789012345678930')));
  Param_DigestUri := 'xmpp/' + UserServer;

  Result := Result + 'username="' + UserName + '",';
  Result := Result + 'realm="' + UserServer + '",';
  Result := Result + 'nonce="' + Param_nonce + '",';
  Result := Result + 'cnonce="' + Param_cnonce + '",';
  Result := Result + 'nc=' + Param_nc + ',';
  Result := Result + 'qop=auth,';
  Result := Result + 'digest-uri="' + Param_DigestUri + '",';
  Result := Result + 'charset=utf-8,';

  Mem := TMemoryStream.Create;
  UnSPwd := HexStrToBytes(AuthHash);
  Mem.Write(UnSPwd[0], 16);
  NonceAndCnonce := ':' + Param_nonce + ':' + Param_cnonce;
  Mem.Write(Pointer(NonceAndCnonce)^, Length(NonceAndCnonce));
  Mem.Position := 0;
  Response := LowerCase(Hasher.HashStreamAsHex(Mem));
  Param_DigestUri := LowerCase(Hasher.HashStringAsHex('AUTHENTICATE:' + Param_DigestUri));
  Response := Response + ':' + Param_nonce + ':' + Param_nc + ':' + Param_cnonce + ':auth:' + Param_DigestUri;
  Response := LowerCase(Hasher.HashStringAsHex(Response));

  Result := MimeEncoder.Encode(Result + 'response=' + Response);

  MimeEncoder.Free;
  Hasher.Free;
  Mem.Free;
end;

class function TJabberClient.GetUniq: string;
begin
  Result := FloatToStr(Double(Now) + Random(1000));
  Result := ShaHASH(Result);
  Result := Copy(Result, 1, 10);
end;

function TJabberClient.GetUniqueID: string;
begin
  Result := TJabberClient.GetUniq;
end;

function TJabberClient.GetVCard(AJID: string): TVCard;
var
  Action: TActionIQVCard;
begin
  Action := TActionIQVCard.Create(FXMPPActions, AJID);
  FXMPPActions.Add(Action);
  while not Action.IsTimeout do
  begin
    Application.ProcessMessages;
    if Action.Executed then
    begin
      Result := Action.VCard;
      Break;
    end;
    FXMPPActions.CheckTimouts;
  end;
  FXMPPActions.Delete(Action);
end;

function TJabberClient.GetListOfConference(Server: string): TConfList;
var
  Action: TActionIQConfList;
begin
  Result := nil;
  Action := TActionIQConfList.Create(FXMPPActions, Server);
  FXMPPActions.Add(Action);
  while not Action.IsTimeout do
  begin
    Application.ProcessMessages;
    if Action.Executed then
    begin
      Result := Action.Result;
      Break;
    end;
    FXMPPActions.CheckTimouts;
  end;
  FXMPPActions.Delete(Action);
  if not Assigned(Result) then
    Result := TConfList.Create;
end;

function TJabberClient.ConferenceEnter(AConf, ANick: string): TConfPresence;
var
  Action: TActionPresenceToConf;
begin
  Action := TActionPresenceToConf.Create(FXMPPActions, AConf, ANick);
  FXMPPActions.Add(Action);
  Result.Error := True;
  while not Action.IsTimeout do
  begin
    Application.ProcessMessages;
    if Action.Executed then
    begin
      Result := Action.Result;
      Break;
    end;
    FXMPPActions.CheckTimouts;
  end;
  FXMPPActions.Delete(Action);
end;

function TJabberClient.GetVersion(AJID: string): TJabberVersion;
var
  Action: TActionIQVersion;
begin
  Action := TActionIQVersion.Create(FXMPPActions, AJID);
  FXMPPActions.Add(Action);
  while not Action.IsTimeout do
  begin
    if Action.Executed then
    begin
      Result := Action.Version;
      Break;
    end;
    FXMPPActions.CheckTimouts;
    Application.ProcessMessages;
  end;
  FXMPPActions.Delete(Action);
end;

procedure TJabberClient.DoGetBookMarks(Sender: TObject; QueryNode: TGmXmlNode);
begin
  if Assigned(OnGetBookMarks) then
    FOnGetBookMarks(Sender, QueryNode);
end;

procedure TJabberClient.DoGetRoster(Sender: TObject; QueryNode: TGmXmlNode);
begin
  if Assigned(OnGetRoster) then
    FOnGetRoster(Sender, QueryNode);
  FRosetReceived := True;
end;

procedure TJabberClient.DoGetIQ(Sender: TObject; QueryNode: TGmXmlNode);
begin
  if Assigned(OnIQ) then
    FOnIQ(Self, QueryNode);
end;

procedure TJabberClient.DoGetMessage(Sender: TObject; Item: TJabberMessage);
begin
  if Assigned(OnMessage) then
    FOnMessage(Self, Item);
end;

procedure TJabberClient.SendAttention(AJID, AMessage: string);
begin
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('message') do
    begin
      Params.Values['from'] := JID + '/' + Resource;
      Params.Values['to'] := AJID;
      Params.Values['type'] := MessageTypeStr[mtHeadLine];
      if AMessage <> '' then
      begin
        Children.AddTagValue('body', ToEscaping(AMessage));
      end;
      Children.AddTagWithParam('attention', 'xmlns', XMLNS_ATTENTION);
    end;
    SendData(Text);
    Free;
  end;
end;

function TJabberClient.SendMessage(AJID, AMessage: string; MessageType: TMessageType): string;
begin
  Result := GetUniqueID;
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('message') do
    begin
      Params.Values['from'] := JID;
      Params.Values['to'] := AJID;
      Params.Values['type'] := MessageTypeStr[MessageType];
      Params.Values['id'] := Result;
      if AMessage <> '' then
      begin
        Children.AddTagValue('body', ToEscaping(AMessage));
      end;
      if MessageType <> mtGroupChat then
        Children.AddTagWithParam('markable', 'xmlns', XMLNS_CHATMARKERS0);
    end;
    SendData(Text);
    Free;
  end;
end;

procedure TJabberClient.DoGetPresence(Sender: TObject; QueryNode: TGmXmlNode);
begin
  if Assigned(OnPresence) then
    FOnPresence(Self, QueryNode);
end;

procedure TJabberClient.StartSetPresence;
begin
  FWaitSetPresence := True;
end;

function TJabberClient.StrForParsing(var Value: string): string;
var
  i, j: Integer;
  XMLItem: string;
  InValue: Boolean;
  ch1, ch2: string;
  FullStr: string;
begin
  Value := Trim(Value);
  if Length(Value) <= 0 then
    Exit('');

  // Проверяем на тэг XML если это тэг идентифицирующй XML то вырезаем его.
  if AnsiLowerCase(Copy(Value, 1, 5)) = '<?xml' then
  begin
    Value := Copy(Value, Pos('>', Value) + 1, Length(Value));
    // Теперь не закрытую строку stream:stream закрываем, чтоб проще было работать
    if Pos('<stream:stream', Value) <> 0 then
      Value := StringReplace(Value, '>', '/>', []);
  end;
  //Проверяем, все ли данные получены до конца
  if Length(Value) > 0 then
    //Заканчиваться должно на '>'
    if Value[Length(Value)] <> '>' then
      Exit('');

  j := 0;
  FullStr := Value;
  InValue := False;
  for i := 0 to Length(Value) - 1 do
  begin
    if (Value[i + 1] = '''') or (Value[i + 1] = '"') then
      InValue := not InValue;

    if not InValue then
    begin
      ch1 := Value[i + 1];
      ch2 := Value[i + 2];
      if (ch1 = '<') and (ch2 <> '/') then
        Inc(j);
      if (ch1 + ch2 = '</') or (ch1 + ch2 = '/>') then
        Dec(j);
    end;

    if j = 0 then
    begin
      //Всё, конец найден, завершаем строку (если там завершающий тег)
      XMLItem := Copy(Value, 1, i);
      Value := Copy(Value, i + 1, Length(Value));
      j := 1;
      if Length(Value) = 0 then
      begin
        Value := FullStr;
        Exit('');
      end;
      try
        while Value[j] <> '>' do
        begin
          if (Value[j] <> '>') then
            XMLItem := XMLItem + Value[j];
          Value := Copy(Value, 2, Length(Value));
          if Value = '' then
          begin
            Value := FullStr;
            Exit('');
          end;
        end;
      except
        begin
          Value := FullStr;
          Exit('');
        end;
      end;
      XMLItem := XMLItem + '>';
      Value := Copy(Value, 2, Length(Value));
      Exit(XMLItem);
    end;
  end;
  Value := FullStr;
  Result := '';
end;

procedure TJabberClient.DoLoginError(Sender: TObject; Error: string);
begin
  Disconnect;
  FLoginError := True;
  if Assigned(FOnLoginError) then
    FOnLoginError(Self, Error);
end;

procedure TJabberClient.DoRosterSet(Sender: TObject; Item: TRosterItem);
begin
  if Assigned(FOnRosterSet) then
    FOnRosterSet(Sender, Item);
end;

{ TXMPPAction }

constructor TXMPPAction.Create(AOwner: TXMPPActions);
begin
  Owner := AOwner;
  FTimeCreate := GetTickCount;
  FTimeout := 30 * 1000;
  FIsTimeout := False;
  FFreeAfterTimeout := False;
  FFreeAfterExecute := False;
end;

procedure TXMPPAction.SetFreeAfterExecute(const Value: Boolean);
begin
  FFreeAfterExecute := Value;
end;

procedure TXMPPAction.SetFreeAfterTimeout(const Value: Boolean);
begin
  FFreeAfterTimeout := Value;
end;

procedure TXMPPAction.SetItem(const Value: string);
begin
  FItem := Value;
end;

procedure TXMPPAction.SetOwner(const Value: TXMPPActions);
begin
  FOwner := Value;
end;

procedure TXMPPAction.SetTimeout(const Value: Cardinal);
begin
  FTimeout := Value;
end;

{ TXMPPActions }

function TXMPPActions.Add(Value: TXMPPAction): Integer;
begin
  Result := inherited Add(Value);
end;

procedure TXMPPActions.CheckTimouts;
var
  i: Integer;
begin
  //Очистка по таймауту (1 за раз)
  for i := 0 to Count - 1 do
  begin
    if Items[i].FreeAfterTimeout then
    begin
      if (Items[i].TimeCreate + Items[i].Timeout) < GetTickCount then
      begin
        Items[i].IsTimeout := True;
      end;
    end;
  end;
end;

procedure TXMPPActions.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Free;
  inherited Clear;
end;

constructor TXMPPActions.Create(Client: TJabberClient);
begin
  inherited Create;
  FJabber := Client;
end;

procedure TXMPPActions.Delete(Action: TXMPPAction);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Items[i] = Action then
    begin
      Delete(i);
      Exit;
    end;
  end;
end;

procedure TXMPPActions.Delete(Index: Integer);
begin
  Items[Index].Free;
  inherited Delete(Index);
end;

function TXMPPActions.Execute(Node: TGmXmlNode): Boolean;
var
  i: Integer;
  Item: string;
begin
  Result := False;
  //Очистка по таймауту (1 за раз)
  CheckTimouts;
  Item := Node.Name;
  //Проверка, выполнение и удаление
  for i := 0 to Count - 1 do
  begin
    if Items[i].Item = Item then
    begin
      if Items[i].Execute(Node) then
      begin
        if Items[i].FreeAfterExecute then
          Delete(i);
        Exit(True);
      end;
    end;
  end;
end;

procedure TXMPPActions.SetJabber(const Value: TJabberClient);
begin
  FJabber := Value;
end;

end.

