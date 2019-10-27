unit Jabber;

interface

uses
  SysUtils, Classes, IdHashMessageDigest, IdCoderMime, Dialogs, Controls,
  Jabber.Types, GmXML, Windows, IdGlobal, StrUtils, System.Generics.Collections,
  System.Win.ScktComp;

type
  TJabberClient = class;

  TXMPPActions = class;

  TXMPPAction = class abstract
  private
    FOwner: TXMPPActions;
    FFreeAfterExecute: Boolean;
    FItem: string;
    procedure SetOwner(const Value: TXMPPActions);
    procedure SetFreeAfterExecute(const Value: Boolean);
    procedure SetItem(const Value: string);
  public
    function Execute(Node: TGmXmlNode): Boolean; virtual; abstract;
    constructor Create(AOwner: TXMPPActions);
    property Item: string read FItem write SetItem;
    property Owner: TXMPPActions read FOwner write SetOwner;
    property FreeAfterExecute: Boolean read FFreeAfterExecute write SetFreeAfterExecute;
  end;

  TXMPPActions = class(TList<TXMPPAction>)
  private
    FJabber: TJabberClient;
    procedure SetJabber(const Value: TJabberClient);
  public
    constructor Create(Client: TJabberClient);
    function Execute(Node: TGmXmlNode): Boolean;
    function Add(Value: TXMPPAction): Integer;
    procedure Delete(Index: Integer);
    procedure Clear;
    property Jabber: TJabberClient read FJabber write SetJabber;
  end;

  TJabberClient = class(TComponent)
  private
    FUserName: string;          // имя до @
    FUserServer: string;          // имя после @
    FUnicalID: string;
    FJabberPort: Word;              // порт для подключения
    FPassword: string;              // Пароль на подключение
    FSocket: TClientSocket;             // Сокет
    FConnected: Boolean;            // Подсоединен ли?
    FJabberOnLine: Boolean;         // Залогинен ли Jabber
    FResource: string;              // Название ресурса
    FUserStatus: TShowType;
    FOnConnect: TOnConnect;
    FOnJabberOnline: TOnJabberOnline;
    FOnDisconnect: TOnDisconnect;
    FOnConnectError: TOnConnectError;
    FOnSendData: TOnSendData;
    FOnReceiveData: TOnReceiveData;
    FOnGetRoster: TOnGetRoster;
    FOnGetBookMarks: TOnGetBookMarks;
    FOnMessage: TOnMessage;
    FOnIQ: TOnIQ;
    FOnPresence: TOnPresence;
    FOnLoginError: TOnLoginEror;
    FOnError: TOnError;
    FBind: string;
    FPriority: Integer;
    FUserStatusText: string;
    FWaitSetPresence: Boolean;
    FOnConnecting: TOnConnect;
    FUserNick: string;
    FXMPPActions: TXMPPActions;
    FOnSubscribe: TOnSubscribe;
    FInReceiveProcess: Boolean;
    procedure _OnConnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure _OnDisconnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure _OnConnectError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
    procedure _OnGetRoster(Sender: TObject; RosterList: string);
    procedure _OnGetBookMarks(Sender: TObject; BookMarks: string);
    procedure _OnReceive(Sender: TObject; Socket: TCustomWinSocket);
    procedure _OnSend(Sender: TObject; StrData: string);
    procedure _OnMessage(Sender: TObject; XMLMessage: string);
    procedure _OnPresence(Sender: TObject; Presence: string);
    procedure _OnIQ(Sender: TObject; XMLMessage: string);
    procedure FreeSocket;
    procedure ParseReceive(Text: string);
    // ----- Процедуры для работы с сервером ------
    procedure SendAuth_1;
    procedure SendPassword;
    procedure SetPresence;
    procedure SetPriority(const Value: Integer);
    procedure SetUserStatus(const Value: TShowType);
    procedure SetUserStatusText(const Value: string);
    procedure SetJID(const Value: string);
    function GetJID: string;
    procedure SetUserNick(const Value: string);
    procedure SendStreamStart;
  protected
    XMLStr: string;
    function GetDigest: string;
    function GetUniqueID: string;
    function WideStringReplace(Value: string; const OldPattern: string; const NewPattern: string; Flags: TReplaceFlags): string;
    function GetSASLResponse(AStr: string): string;
  public
    procedure _OnJabberOnline(Sender: TObject);
    procedure _OnLoginError(Sender: TObject; Error: string);
    procedure _OnError(Sender: TObject; Error: string);
    procedure _OnSubscribe(From, Nick: string);
    class function GetUniq: string;
    function StrForParsing(var Value: string): string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    procedure SendData(Str: string);
    procedure SendAuthType(AuthType: TMechanisms);
    procedure SendBind;
    procedure SendSession;
    function SendSetSession: string;
    function SendSetBind: string;
    procedure SendResponse(XMLNX, ResponseValue: string);
    procedure SendPresence(AType, ATo: string);
    function SendAuthRemove(AJID: string): string;
    procedure SendAuthRequest(AJID: string);
    procedure SendSubscribeAccept(AJID: string);
    procedure SendSubscribeCancel(AJID: string);
    procedure SendUnsubscribe(AJID: string);
    function SendAddSetContact(AJID, ANick, AGroup: string): string;
    function SendDeleteContact(AJID: string): string;
    procedure GetRoster;
    procedure GetBookMarks;
    property Connected: Boolean read FConnected;
    property Online: Boolean read FJabberOnLine;
    procedure SendMessage(strTo: string; strType: string; strBody: string);
    procedure SendReadMessage(strTo, MessageID: string);
    function DeleteBadSymbols(Value: string): string;
    procedure StartSetPresence;
    procedure EndSetPresence;
    function CheckAccount: Boolean;
    property Actions: TXMPPActions read FXMPPActions;
    procedure AddContact(AJID, ANick, AGroup: string);
    procedure RenameContact(AJID, ANewNick, AGroup: string);
    procedure DeleteContact(AJID: string);
  published
    property OnConnect: TOnConnect read FOnConnect write FOnConnect;
    property OnConnecting: TOnConnect read FOnConnecting write FOnConnecting;
    property OnDisconnect: TOnDisconnect read FOnDisconnect write FOnDisconnect;
    property OnConnectError: TOnConnectError read FOnConnectError write FOnConnectError;
    property OnError: TOnError read FOnError write FOnError;
    property OnReceiveData: TOnReceiveData read FOnReceiveData write FOnReceiveData;
    property OnSendData: TOnSendData read FOnSendData write FOnSendData;
    property OnJabberOnline: TOnJabberOnline read FOnJabberOnline write FOnJabberOnline;
    property OnGetRoster: TOnGetRoster read FOnGetRoster write FOnGetRoster;
    property OnGetBookMarks: TOnGetBookMarks read FOnGetBookMarks write FOnGetBookMarks;
    property OnMessage: TOnMessage read FOnMessage write FOnMessage;
    property OnPresence: TOnPresence read FOnPresence write FOnPresence;
    property OnIQ: TOnIQ read FOnIQ write FOnIQ;
    property OnLoginError: TOnLoginEror read FOnLoginError write FOnLoginError;
    property JabberPort: Word read FJabberPort write FJabberPort;
    property JID: string read GetJID write SetJID;
    property Password: string read FPassword write FPassword;
    property UserName: string read FUserName write FUserName;
    property UserServer: string read FUserServer write FUserServer;
    property Resource: string read FResource write FResource;
    property Priority: Integer read FPriority write SetPriority;
    property UserStatus: TShowType read FUserStatus write SetUserStatus;
    property UserStatusText: string read FUserStatusText write SetUserStatusText;
    property UserNick: string read FUserNick write SetUserNick;
    property OnSubscribe: TOnSubscribe read FOnSubscribe write FOnSubscribe;
  end;

function LoginFromJID(JID: string): string;

function GetMechainsms(XMLItem: TGmXmlNode): TMechanisms;

implementation

uses
  Math, Jabber.Actions, IM.Main;

function ShaHASH(Value: string): string;
var
  Hasher: TIdHashMessageDigest5;
begin
  Hasher := TIdHashMessageDigest5.Create;
  Result := LowerCase(Hasher.HashStringAsHex(Trim(Value)));
  Hasher.Free;
end;

function LoginFromJID(JID: string): string;
begin
  if Pos('/', JID) > 1 then
    Result := Copy(JID, 1, Pos('/', JID) - 1)
  else
    Result := JID;
end;

{ TJabberClient }

procedure TJabberClient.AddContact(AJID, ANick, AGroup: string);
begin
  FXMPPActions.Add(TActionIQContactAdd.Create(FXMPPActions, AJID, ANick, AGroup));
end;

procedure TJabberClient.DeleteContact(AJID: string);
begin
  FXMPPActions.Add(TActionIQContactDelete.Create(FXMPPActions, AJID));
end;

function TJabberClient.CheckAccount: Boolean;
begin
  Result := (FUserServer <> '') and (FUserName <> '') and (FJabberPort > 0);
end;

procedure TJabberClient.Connect;
begin
  if not Connected then
  begin
    FSocket := TClientSocket.Create(nil);
    FSocket.Host := FUserServer;
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
  FInReceiveProcess := False;
  FWaitSetPresence := False;
  FUserServer := 'jabber.ru';
  FUserNick := '';
  FJabberPort := 5222;
  FResource := 'jabbrel';
  FBind := 'bind_1';
  FPriority := 1;
  FXMPPActions := TXMPPActions.Create(Self);

  //Обработка запросов подписки
  FXMPPActions.Add(TActionPresenceSubscribe.Create(FXMPPActions));
  //Обработка при получении ошибок
  FXMPPActions.Add(TActionStreamError.Create(FXMPPActions));
  //Обработка при получении ошибки при аутентификации
  FXMPPActions.Add(TActionFailure.Create(FXMPPActions));
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

procedure TJabberClient._OnConnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  FConnected := True;
  SendStreamStart;
  if Assigned(FOnConnect) then
    FOnConnect(Self);
end;

procedure TJabberClient._OnDisconnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  FConnected := False;
  FJabberOnLine := False;
  if Assigned(FOnDisconnect) then
    FOnDisconnect(Self);
end;

procedure TJabberClient._OnError(Sender: TObject; Error: string);
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
  FXMPPActions.Clear;
  FXMPPActions.Free;
  if FConnected then
  begin
    FreeSocket;
  end;
  inherited;
end;

procedure TJabberClient.Disconnect;
begin
  if FConnected then
  begin
    FreeSocket;
    _OnDisconnect(Self, nil);
  end;
end;

procedure TJabberClient.EndSetPresence;
begin
  FWaitSetPresence := False;
  SetPresence;
end;

procedure TJabberClient.FreeSocket;
begin
  if Assigned(FSocket) then
  begin
    FSocket.Free;
    FSocket := nil;
  end;
end;

procedure TJabberClient.SendSession;
begin
  FXMPPActions.Add(TActionIQSetSession.Create(FXMPPActions));
end;

function TJabberClient.SendSetBind: string;
begin
  Result := FBind;
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

procedure TJabberClient.SendData(Str: string);
var
  DataSize: Integer;
  Data: UTF8String;
begin
  Data := UTF8Encode(DeleteBadSymbols(Str));
  DataSize := Length(Data);
  if DataSize > 65534 then
    raise Exception.Create(MSG_BigDataForSend);

  FSocket.Socket.SendBuf((@Data[1])^, DataSize);
  _OnSend(Self, Str);
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

procedure TJabberClient._OnSubscribe(From, Nick: string);
begin
  if Assigned(FOnSubscribe) then
    FOnSubscribe(Self, From, Nick);
end;

procedure TJabberClient.ParseReceive(Text: string);
var
  CheckedStr: string;
  XMLParser: TGmXML;
  XMLItem, FeatureItem: TGmXmlNode;
  ItemName: string;
  TempStr: string;
  AuthType: TMechanisms;
  Handled: Boolean;
begin
  XMLStr := XMLStr + UTF8ToString(Text);
  if FInReceiveProcess then
    Exit;
  FInReceiveProcess := True;
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

    XMLParser := TGmXML.Create;
    try
      XMLParser.Text := CheckedStr;
      CheckedStr := '';
      if XMLParser.Nodes.Count <= 0 then
        Break; //Почему-то пусто
      // Получаем имя пришедшего элемента MESSAGE, IQ, PRESENCE etc
      XMLItem := XMLParser.Nodes.Root;

      //Обработчик
      if FXMPPActions.Execute(XMLItem) then
        Continue;

      ItemName := XMLItem.Name;

      // Обработка возможностей сервера
      if ItemName = XMLNS_STREAMFEATURES then
      begin
        // Секция механизм аутентификации MECHANISM
        FeatureItem := XMLItem.Children.NodeByName['mechanisms'];
        if Assigned(FeatureItem) then
        begin
          AuthType := GetMechainsms(FeatureItem);
          // Отправляем на сервер механизм аутентификации
          if AuthType <> mecNONE then
            SendAuthType(AuthType)
          else
            raise Exception.Create(MSG_StreamError);
        end;
        // Если секция BIND
        FeatureItem := XMLItem.Children.NodeByName['bind'];
        if Assigned(FeatureItem) then
        begin
          if FeatureItem.Params.Values['xmlns'] = XMLNS_XMPP_BIND then
            SendBind;
        end;
        Continue;
      end;

      // Получили challenge
      if ItemName = XMLNS_ITEMCHALLENGE then
      begin
        SendResponse(XMLNS_XMPP_SASL, GetSASLResponse(XMLItem.AsString));
        Continue;
      end;

      // Получили success
      if ItemName = XMLNS_ITEMSUCCESS then
      begin
        if XMLItem.Params.Values['xmlns'] = XMLNS_XMPP_SASL then
          SendStreamStart;
        Continue;
      end;

      // Получили IQ
      if ItemName = XMLNS_IQUERY then
      begin
        _OnIQ(Self, XMLParser.Text);
        TempStr := XMLItem.Params.Values['id'];

        // Получили bind
       { if TempStr = FBind then
        begin
          SendStr('<iq type="set" id="' + GetUniqueID + '" ><session xmlns="' + XMLNS_XMPP_SESSION + '"/></iq>');
          if XMLItem.Params.Values['type'] = 'result' then
          begin
            if not FJabberOnLine then
            begin
              _OnJabberOnline(Self);
            end;
          end;
        end  // Если запрос пароля то отправляем пароль
        else }
        if TempStr = 'auth_1' then
        begin
          if XMLItem.Params.Values['type'] = 'error' then
          begin
            _OnLoginError(Self, XMLParser.Text);
            FJabberOnLine := False;
            FConnected := False;
          end
          else
            SendPassword;
        end // ответ на правильный пароль
        else if TempStr = 'auth_2' then
        begin
          if XMLItem.Params.Values['type'] = 'error' then
          begin
            _OnLoginError(Self, XMLParser.Text);
            FJabberOnLine := False;
            FConnected := False;
          end;
        end;

        //Если есть запрос
        if XMLItem.Children.NodeByName['query'] <> nil then
        begin
          TempStr := XMLItem.Children.NodeByName['query'].Params.Values['xmlns'];
          // Если ростер
          if (TempStr = XMLNS_ROSTER) and (XMLItem.Params.Values['type'] = 'result') then
          begin
            if not FJabberOnLine then
            begin
              _OnJabberOnline(Self);
            end;
            _OnGetRoster(Self, XMLParser.Text);
          end;
        end;
        Continue;
      end;

      // Сообщения
      if ItemName = XMLNS_ITEMMESSAGE then
      begin
        _OnMessage(Self, XMLParser.Text);
        Continue;
      end;

      // PRESENCE
      if ItemName = XMLNS_ITEMPRESENCE then
      begin
        _OnPresence(self, XMLParser.Text);
        Continue;
      end;
    except
      on E: Exception do
      begin
        _OnError(Self, E.Message);
      end
    end;
    XMLParser.Free;
  end;
  FInReceiveProcess := False;
end;

procedure TJabberClient.RenameContact(AJID, ANewNick, AGroup: string);
begin
  FXMPPActions.Add(TActionIQContactRename.Create(FXMPPActions, AJID, ANewNick, AGroup));
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

function TJabberClient.SendAddSetContact(AJID, ANick, AGroup: string): string;
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
          Params.Values['name'] := ANick;
          Params.Values['jid'] := AJID;
          if AGroup <> '' then
            with Children.AddOpenTag('group') do
            begin
              AsString := AGroup;
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
  //SendStr('<presence type="subscribe" to="' + AJID + '" >' + '<nick xmlns="' + XMLNS_NICK + '">' + JID + '</nick></presence>');
end;

procedure TJabberClient.SendAuthType(AuthType: TMechanisms);
var
  Mec: string;
begin
  case AuthType of
    mecDIGEST_MD5:
      Mec := 'DIGEST-MD5';
    mecPLAIN:
      Mec := 'PLAIN';
  end;
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('auth') do
    begin
      Params.Values['xmlns'] := XMLNS_XMPP_SASL;
      Params.Values['mechanism'] := Mec;
    end;
    SendData(Text);
    Free;
  end;
end;

procedure TJabberClient.SendAuth_1;
begin
  with TGmXML.Create do
  begin
    with Nodes.AddOpenTag('iq') do
    begin
      Params.Values['type'] := 'get';
      Params.Values['id'] := 'auth_1';
      Params.Values['to'] := FUserServer;
      with Children.AddOpenTag('query') do
      begin
        Params.Values['xmlns'] := XMLNS_AUTH;
        with Children.AddOpenTag('username') do
          AsString := FUserName;
      end;
    end;
    SendData(Text);
    Free;
  end;
end;

procedure TJabberClient.SendPassword;
var
  XMLParser: TGmXML;
  XmlItem: TGmXmlNode;
begin
{ TODO : Отправляем на сервер запрос GET должны поместить в очередь на сверку с пришедшим. }
  XMLParser := TGmXML.Create;
  XmlItem := XMLParser.Nodes.AddOpenTag('iq');
  XmlItem.Params.Values['type'] := 'set';
  XmlItem.Params.Values['id'] := 'auth_2';
  XmlItem.Params.Values['to'] := FUserServer;

  XmlItem := XmlItem.Children.AddOpenTag('query');
  XmlItem.Params.Values['xmlns'] := XMLNS_AUTH;
  XmlItem := XmlItem.Children.AddOpenTag('username');
  XmlItem.AsString := FUserName;
  XmlItem.Children.AddCloseTag;

  XmlItem := XmlItem.Children.AddOpenTag('digest');
  XmlItem.AsString := GetDigest;
  XmlItem.Children.AddCloseTag;

  XmlItem := XmlItem.Children.AddOpenTag('resource');
  XmlItem.AsString := FResource;
  XmlItem.Children.AddCloseTag;

  SendData(XMLParser.Text);
  FreeAndNil(XMLParser);
end;

procedure TJabberClient.SendPresence(AType, ATo: string);
begin
  SendData('<presence type="' + AType + '" to="' + ATo + '" />');
end;

procedure TJabberClient.SendReadMessage(strTo, MessageID: string);
var
  XMLParser: TGmXML;
  XMLItem: TGmXmlNode;
begin
{ TODO : Отправляем на сервер запрос GET должны поместить в очередь на сверку с пришедшим. }
  XMLParser := TGmXML.Create;
  XMLItem := XMLParser.Nodes.AddOpenTag('message');

  XMLItem.Params.Values['from'] := FUserName + '@' + FUserServer;
  XMLItem.Params.Values['to'] := strTo;
  XMLItem.Params.Values['type'] := 'chat';
  XMLItem.Params.Values['id'] := GetUniqueID;

  XMLItem := XMLItem.Children.AddOpenTag('displayed');
  XMLItem.Params.Values['xmlns'] := XMLNS_CHATMARKERS0;
  XMLItem.Params.Values['id'] := MessageID;
  SendData(XMLParser.Text);
  FreeAndNil(XMLParser);
end;

procedure TJabberClient.SendResponse(XMLNX, ResponseValue: string);
begin
  if ResponseValue <> '' then
    SendData('<response xmlns="' + XMLNX + '">' + ResponseValue + '</response>')
  else
    SendData('<response xmlns="' + XMLNX + '"/>');
end;

procedure TJabberClient.GetBookMarks;
var
  XMLParser: TGmXML;
  XmlItem: TGmXmlNode;
begin
  { TODO -oГеннадий -c :Пока выключил  26.10.2019 22:58:21 }
  Exit;
  XMLParser := TGmXML.Create;

  XmlItem := XMLParser.Nodes.AddOpenTag('iq');
  XmlItem.Params.Values['type'] := 'get';
  XmlItem.Params.Values['id'] := GetUniqueID;
  XmlItem := XmlItem.Children.AddOpenTag('query');
  XmlItem.Params.Values['xmlns'] := 'jabber:iq:private';
  XmlItem := XmlItem.Children.AddOpenTag('storage');
  XmlItem.Params.Values['xmlns'] := 'storage:bookmarks';
  SendData(XMLParser.Text);
  FreeAndNil(XMLParser);
end;

function TJabberClient.GetDigest: string;
begin
  Result := ShaHash(Trim(FUnicalID) + Trim(Password));
end;

function TJabberClient.GetJID: string;
begin
  Result := FUserName + '@' + FUserServer;
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

{ TODO : Переписать способ установки статуса }
procedure TJabberClient.SetJID(const Value: string);
begin
  FUserName := Copy(Value, 1, Pos('@', Value) - 1);
  if FUserNick = '' then
    FUserName := FUserName;
  FUserServer := Copy(Value, Pos('@', Value) + 1, Length(Value));
end;

procedure TJabberClient.SetPresence;
var
  XMLParser: TGmXML;
  XmlItem: TGmXmlNode;
  FShow: string;
begin
  if not FJabberOnLine then
    Exit;

  XMLParser := TGmXML.Create;
  XmlItem := XMLParser.Nodes.AddOpenTag('presence');
  case FUserStatus of
    stNormal:
      FShow := 'avaliable';
    stAway:
      FShow := 'away';
    stChat:
      FShow := 'chat';
    stDnd:
      FShow := 'dnd';
    stXa:
      FShow := 'xa';
    stInvisible:
      FShow := 'invisible';
  end;
  XmlItem.Children.AddOpenTag('show').AsString := FShow;
  XmlItem.Children.AddOpenTag('status').AsString := FUserStatusText;
  XmlItem := XmlItem.Children.AddOpenTag('priority');
  XmlItem.AsInteger := FPriority;
  SendData(XMLParser.Text);
  XMLParser.Free;
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

procedure TJabberClient._OnJabberOnline(Sender: TObject);
begin
  if not FJabberOnLine then
  begin
    // Устанавливаем состояние
    SetPresence;
    // Запрашиваем ростер
    GetRoster;
    // Запрашиваем BookMarks
    GetBookMarks;
    FJabberOnLine := True;
    if Assigned(OnJabberOnLine) then
      FOnJabberOnline(Self);
  end;
end;

// Отправка запроса на сервер на получение Ростера
procedure TJabberClient.GetRoster;
var
  XMLParser: TGmXML;
  XMLItem: TGmXmlNode;
begin
  XMLParser := TGmXML.Create;
  XMLItem := XMLParser.Nodes.AddOpenTag('iq');
  XMLItem.Params.Values['type'] := 'get';
  XMLItem.Params.Values['id'] := GetUniqueID;
  XMLItem := XMLItem.Children.AddOpenTag('iq');
  XMLItem.Params.Values['xmlns'] := 'jabber:iq:roster';
  SendData(XMLParser.Text);
  FreeAndNil(XMLParser);
end;

procedure parseNameValues(list: TStringlist; str: string);
var
  i: integer;
  q: boolean;
  n, v: string;
  ns, vs: integer;
begin
    // Parse a list of:
    // foo="bar",thud="baz"
    // 12345678901234567890
    // foo=bar,
    // 12345678
    // ns = 1
    // vs = 5
    // i = 9
  ns := 1;
  vs := 1;
  q := false;
  for i := 0 to Length(str) - 1 do
  begin
    if (not q) then
    begin
      if (str[i] = ',') then
      begin
                // end of name-value pair
        if (v = '') then
          v := Copy(str, vs, i - vs);
        //list.Add(n);
        list.Values[n] := v;
        ns := i + 1;
        n := '';
        v := '';
      end
      else if (str[i] = '"') then
      begin
                // if we are quoting... start here
        q := true;
        vs := i + 1;
      end
      else if (str[i] = '=') then
      begin
                // end of name, start of value
        n := Copy(str, ns, i - ns);
        vs := i + 1;
      end;
    end
    else if (str[i] = '"') then
    begin
      v := Copy(str, vs, i - vs);
      q := false;
    end;
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

  UnSPwd := Hasher.HashString(UserName + ':' + UserServer + ':' + Password);

  Mem := TMemoryStream.Create;
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
  Result := (FloatToStr(Double(Now) + Random(1000)));
  Result := ShaHASH(Result);
  Result := Copy(Result, 1, 10);
end;

function TJabberClient.GetUniqueID: string;
begin
  Result := TJabberClient.GetUniq;
end;

procedure TJabberClient._OnGetBookMarks(Sender: TObject; BookMarks: string);
begin
  if Assigned(OnGetBookMarks) then
    FOnGetBookMarks(Sender, BookMarks);
end;

// Событие получаем ROSTER
procedure TJabberClient._OnGetRoster(Sender: TObject; RosterList: string);
begin
  if Assigned(OnGetRoster) then
    FOnGetRoster(Sender, RosterList);
end;

procedure TJabberClient._OnIQ(Sender: TObject; XMLMessage: string);
begin
  if Assigned(OnIQ) then
    FOnIQ(Self, XMLMessage);
end;

procedure TJabberClient._OnMessage(Sender: TObject; XMLMessage: string);
begin
  if Assigned(OnMessage) then
    FOnMessage(Self, XMLMessage);
end;

procedure TJabberClient.SendMessage(strTo, strType, strBody: string);
var
  XMLParser: TGmXML;
  XMLItem: TGmXmlNode;
begin
  XMLParser := TGmXML.Create;
  XMLItem := XMLParser.Nodes.AddOpenTag('message');

  XMLItem.Params.Values['from'] := JID;
  XMLItem.Params.Values['to'] := strTo;
  XMLItem.Params.Values['type'] := strType;
  XMLItem.Params.Values['id'] := GetUniqueID;

  if strBody <> '' then
  begin
    XMLItem := XMLItem.Children.AddOpenTag('body');
    XMLItem.AsDisplayString := strBody;
  end;
  SendData(XMLParser.Text);
  FreeAndNil(XMLParser);
end;

procedure TJabberClient._OnPresence(Sender: TObject; Presence: string);
begin
  if Assigned(OnPresence) then
    FOnPresence(Self, Presence);
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

function TJabberClient.WideStringReplace(Value: string; const OldPattern, NewPattern: string; Flags: TReplaceFlags): string;
var
  SearchStr, Patt, NewStr: string;
  Offset: Integer;
begin
  if rfIgnoreCase in Flags then
  begin
    SearchStr := WideUpperCase(Value);
    Patt := WideUpperCase(OldPattern);
  end
  else
  begin
    SearchStr := Value;
    Patt := OldPattern;
  end;
  NewStr := Value;
  Result := '';
  while SearchStr <> '' do
  begin
    Offset := AnsiPos(Patt, SearchStr);
    if Offset = 0 then
    begin
      Result := Result + NewStr;
      Break;
    end;
    Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
    NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
    if not (rfReplaceAll in Flags) then
    begin
      Result := Result + NewStr;
      Break;
    end;
    SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
  end;
end;

procedure TJabberClient._OnLoginError(Sender: TObject; Error: string);
begin
  Disconnect;
  if Assigned(OnLoginError) then
    FOnLoginError(Self, Error);
end;

{ TXMPPAction }

constructor TXMPPAction.Create(AOwner: TXMPPActions);
begin
  Owner := AOwner;
end;

procedure TXMPPAction.SetFreeAfterExecute(const Value: Boolean);
begin
  FFreeAfterExecute := Value;
end;

procedure TXMPPAction.SetItem(const Value: string);
begin
  FItem := Value;
end;

procedure TXMPPAction.SetOwner(const Value: TXMPPActions);
begin
  FOwner := Value;
end;

{ TXMPPActions }

function TXMPPActions.Add(Value: TXMPPAction): Integer;
begin
  Result := inherited Add(Value);
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
  Item := Node.Name;
  for i := 0 to Count - 1 do
  begin
    if Items[i].Item = Item then
      if Items[i].Execute(Node) then
      begin
        if Items[i].FreeAfterExecute then
          Delete(i);
        Exit(True);
      end;
  end;
end;

procedure TXMPPActions.SetJabber(const Value: TJabberClient);
begin
  FJabber := Value;
end;

end.

