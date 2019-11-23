unit Jabber.Types;

interface

uses
  Vcl.Controls, Winapi.Messages, System.SysUtils, System.Classes, GmXml,
  System.Generics.Collections, Vcl.Imaging.pngimage, Vcl.Graphics;

const
  XMLNS_AUTH = 'jabber:iq:auth'; //unusable
  XMLNS_ROSTER = 'jabber:iq:roster';
  XMLNS_REGISTER = 'jabber:iq:register';
  XMLNS_LAST = 'jabber:iq:last';
  XMLNS_TIME = 'jabber:iq:time';
  XMLNS_URN_TIME = 'urn:xmpp:time';
  XMLNS_VERSION = 'jabber:iq:version';
  XMLNS_PING = 'jabber:iq:ping';
  XMLNS_IQOOB = 'jabber:iq:oob';
  XMLNS_BROWSE = 'jabber:iq:browse';
  XMLNS_AGENTS = 'jabber:iq:agents';
  XMLNS_SEARCH = 'jabber:iq:search';
  XMLNS_PRIVATE = 'jabber:iq:private';
  XMLNS_CONFERENCE = 'jabber:iq:conference';
  XMLNS_CLIENT = 'jabber:client';
  XMLNS_XEVENT = 'jabber:x:event';
  XMLNS_XDELAY = 'jabber:x:delay';
  XMLNS_XROSTER = 'jabber:x:roster';
  XMLNS_XCONFERENCE = 'jabber:x:conference';
  XMLNS_XDATA = 'jabber:x:data';
  XMLNS_XOOB = 'jabber:x:oob';
  XMLNS_STREAMFEATURES = 'stream:features';
  XMLNS_STREAMERROR = 'stream:error';
  XMLNS_STREAM = 'stream:stream';
  XMLNS_BM = 'storage:bookmarks';
  XMLNS_PREFS = 'storage:imprefs';
  XMLNS_VCARD = 'vcard-temp';
  XMLNS_VCARDUPDATE = 'vcard-temp:x:update';
  XMLNS_MUC = 'http://jabber.org/protocol/muc';
  XMLNS_MUCOWNER = 'http://jabber.org/protocol/muc#owner';
  XMLNS_MUCADMIN = 'http://jabber.org/protocol/muc#admin';
  XMLNS_MUCUSER = 'http://jabber.org/protocol/muc#user';
  XMLNS_DISCO = 'http://jabber.org/protocol/disco';
  XMLNS_DISCOITEMS = 'http://jabber.org/protocol/disco#items';
  XMLNS_DISCOINFO = 'http://jabber.org/protocol/disco#info';
  XMLNS_SI = 'http://jabber.org/protocol/si';
  XMLNS_FTPROFILE = 'http://jabber.org/protocol/si/profile/file-transfer';
  XMLNS_BYTESTREAMS = 'http://jabber.org/protocol/bytestreams';
  XMLNS_FEATNEG = 'http://jabber.org/protocol/feature-neg';
  XMLNS_CLIENTCAPS = 'http://jabber.org/protocol/caps';
  XMLNS_STREAMERR = 'urn:ietf:params:xml:ns:xmpp-stanzas';
  XMLNS_XMPP_SASL = 'urn:ietf:params:xml:ns:xmpp-sasl';
  XMLNS_XMPP_BIND = 'urn:ietf:params:xml:ns:xmpp-bind';
  XMLNS_XMPP_SESSION = 'urn:ietf:params:xml:ns:xmpp-session';
  XMLNS_CHATMARKERS0 = 'urn:xmpp:chat-markers:0';
  XMLNS_ATTENTION = 'urn:xmpp:attention:0';
  XMLNS_COMMANDS = 'http://jabber.org/protocol/commands';
  XMLNS_CAPS = 'http://jabber.org/protocol/caps';
  XMLNS_ADDRESS = 'http://jabber.org/protocol/address';
  XMLNS_XHTMLIM = 'http://jabber.org/protocol/xhtml-im';
  XMLNS_XHTML = 'http://www.w3.org/1999/xhtml';
  XMLNS_XML = 'http://www.w3.org/XML/1998/namespace';
  XMLNS_SHIM = 'http://jabber.org/protocol/shim';
  XMLNS_RSM = 'http://jabber.org/protocol/rsm';
  XMLNS_NICK = 'http://jabber.org/protocol/nick';
  XMLNS_STREAMS = 'http://etherx.jabber.org/streams';
  XMLNS_ITEMCHALLENGE = 'challenge';
  XMLNS_ITEMSUCCESS = 'success';
  XMLNS_ITEMFAILURE = 'failure';
  XMLNS_ITEMMESSAGE = 'message';
  XMLNS_ITEMPRESENCE = 'presence';
  XMLNS_IQUERY = 'iq';
  XMLNS_XMPP_TIME = 'time';

  // константы видов IM
  JC_STANDART = $00000000;
  JC_JABBER = $00000000;
  JC_ICQ = $00000001;
  JC_AIM = $00000002;

  // Константы стандартных жаберных групп
  JG_ONLINE = 001;
  JG_OFFLINE = 002;
  JG_NOTLIST = 003;

  // Константы состояния пользователей
  S_OFFLINE = $00;    //The user is offline. / Set status to offline
  S_ONLINE = $01;    //Online
  S_AWAY = $02;    //Away
  S_NA = $03;    //N/A
  S_OCCUPIED = $04;    //Occupied
  S_DND = $05;    //Do Not Disturb
  S_FFC = $06;    //Free For Chat
  S_EVIL = $07;    //Злой
  S_DEPRESSION = $08;    //депрессия
  S_ATHOME = $09;    //Дома
  S_ATWORK = $0A;    //На работе
  S_LAUNCH = $0B;    //кушаем
  S_INVISIBLE = $0C;    //Invisible
  S_NOTINLIST = $0D;    //Not in List

  S_MESAGEICON = $08;    // Иконка конверта
  S_NOICON = $FF;    //нет иконки
  // уведомление что есть сообщение
  S_MESSAGE = $00000020;    //Message

  //Запрос на подписку
  S_SUBSCRIBE = $00000021;    //Запрос на подписку

  // Типы подписок отвечают за получение текущего статуса
  // если пользователь не имеет подписки то он не сможет знать текущее состояние контакта
  S_SUBSCRIBE_NONE = $00000030; // не подписаный клиент
  S_SUBSCRIBE_TO = $00000031; // пользователь имеет подписку контакт нет
  S_SUBSCRIBE_FROM = $00000032; // контакт имеет подписку пользователь нет
  S_SUBSCRIBE_BOTH = $00000033; // пользователь и контакт имеют подписку

  // строковые ошибки
  MSG_BigDataForSend = 'Big String for send';
  MSG_StreamError = 'Error Connect';
  MSG_Failure = 'Protokol Error';

type
  TJabberMessage = record
    ID: string;
    From: string;
    Nick: string;
    Subject: string;
    ToJID: string;
    Body: string;
    Delay: Boolean;
    DelayDate: TDateTime;
    MessageType: string;
    Thread: string;
    Displayed: Boolean;
    Attention: Boolean;
    Received: Boolean;
    XMLNS_XOOB: record
      URL: string;
    end;
    Error: record
      Code: string;
      ErrorType: string;
      Text: string;
    end;
  end;

  TConfItem = record
    Name: string;
    JID: string;
  end;

  TConfList = class(TList<TConfItem>)
    ConfLast: string;
    ConfCount: Integer;
  end;

  TErrorType = (ERR_SOCKET, ERR_INTERNAL, ERR_WARNING, ERR_PROXY, ERR_PROTOCOL, ERR_CONNTIMEOUT, ERR_LOGIN);

  TOnError = procedure(Sender: TObject; ErrorType: TErrorType; ErrorMsg: string) of object;

  TOnConnect = procedure(Sender: TObject) of object;

  TOnJabberOnline = procedure(Sender: TObject) of object;

  TOnDisconnect = procedure(Sender: TObject) of object;

  TOnConnectError = procedure(Sender: TObject) of object;

  TOnGetRoster = procedure(Sender: TObject; QueryNode: TGmXmlNode) of object;

  TOnGetBookMarks = procedure(Sender: TObject; QueryNode: TGmXmlNode) of object;

  TOnMessage = procedure(Sender: TObject; Item: TJabberMessage) of object;

  TOnIQ = procedure(Sender: TObject; QueryNode: TGmXmlNode) of object;

  TOnPresence = procedure(Sender: TObject; QueryNode: TGmXmlNode) of object;

  TOnLoginEror = procedure(Sender: TObject; Error: string) of object;

  TOnSendData = procedure(Sender: TObject; SendStr: string) of object;

  TOnReceiveData = procedure(Sender: TObject; SendStr: string; Handled: Boolean) of object;

  TOnActionResult = procedure(Sender: TObject; IsOK: Boolean) of object;

  TOnSubscribe = procedure(Sender: TObject; From, Nick: string) of object;

  TOnWorkState = procedure(Sender: TObject; State: Boolean) of object;

  // Тип подписки
  TSubscribeType = (sbNone, sbTo, sbFrom, sbBoth);

  // тип сообщения
  TMessageType = (mtChat, mtGroupchat, mtHeadLine);

  // Тип присутствия
  TShowType = (stNormal, stAway, stChat, stDnd, stXa, stInvisible, stOffline);

  // Описание типа контакта
  TContactType = (ctUser, ctChatRoom, ctTransport, ctNode);

  TMechanisms = (mecDIGEST_MD5, mecPLAIN, mecNONE);

  TMechanismStr = array[TMechanisms] of string;

  TShowTypeStr = array[TShowType] of string;

  TMessageTypeStr = array[TMessageType] of string;

  TRosterItem = class
    JID: string;
    Name: string;
    Subscription: string;
    StatusText: string;
    Status: TShowType;
    Groups: TStringList;
    Photo: string; //SHA-1 hash
    LastMessage: record
      ID: string;
      Body: string;
      Unread: Boolean;
    end;
    GroupData: record
      Affiliation: string;
      Role: string;
    end;
    Color: TColor;
    Avatar: TPngImage;
    constructor Create; overload;
    constructor Create(AJID, ANick: string); overload;
    destructor Destroy; override;
    function GetDisplayStatus: string;
  end;

  TOnRosterSet = procedure(Sender: TObject; Item: TRosterItem) of object;

  TJabberVersion = record
    Name: string;
    OS: string;
    Version: string;
    Error: string;
  end;

  TAddressFlag = (afHome, afWork, afPostal, afParcel, afDom, afIntl, afPref);

  TAddressFlags = set of TAddressFlag;

  TTelFlag = (tfHome, tfWork, tfVoice, tfFAX, tfPager, tfMSG, tfCell, tfVideo, tfBBS, tfModem, tfISDN, tfPCS, tfPref);

  TTelFlags = set of TTelFlag;

  TEmailFlag = (efHome, efWork, efInternet, efPref, efX400);

  TEmailFlags = set of TEmailFlag;

  TAddress = record
    Flags: TAddressFlags;
    ExtAdd: string;
    Street: string;
    Locality: string;
    Region: string;
    PCode: string;
    Country: string;
  end;

  TTel = record
    Flags: TTelFlags;
    Number: string;
  end;

  TEmail = record
    Flags: TEmailFlags;
    UserId: string;
  end;

  TOrg = record
    Name: string;
    OrgUnit: string;
  end;

  TPhoto = record
    PhotoType: string;
    BinVal: string;
  end;

  TName = record
    FirstName: string;
    MiddleName: string;
    LastName: string;
  end;

  TVCard = record
    FullName: string;
    Name: TName;
    NickName: string;
    BirthDay: TDate;
    URL: string;
    Title: string;
    Role: string;
    Desc: string;
    Address: array of TAddress;
    Tel: array of TTel;
    EMail: array of TEmail;
    Organisation: TOrg;
    Photo: TPhoto;
    function AddAddress(Value: TAddress): Integer;
    function AddEmail(Value: TEmail): Integer;
    function AddTel(Value: TTel): Integer;
    procedure ClearTel;
    procedure ClearAdr;
    procedure ClearEmail;
  end;

  TConfPresence = record
    Error: Boolean;
    ErrorData: record
      Code: string;
      ErrorType: string;
      Conflict: string;
      Text: string;
    end;
    Affiliation: string;
    Role: string;
  end;

  //Такой способ позволяет не забыть добавить строковое представление значения
var
  MechanismStr: array[TMechanisms] of string = ('DIGEST-MD5', 'PLAIN', '');
  ShowTypeStr: array[TShowType] of string = ('available', 'away', 'chat', 'dnd', 'xa', 'invisible', 'offline');
  ShowTypeText: array[TShowType] of string = ('Доступен', 'Отошёл', 'Готов поболтать', 'Не беспокоить', 'Давно отошёл', 'Невидимый', 'Оффлайн');
  MessageTypeStr: array[TMessageType] of string = ('chat', 'groupchat', 'headline');
  AddressFlagToStr: array[TAddressFlag] of string = ('HOME', 'WORK', 'POSTAL', 'PARCEL', 'DOM', 'INTL', 'PREF');
  TelFlagToStr: array[TTelFlag] of string = ('HOME', 'WORK', 'VOICE', 'FAX', 'PAGER', 'MSG', 'CELL', 'VIDEO', 'BBS', 'MODEM', 'ISDN', 'PCS', 'PREF');
  EMailFlagToStr: array[TEmailFlag] of string = ('HOME', 'WORK', 'INTERNET', 'PREF', 'X400');

function XmlToDate(Value: string): TDate;

function StrToShowType(Value: string): TShowType;

function FromEscaping(Value: string): string;

function ToEscaping(Value: string): string;

function AffiliationInfo(Value: string; var Translate: string): TColor;

function RoleInfo(Value: string; var Translate: string): TColor;

implementation

function AffiliationInfo(Value: string; var Translate: string): TColor;
begin
  if Value = 'none' then
  begin
    Translate := '';
    Exit(clNone);
  end;
  if Value = 'member' then
  begin
    Translate := 'Участник';
    Exit($00DA9734);
  end;
  if Value = 'admin' then
  begin
    Translate := 'Админ';
    Exit($003A4DE7);
  end;
  if Value = 'owner' then
  begin
    Translate := 'Владелец';
    Exit($00A6A595);
  end;
  if Value = 'outcast' then
  begin
    Translate := 'Изгнаник';
    Exit($009DBD18);
  end;
  Translate := '';
  Result := clNone;
end;

function RoleInfo(Value: string; var Translate: string): TColor;
begin
  if Value = 'none' then
  begin
    Translate := '';
    Exit(clNone);
  end;
  if Value = 'moderator' then
  begin
    Translate := 'Модератор';
    Exit($001F81E5);
  end;
  if Value = 'participant' then
  begin
    Translate := 'Участник';
    Exit($00DA9734);
  end;
  if Value = 'visitor' then
  begin
    Translate := 'Прохожий';
    Exit($00DA9734);
  end;
  Translate := '';
  Result := clNone;
end;

function ToEscaping(Value: string): string;
begin
  Result := Value;
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&', '&amp;', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '©', '&copy;', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '™', '&trade;', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '"', '&bdquo;', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '"', '&ldquo;', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '''', '&apos;', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, ':\'')', ':'')', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, ':\''(', ':''(', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, ':-\\', ':-\', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '\\m/', '\m/', [rfReplaceAll, rfIgnoreCase]);
end;

function FromEscaping(Value: string): string;
begin
  Result := Value;
  Result := StringReplace(Result, '&gt;', '>', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&amp;', '&', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&lt;', '<', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&quot;', '"', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&copy;', '©', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&trade;', '™', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&bdquo;', '"', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&ldquo;', '"', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&apos;', '''', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, ':'')', ':\'')', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, ':''(', ':\''(', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, ':-\', ':-\\', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '\m/', '\\m/', [rfReplaceAll, rfIgnoreCase]);
end;

function StrToShowType(Value: string): TShowType;
var
  i: Integer;
begin
  Value := LowerCase(Value);
  Result := stNormal;
  for i := Ord(stNormal) to Ord(stOffline) do
    if ShowTypeStr[TShowType(i)] = Value then
      Exit(TShowType(i));
end;

function XmlToDate(Value: string): TDate;
var
  D: TDateTime;
begin
  if Value = '' then
    Result := 0
  else if TryStrToDate(Value, D) then
    Exit(D)
  else
    Result := StrToDateDef(Copy(Value, 9, 2) + '.' + Copy(Value, 6, 2) + '.' + Copy(Value, 1, 4), 0);
end;

{ TRosterItem }

function TRosterItem.GetDisplayStatus: string;
begin
  if (StatusText.IsEmpty) {or (Status = stOffline)} then
    Result := ShowTypeText[Status]
  else
    Result := StatusText;
end;

constructor TRosterItem.Create;
begin
  inherited;
  Status := stOffline;
  Avatar := TPngImage.Create;
  Groups := TStringList.Create;
end;

constructor TRosterItem.Create(AJID, ANick: string);
begin
  Create;
  JID := AJID;
  Name := ANick;
end;

destructor TRosterItem.Destroy;
begin
  inherited;
  Avatar.Free;
  Groups.Free;
end;

{ TVCard }

function TVCard.AddAddress(Value: TAddress): Integer;
begin
  SetLength(Address, Length(Address) + 1);
  Result := High(Address);
  Address[Result] := Value;
end;

function TVCard.AddEmail(Value: TEmail): Integer;
begin
  SetLength(EMail, Length(EMail) + 1);
  Result := High(EMail);
  EMail[Result] := Value;
end;

function TVCard.AddTel(Value: TTel): Integer;
begin
  SetLength(Tel, Length(Tel) + 1);
  Result := High(Tel);
  Tel[Result] := Value;
end;

procedure TVCard.ClearAdr;
begin
  SetLength(Address, 0);
end;

procedure TVCard.ClearEmail;
begin
  SetLength(EMail, 0);
end;

procedure TVCard.ClearTel;
begin
  SetLength(Tel, 0);
end;

end.

