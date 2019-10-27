unit Jabber.Types;

interface

uses
  Vcl.Controls, Winapi.Messages, System.SysUtils, System.Classes;

const
  XMLNS_AUTH = 'jabber:iq:auth';
  XMLNS_ROSTER = 'jabber:iq:roster';
  XMLNS_REGISTER = 'jabber:iq:register';
  XMLNS_LAST = 'jabber:iq:last';
  XMLNS_TIME = 'jabber:iq:time';
  XMLNS_VERSION = 'jabber:iq:version';
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
  XMLNS_BM = 'storage:bookmarks';
  XMLNS_PREFS = 'storage:imprefs';
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
  XMLNS_COMMANDS = 'http://jabber.org/protocol/commands';
  XMLNS_CAPS = 'http://jabber.org/protocol/caps';
  XMLNS_ADDRESS = 'http://jabber.org/protocol/address';
  XMLNS_XHTMLIM = 'http://jabber.org/protocol/xhtml-im';
  XMLNS_XHTML = 'http://www.w3.org/1999/xhtml';
  XMLNS_XML = 'http://www.w3.org/XML/1998/namespace';
  XMLNS_SHIM = 'http://jabber.org/protocol/shim';
  XMLNS_NICK = 'http://jabber.org/protocol/nick';
  XMLNS_STREAMS = 'http://etherx.jabber.org/streams';
  XMLNS_ITEMCHALLENGE = 'challenge';
  XMLNS_ITEMSUCCESS = 'success';
  XMLNS_ITEMFAILURE = 'failure';
  XMLNS_ITEMMESSAGE = 'message';
  XMLNS_ITEMPRESENCE = 'presence';
  XMLNS_IQUERY = 'iq';

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
  TErrorType = (ERR_SOCKET, ERR_INTERNAL, ERR_WARNING, ERR_PROXY, ERR_PROTOCOL, ERR_CONNTIMEOUT, ERR_LOGIN);

  TMechanisms = (mecDIGEST_MD5, mecPLAIN, mecNONE);

  TOnError = procedure(Sender: TObject; ErrorType: TErrorType; ErrorMsg: string) of object;

  TOnConnect = procedure(Sender: TObject) of object;

  TOnJabberOnline = procedure(Sender: TObject) of object;

  TOnDisconnect = procedure(Sender: TObject) of object;

  TOnConnectError = procedure(Sender: TObject) of object;

  TOnGetRoster = procedure(Sender: TObject; RosterList: string) of object;

  TOnGetBookMarks = procedure(Sender: TObject; BookMarks: string) of object;

  TOnMessage = procedure(Sender: TObject; XMLMessage: string) of object;

  TOnIQ = procedure(Sender: TObject; XMLMessage: string) of object;

  TOnPresence = procedure(Sender: TObject; Presence: string) of object;

  TOnLoginEror = procedure(Sender: TObject; Error: string) of object;

  TOnSendData = procedure(Sender: TObject; SendStr: string) of object;

  TOnReceiveData = procedure(Sender: TObject; SendStr: string; Handled: Boolean) of object;

  TOnActionResult = procedure(Sender: TObject; IsOK: Boolean) of object;

  TOnSubscribe = procedure(Sender: TObject; From, Nick: string) of object;

  // Тип подписки
  TSubscribeType = (sbNone, sbTo, sbFrom, sbBoth);

  // тип сообщения
  TMessageType = (mtChat, mtError, mtGroupchat, mtHeadline, mtNormal);

  // Тип присутствия
  TShowType = (stNormal, stAway, stChat, stDnd, stXa, stInvisible);

  // описание типа контакта
  TContactType = (ctUser, ctChatRoom, ctTransport, ctNode);

implementation

end.

