
#include "botclient.hh"
#include <iostream>
#include <sstream>
#include <ctime>

using namespace gloox;
using namespace std;

template <class T> static inline std::string to_string(const T& t)
{
	std::stringstream ss;
	ss << t;
	return ss.str();
}

Botclient::Botclient(const std::string& jid,
		     const std::string& resource,
		     const std::string& password,
		     tHandler handler,
		     const std::string& logfile,
		     const std::string& server,
		     int port)
	: m_log(logfile.c_str(), std::ios::out | std::ios::app)
{
	m_client = new Client(JID(jid), password);
	m_vcardmgr = new VCardManager(m_client);
	m_client->registerMessageHandler(this);
	m_client->registerConnectionListener(this);
	m_client->logInstance().registerLogHandler(LogLevelDebug, LogAreaAll, this);
	m_client->setResource(resource);
	if (server != "") m_client->setServer(server);
	if (port != -1) m_client->setPort(port);
	m_client->setPresence(PresenceAvailable, 5, "Awaiting your command.");
	m_handler = handler;
	m_connecterror = 0;
}

Botclient::~Botclient()
{
	delete m_vcardmgr;
	delete m_client;
	m_client = NULL;
	m_vcardmgr = NULL;
	m_log.close();
}

void Botclient::setPhoto(const std::string& type, const std::string& path)
{
	m_photo_type = type;
	m_photo_path = path;
}

int Botclient::go()
{
	m_connecterror = 0;
	log("attempting connect");
	bool rc = m_client->connect();
	log("attempt complete. result = " + to_string<bool>(rc));
	return m_connecterror;
}

void Botclient::halt()
{
	m_client->disconnect();
}

void Botclient::handleMessage(Stanza *stanza, MessageSession *session)
{
	stringstream ss;
	ss << "incoming (subtype " << (int) stanza->subtype() << "): " << stanza->body().substr(0, 40);
	if (stanza->body().length() > 40)
		ss << "...";
	log(ss.str());
	m_handler(stanza->from().full(), stanza->body());
}

void Botclient::handleLog(LogLevel level, LogArea area, const std::string& message)
{
	time_t t = time(NULL);
	struct tm loc;
	char tsbuf[100];

	localtime_r(&t, &loc);
	strftime(tsbuf, 100, "%F %T", &loc);

	std::string slevel, sarea;
	switch (level) {
	  case LogLevelDebug:       slevel = "debug"; break;
	  case LogLevelWarning:     slevel = "warning"; break;
	  case LogLevelError:       slevel = "error"; break;
	  default:                  slevel = "<unknown: " + to_string<int>(level) + ">"; break;
	}
	switch (area) {
	  case LogAreaClassParser:               sarea = "parser"; break;
	  case LogAreaClassConnectionTCPBase:    sarea = "connection"; break;
	  case LogAreaClassClient:               sarea = "client"; break;
	  case LogAreaClassClientbase:           sarea = "clientbase"; break;
	  case LogAreaClassComponent:            sarea = "component"; break;
	  case LogAreaClassDns:                  sarea = "dns"; break;
	  case LogAreaAllClasses:                sarea = "all classes"; break;
	  case LogAreaXmlIncoming:               sarea = "xml incoming"; break;
	  case LogAreaXmlOutgoing:               sarea = "xml outgoing"; break;
	  case LogAreaUser:                      sarea = "user-defined"; break;
	  case LogAreaAll:                       sarea = "all"; break;
	  default:                               sarea = "<unknown: " + to_string<int>(area) + ">"; break;
	}

	m_log << "[" << tsbuf << " :: " << slevel << ", " << sarea << "] " << message << endl;
}

void Botclient::log(const std::string& message)
{
	m_client->logInstance().log(LogLevelWarning, LogAreaUser, message);
}

void Botclient::sendMessage(const std::string& jid, const std::string& msg)
{
	Tag *reply = Stanza::createMessageStanza(jid, msg);
	stringstream ss;
	ss << "outgoing: " << msg.substr(0, 40);
	if (msg.length() > 40)
		ss << "...";
	log(ss.str());
	m_client->send(reply);
}

bool Botclient::onTLSConnect(const CertInfo& info)
{
	log("Established TLS connection");
	return true;
}

void Botclient::onConnect()
{
	log("CONNECTED");
	if (!m_photo_path.empty()) {
		log("Fetching my VCard");
		m_vcardmgr->fetchVCard(m_client->jid(), this);
	}
}

void Botclient::handleVCard(const JID& jid, VCard *vcard)
{
	if (jid.bare() != m_client->jid().bare()) {
		stringstream ss;
		ss << "Got a vcard for " << jid.bare() << " but that isn't me! (I'm " << m_client->jid().bare() << ")";
		log(ss.str());
		delete vcard;
		return;
	}
	filebuf fb;
	stringstream ss;
	if (!fb.open(m_photo_path.c_str(), ios::in|ios::binary)) {
		log("Failed to open photo file " + m_photo_path);
		delete vcard;
		return;
	}
	ss << &fb;
	fb.close();
	vcard->setPhoto(m_photo_type, ss.str());
	m_vcardmgr->storeVCard(vcard, this);
}

void Botclient::handleVCardResult(VCardContext context, const JID& jid, StanzaError se)
{
	if (context == StoreVCard && se == StanzaErrorUndefined) {
		log("Successfully stored my own vcard using " + m_photo_type + " file at " + m_photo_path);
		return;
	}
	std::string direction(context == FetchVCard ? "fetching" : "storing");
	std::string correctness(se == StanzaErrorUndefined ? "Success" : "Failure");
	stringstream ss;

	ss << correctness << " " << direction << " vcard for " << jid.full();
	if (se != StanzaErrorUndefined)
		ss << " (StanzaError = " << (int) se << ")";
	log(ss.str());
}

void Botclient::onDisconnect(ConnectionError e)
{
	m_connecterror = (int) e;
	log("disconnected, status = " + to_string<int>(m_connecterror));
}

void Botclient::onSessionCreateError(SessionCreateError e)
{
	log("session create error, status = " + to_string<int>(e));
}
