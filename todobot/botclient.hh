
#include <fstream>
#include <client.h>
#include <messagehandler.h>
#include <loghandler.h>
#include <connectionlistener.h>
#include <stanza.h>
#include <messagesession.h>
#include <vcardhandler.h>
#include <vcardmanager.h>

using namespace gloox;

// callback type for message receipts. params are jid and message.
typedef void (*tHandler)(const std::string&, const std::string&);

class Botclient : public MessageHandler, public ConnectionListener, public LogHandler, public VCardHandler
{
public:
  Botclient(const std::string& jid,
	    const std::string& resource,
	    const std::string& password,
	    tHandler handler,
	    const std::string& logfile = "/dev/null",
	    const std::string& server = "",
	    int port = -1);
  virtual ~Botclient();

  void setPhoto(const std::string& type, const std::string& path);
  void sendMessage(const std::string& jid, const std::string& msg);
  int go();
  void halt();
  void log(const std::string& message);

protected:
  void handleMessage(Stanza *stanza, MessageSession *session = 0);
  void handleLog(LogLevel level, LogArea area, const std::string& message);
  void onConnect();
  void onDisconnect(ConnectionError e);
  void onSessionCreateError(SessionCreateError info);
  bool onTLSConnect(const CertInfo& info);
  void handleVCard(const JID& jid, VCard *vcard);
  void handleVCardResult(VCardContext context, const JID& jid, StanzaError se = StanzaErrorUndefined);

private:
  Client *m_client;
  tHandler m_handler;
  int m_connecterror;
  std::ofstream m_log;
  VCardManager *m_vcardmgr;
  std::string m_photo_path;
  std::string m_photo_type;
};
