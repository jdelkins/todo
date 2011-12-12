#include "botclient.hh"
#include <iostream>

extern "C" {
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/callback.h>

Botclient *g_client = 0;
value *g_callback = 0;

using namespace std;

// callback must have the caml type
// string -> string -> unit
void handle_message(const std::string& jid, const std::string& msg)
{
	CAMLparam0();
	CAMLlocal2(jid_val, msg_val);

	jid_val = copy_string(jid.c_str());
	msg_val = copy_string(msg.c_str());
	callback2(*g_callback, jid_val, msg_val);
	CAMLreturn0;
}

void send_message(value to, value msg)
{
	CAMLparam2(to, msg);

	g_client->sendMessage(String_val(to), String_val(msg));
	CAMLreturn0;
}

static int connect_and_run(
	const string& jid,
	const string& resource,
	const string& password,
	const string& photo_type,
	const string& photo_path,
	const value handler,
	const string& logfile,
	const string& server = "",
	const int port = -1)
{
	if (0 != g_callback) {
		remove_global_root(g_callback);
		// should I delete it, or does the caml GC handle that?
		delete g_callback;
	}
	g_callback = new value;
	register_global_root(g_callback);
	*g_callback = handler;
	
	if (0 != g_client) {
		delete g_client;
	}
	if (server == "") {
		g_client = new Botclient(jid, resource, password, handle_message, logfile);
	} else {
		g_client = new Botclient(jid, resource, password, handle_message, logfile, server, port);
	}

	if (!photo_path.empty()) {
		g_client->setPhoto(photo_type, photo_path);
	}
	return g_client->go();
}

// clientdata is (jid, resource, password, photo-mime-type, photo-path) quint
value run(value clientdata, value cback, value logfile)
{
	CAMLparam3(clientdata, cback, logfile);
	CAMLlocal1(rc);
	const string jid(String_val(Field(clientdata, 0)));
	const string resource(String_val(Field(clientdata, 1)));
	const string password(String_val(Field(clientdata, 2)));
	const string photo_type(String_val(Field(clientdata, 3)));
	const string photo_path(String_val(Field(clientdata, 4)));

	rc = Val_int(connect_and_run(jid, resource, password, photo_type, photo_path, cback, String_val(logfile)));
	CAMLreturn(rc);
}

// clientdata is (jid, resource, password, photo-mime-type, photo-path) quint
// serverport is (server, port) double
value run_server(value clientdata, value cback, value logfile, value serverport)
{
	CAMLparam4(clientdata, cback, logfile, serverport);
	CAMLlocal1(rc);
	const string jid(String_val(Field(clientdata, 0)));
	const string resource(String_val(Field(clientdata, 1)));
	const string password(String_val(Field(clientdata, 2)));
	const string photo_type(String_val(Field(clientdata, 3)));
	const string photo_path(String_val(Field(clientdata, 4)));
	const string server(String_val(Field(serverport, 0)));
	const int port = Int_val(Field(serverport, 1));

	rc = Val_int(connect_and_run(jid, resource, password, photo_type, photo_path, cback, String_val(logfile), server, port));
	CAMLreturn(rc);
}

void halt()
{
	CAMLparam0();
	g_client->halt();
	CAMLreturn0;
}

void logmessage(value message)
{
	CAMLparam1(message);
	g_client->log(String_val(message));
	CAMLreturn0;
}

} // extern "C"
