/*
 * GmatSocketServer.h
 *
 *  Created on: Mar 22, 2011
 *      Author: Tuan Nguyen
 */

#ifndef GmatSocketServer_hpp
#define GmatSocketServer_hpp

#include <wx/event.h>

#ifdef LINUX_MAC
	#include <unistd.h>
	#include <pthread.h>

	#include <sys/time.h>
	#include <sys/select.h>
	#include <sys/types.h>
	#include <sys/socket.h>
	#include <netinet/in.h>
	#include <arpa/inet.h>
	#include <netdb.h>

	#define SOCKET_ERROR	-1
	#define INVALID_SOCKET	-1

#else
	#include <winsock2.h>
	#include <process.h>
#endif


#define IP_ADDRESS	"localhost"
#define TCP_PORT	3000


class GmatSocketServer
{
public:
	GmatSocketServer(wxEvtHandler* handler);
	virtual ~GmatSocketServer();

	void SetEventHandler(wxEvtHandler* handler) {evthandler = handler;}

#ifdef LINUX_MAC
	bool RunRequest(int sock);
#else
	bool RunRequest(SOCKET sock);
#endif
	char* OnRequest(char* item);
	bool OnPoke(char* data);

	void RunServer();
#ifdef LINUX_MAC
	void OnAccept(int sock);
	static void* StaticOnAccept(void* objPtr)
	{
		GmatSocketServer* pThis = (GmatSocketServer*)objPtr;
		pThis->OnAccept(pThis->client_sock);
		return NULL;
	}
	static void* StaticRunServer(void* objPtr)
	{
		GmatSocketServer* pThis = (GmatSocketServer*)objPtr;
		pThis->RunServer();
      return NULL;
	}
#else
	void OnAccept(SOCKET sock);
	static void StaticOnAccept(void* objPtr)
	{
		GmatSocketServer* pThis = (GmatSocketServer*)objPtr;
		pThis->OnAccept(pThis->client_sock);
	}
	static void StaticRunServer(void* objPtr)
	{
		GmatSocketServer* pThis = (GmatSocketServer*)objPtr;
		pThis->RunServer();
	}
#endif
private:
	int error;
	int m_numClients;
	bool shutdownserver;

	wxEvtHandler* evthandler;

#ifdef LINUX_MAC
	int client_sock;
#else
	SOCKET client_sock;
#endif
};

#endif /* GmatSocketServer_hpp */
