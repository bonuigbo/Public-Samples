#ifndef I_SOCKET
#define I_SOCKET
#include <iostream>
#include <string>
#include <sstream>
#include <vector>
#define WIN32_MEAN_AND_LEAN
#include <winsock2.h>
#include <windows.h>
#include "Server_Error.h"
using namespace std;


//======================================================================
// This socket interface utilizes winsock, and contains functions based on that
//======================================================================
class I_Socket
{
public:
	virtual bool Start_WSA() = 0;
	virtual bool Close_WSA() = 0;
	virtual bool Create_Socket() = 0;
	virtual bool Disconnect() = 0;
	virtual bool Connect() = 0;
};

#endif