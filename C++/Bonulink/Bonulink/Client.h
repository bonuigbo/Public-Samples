#include "I_Socket.h"
#include "Console_Window.h"


//======================================================================
// This class represents clients that intend to connect to and send commands to a
// central server
//======================================================================
using namespace std;
class Client : public I_Socket
{
private:
	char* _address;
	int _port;

	enum COMMANDS{ EXIT = 0, RECEIVE, SEND };
	static const int WINSOCK_VER = 2;
	WSADATA _wsa_data;
	SOCKET _socket;
	sockaddr_in _server_addr;
public:
	Client();
	~Client() {}
	char* Get_Address() const { return _address; }
	int Get_Port() const { return _port; }
	bool Start_WSA();
	bool Close_WSA();
	bool Create_Socket();
	bool Disconnect();
	bool Connect();
	string Manage_Connection(string);
	string Receive_Data();
	bool Send_Data( const char* data );
};
//======================================================================
// This function sets the address to connect to
//======================================================================
Client::Client()
{
	_address = "127.0.0.1";
	_port = 20202;
	_socket = INVALID_SOCKET;
}
//======================================================================
// This function initliazes the client socket
//======================================================================
bool Client::Create_Socket()
{
	_socket = socket( AF_INET, SOCK_STREAM, IPPROTO_TCP );
	if( _socket == INVALID_SOCKET )
	{
		throw Server_Error("Socket creation failed.\n");
		return false;
	}
	return true;
}
//======================================================================
// This function attempts to connect to the given server socket
//======================================================================
bool Client::Connect()
{
	_server_addr.sin_family = AF_INET;
	_server_addr.sin_port = htons(_port);
	_server_addr.sin_addr.S_un.S_addr = inet_addr(_address);
	if( connect(_socket, reinterpret_cast<sockaddr*>( &_server_addr ), sizeof( _server_addr ) ) != 0 )
	{
		return false;
	}
	unsigned long no_block = 1;
	ioctlsocket( _socket, FIONBIO, &no_block );
	return true;
	
}
//======================================================================
// This function prompts the user for a command, and processes the command based on
// what the user input
//======================================================================
string Client::Manage_Connection(string command)
{
	if( command == "EXIT" )
		return "";
	string phrase = Receive_Data();
	Send_Data( command.c_str() );
	return phrase;
}

//======================================================================
// This function closes the socket and disconnects
//======================================================================
bool Client::Disconnect()
{
	return !closesocket( _socket );
}
//======================================================================
// This function initializes WSA
//======================================================================
bool Client::Start_WSA()
{
	bool success = true;
	if ( WSAStartup( MAKEWORD( WINSOCK_VER, 0 ), &_wsa_data ) != 0)
	{
		success = false;
		throw Server_Error("Start up failed.\n");
	}
	return true;
}

bool Client::Close_WSA()
{
	bool success = true;
	if ( WSACleanup() != 0 )
	{
		success = false;
		throw Server_Error("===== Cleanup failed.\n");
	}
	return true;
}
//======================================================================
// This function recieves data from the connected server
//======================================================================
string Client::Receive_Data()
{
	string data = "";
	char buffer[128];
	ZeroMemory( &buffer, sizeof(buffer) );
	int bytes_received = recv( _socket, buffer, sizeof(buffer), 0 );
	//if( bytes_received == SOCKET_ERROR )

	data += string( buffer ) + '\n';
	return data;

}
bool Client::Send_Data( const char* data )
{
	send( _socket, data, strlen(data),  0 );
	return true;
}