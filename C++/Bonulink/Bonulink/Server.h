#include "I_Socket.h"
#include "Client.h"
#include "Console_Window.h"


using namespace std;
//======================================================================
// This connection structure stores the remote socket, the current characters in the 
// sockets buffer, a buffer to store the received data, and the address of the socket
//======================================================================
struct Connection
{
	SOCKET _socket;
	char* _address;
	unsigned short _port;
	char _buffer[1024];
	int _chars_in_buffer;

	Connection() {}
	Connection(SOCKET socket) : _socket(socket), _chars_in_buffer(0) {}
	Connection(SOCKET socket, char* address, unsigned short port) : 
		_socket(socket), _address(address), _port(port), _chars_in_buffer(0) {}
};
//======================================================================
// This implements the socket interface to be more server oriented. It contains a list 
// of the clients connected to the server, as well as general information about the 
// sockets in general
//======================================================================
class Server : public I_Socket
{
private:
	vector<Connection> _client_list;
	char* _address;
	int _port;

	static const int WINSOCK_VER = 2;
	WSADATA _wsa_data;
	SOCKET _server_socket, _remote_socket;
	sockaddr_in _server_addr, _remote_addr;
public:
	Server() : _address(0), _port(20202), _server_socket(INVALID_SOCKET),
		_remote_socket(INVALID_SOCKET) {}

	Server( char* address, int port ) : _address(address), _port(port),
		_server_socket(INVALID_SOCKET), _remote_socket(INVALID_SOCKET) {}
	~Server() {}
	string Get_Remote_Address() const { return inet_ntoa(_remote_addr.sin_addr ); }
	string Get_Remote_Port() const { return To_String( ntohs(_remote_addr.sin_port) ); }
	bool Start_WSA();
	bool Close_WSA();
	bool Create_Socket();
	bool Bind();
	bool Connect();
	bool Disconnect();
	string  Manage_Connection();
	bool Send_Data( Connection& connection, char* data );
	bool Receive_Data(Connection& client);
	bool Receive_Data() { return false; }
	string Print_Connections();
	string Process_Command(Connection& connection);
	bool Run();
};

//======================================================================
// This function initializes the socket 
//======================================================================
bool Server::Create_Socket()
{
	_server_socket = socket( AF_INET, SOCK_STREAM, IPPROTO_TCP );
	if( _server_socket == INVALID_SOCKET )
	{
		return false;
	}
	unsigned long no_block = 1;
	ioctlsocket( _server_socket, FIONBIO, &no_block );
	return true;
}
//======================================================================
// This function binds the socket to the given address and port
//======================================================================
bool Server::Bind()
{
	_server_addr.sin_family = AF_INET;
	_server_addr.sin_port = htons(_port);
	_server_addr.sin_addr.S_un.S_addr = INADDR_ANY;
	if( bind(_server_socket, reinterpret_cast<sockaddr*>(&_server_addr), sizeof(_server_addr)) != 0 )
	{
		return false;
	}
	if( listen( _server_socket, SOMAXCONN ) != 0 )
	{
		return false;
	}
	return true;
}
//======================================================================
// This function connects to a remote socket if a request is made, otherwise it does
// nothing
//======================================================================
bool Server::Connect()
{
	int _remote_addr_len = sizeof( _remote_addr );
	_remote_socket = accept(_server_socket, reinterpret_cast<sockaddr*>(&_remote_addr), &_remote_addr_len );
	if( _remote_socket != INVALID_SOCKET )
	{
		Connection connection( _remote_socket, inet_ntoa( _remote_addr.sin_addr), _remote_addr.sin_port );
		unsigned long no_block = 1;
		ioctlsocket( _remote_socket, FIONBIO, &no_block );
		Send_Data(connection, "===== Welcome to the BonuServer!");
		_client_list.push_back( connection );
		return true;
	}
	return false;
}
//======================================================================
// This function closes the socket
//======================================================================
bool Server::Disconnect()
{
	return !closesocket(_server_socket);
}
//======================================================================
// This function processes each connection in the vector list, checking to see if the
// socket sent any commands to the server, and adding closed sockets to a list in
// order to be erased. It also processes commands
//======================================================================
string Server::Manage_Connection()
{
	string status = "";
	vector<Connection>::iterator it = _client_list.begin();
	vector<int> erase_positions;
	int erase_position = 0;
	while( it != _client_list.end() )
	{
		if ( Receive_Data(*it) )
		{
			status += "Socket " + To_String( it->_socket ) + " was closed. Shutting down.\n";
			erase_positions.push_back( erase_position );
		}
		if( it->_chars_in_buffer > 0 )
		{
			status += Process_Command( *it ) + '\n';
		}
		++it;
		++erase_position;
	}

	vector<int>::iterator e_it = erase_positions.begin();
	while( e_it != erase_positions.end() )
	{
		_client_list.erase( _client_list.begin() + *e_it );
		++e_it;
	}
	return status;
}
//======================================================================
// This function initializes WSA
//======================================================================
bool Server::Start_WSA()
{
	if ( WSAStartup( MAKEWORD( WINSOCK_VER, 0 ), &_wsa_data ) != 0)
	{
		throw Server_Error("Start up failed.\n");
		return false;
	}
	return true;
}
//======================================================================
// This function cleans up WSA
//======================================================================
bool Server::Close_WSA()
{
	if ( WSACleanup() != 0 )
	{
		throw Server_Error("===== Cleanup failed.\n");
		return false;
	}
	return true;
}
//======================================================================
// This function sends a string to the remote socket
//======================================================================
bool Server::Send_Data( Connection& connection, char* data )
{
	send( connection._socket, data, strlen(data),  0 );
	return true;
}

//======================================================================
// This function receives and prints data from the given socket port
//======================================================================
bool Server::Receive_Data(Connection& client)
{
	bool connection_closed = false;
	ZeroMemory( &client._buffer, sizeof(client._buffer) );
	client._chars_in_buffer = recv( client._socket, client._buffer, sizeof(client._buffer), 0 );
	if( client._chars_in_buffer == SOCKET_ERROR )
	{
		//No data sent by client
	}
	else if( client._chars_in_buffer == 0 )
	{
		connection_closed = true;
	}
	return connection_closed;
}
//======================================================================
// This function prints a list of all the sockets connected to the server
//======================================================================
string Server::Print_Connections()
{
	string message = "Client List:\n";
	vector<Connection>::iterator it = _client_list.begin();
	while( it != _client_list.end() )
	{
		message += To_String(it->_socket) + " -- " + To_String(it->_address) + ":" + To_String(it->_port) + "\n";
		++it;
	}
	return message;
}

string Server::Process_Command(Connection& connection)
{
	string command = connection._buffer;
	if( command == "SEND" )
	{
		Send_Data( connection, "Test of connection command." );
	}
	else if( command == "PRINT_CONNECTIONS" )
	{
		Send_Data( connection, const_cast<char*>( Print_Connections().c_str() ) );
		return Print_Connections();
	}
	return command;
}
