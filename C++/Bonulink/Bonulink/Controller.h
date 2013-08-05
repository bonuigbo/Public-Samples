#include "Server.h"
#include "Console_Window.h"

class Controller
{
	Server _server;
	Client _client;
	Console_Window window;
public:
	Controller();
	~Controller();
	void Run();
	void Run_Server();
	void Run_Client();
	void Manage_Commands();
};

Controller::Controller()
{
}

Controller::~Controller()
{
}

//=====================================================================
// This function runs the console interface, first prompting the user to determine the 
// system type, and then running either in server mode or client mode.
//=====================================================================
void Controller::Run()
{
	cout << "===== Welcome to BonuLink.\n";
	cout << "===== Enter {0} for user mode, {1} for server mode.\n===== ";
	int option;
	cin >> option;
	switch(option)
	{
	case 0:
		cout << "===== User mode.\n";
		Run_Client();
		break;
	case 1:
		cout << "===== Server mode engaged.\n";
		Run_Server();
		break;
	default:
		cout << "===== Reenter a proper value.\n";
		break;
	}
}

void Controller::Run_Server()
{
	window.Print_Message( "===== Starting up the server connection.\n" );
	if ( _server.Start_WSA() )
	{
		if( _server.Create_Socket() )
		{
			window.Print_Message( "===== Server socket initialized.\n" );
			if( _server.Bind() )
			{
					window.Print_Message( "===== Server bound to port 20202.\n===== Waiting for connections...\n" );
				for(;;)
				{
					if( _server.Connect() )
					{
						window.Print_Message( "===== Connected to " + _server.Get_Remote_Address() + ':' + _server.Get_Remote_Port() + ".\n");
						window.Print_Message( _server.Print_Connections() );
					}
					window.Print_Message( _server.Manage_Connection() );
				}
				if( _server.Disconnect() )
					window.Print_Message("===== Server socket closed.\n");
			}
			else
				window.Print_Message( "===== Server binding failed\n" );
		}
		else
			window.Print_Message( "===== Initialization of server socket failed.\n" );
		window.Print_Message("===== Cleaning up the connection.\n");
		if ( !_server.Close_WSA() )
		{
			window.Print_Message("===== Cleanup failed.\n");
		}
	}
	else
		window.Print_Message("===== Startup failed.\n");

}

void Controller::Run_Client()
{
	window.Print_Message( "===== Starting up the client.\n" );
	if ( _client.Start_WSA() )
	{
		// Initialize the server socket to connect to
		if( _client.Create_Socket() )
		{
			window.Print_Message( "===== Client socket initialized.\n" );
			// Connect to the server socket
			if( _client.Connect() )
			{
				window.Print_Message( "===== Connected to " + To_String( _client.Get_Address() ) + ':' + To_String( _client.Get_Port() ) + ".\n");
				Manage_Commands();
				if( _client.Disconnect() )
					window.Print_Message("===== Client socket closed.\n");
			}
			else
				window.Print_Message("===== Connection to server failed.\n" );
			window.Print_Message("===== Cleaning up the connection.\n");
			if ( !_server.Close_WSA() )
			{
				window.Print_Message("===== Cleanup failed.\n");
			}
		}
		else
			window.Print_Message( "===== Initialization of client socket failed.\n" );
	}
	else
		window.Print_Message("===== Startup failed.\n");

}

void Controller::Manage_Commands()
{
	string command;
	window.Print_Message( "===== Enter a command.\n" );
	getline( cin, command );
	while(command != "EXIT" )
	{
		window.Print_Message( _client.Manage_Connection(command) );
		window.Print_Message( "===== Enter a command.\n" );
		getline(cin, command );	
	}
}