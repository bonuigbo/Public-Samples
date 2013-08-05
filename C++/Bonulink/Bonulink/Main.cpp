#include "Controller.h"

int main()
{
	Controller* _controller = new Controller();
	try
	{
		_controller->Run();
	}
	catch(Server_Error e)
	{
		cerr << "===== " << e.Get_Error() << '\n';
	}
	delete _controller;
	cin.get();
}