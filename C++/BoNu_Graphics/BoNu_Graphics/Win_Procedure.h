#ifndef WIN_PROCEDURE
#define WIN_PROCEDURE

#include "Windows.h"
#include "resource.h"

//====================================================================================================
// The controller class aids with processing the windows procedures for particualr instances
//====================================================================================================
class Controller
{
private:
	HWND _h_window;
	PAINTSTRUCT _paint_struct;
	HDC _hdc;
public:
	Controller( HWND h_window );
	~Controller();
	void Create() { }
	void Paint();
	void Command( WPARAM w_param, LPARAM l_param );

};
//====================================================================================================
// The controller is created with a handle to the particular window receiving the windows messages
//====================================================================================================
Controller::Controller( HWND h_window ) 
	: _h_window( h_window )
{
}
//====================================================================================================
// The controller destroyed with the window it controls
//====================================================================================================
Controller::~Controller()
{
	PostQuitMessage(0);
}
//====================================================================================================
// Called whenever the window is repainted
//====================================================================================================
void Controller::Paint()
{
	_hdc = BeginPaint( _h_window, &_paint_struct );
	EndPaint( _h_window, &_paint_struct );
}
//====================================================================================================
// The controller destroyed with the window it controls
//====================================================================================================
void Controller::Command( WPARAM w_param, LPARAM l_param )
{

}

LRESULT CALLBACK Win_Proc( HWND h_window, UINT message, WPARAM w_param, LPARAM l_param )
{
	Controller* p_controller = new Controller( h_window );
    switch( message )
    {
		case WM_CREATE:
			p_controller->Create();
			break;
		case WM_PAINT:
			p_controller->Paint();
			break;
        case WM_DESTROY:
			delete p_controller;
			break;
		case WM_COMMAND:
			break;
		case WM_LBUTTONDOWN:
			MessageBox(h_window, L"Test", L"Test", 0);
        default:
            return DefWindowProc( h_window, message, w_param, l_param );
    }

    return 0;
}

#endif