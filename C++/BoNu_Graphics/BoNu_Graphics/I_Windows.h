#ifndef I_WINDOWS
#define I_WINDOWS

#include "Win_Procedure.h"
#include <Windows.h>
/*
		Win Class deals with registering the windows class, and including any customized icons and menus
*/
class Win_Class
{
private:
	WNDCLASSEX _win_class;
public:
	Win_Class( WNDPROC win_proc, wchar_t* const class_name, HINSTANCE h_instance );
	void Set_Defaults();
	void Set_Icons( int icon_id );
	void Set_Color( int color );
	void Set_Cursor( int cursor );
	void Set_Menu( int menu );
	wchar_t* Get_Class_Name() const;
	HINSTANCE Get_HINSTANCE();
	void Register();
};

/*
		Sets default parameters for the windows class
*/
void Win_Class::Set_Defaults()
{
	_win_class.style = CS_HREDRAW | CS_VREDRAW;
    _win_class.cbClsExtra = 0;												//Additional memory to request for win_class
    _win_class.cbWndExtra = 0;												//Additional memory for the window 
	_win_class.hIcon = LoadIcon(NULL, IDI_APPLICATION);				//Type of Icon to Load
    _win_class.hCursor = LoadCursor( NULL, IDC_ARROW );				//default cursor
    _win_class.hbrBackground = reinterpret_cast<HBRUSH>( COLOR_WINDOW + 1 );	//default color, white
    _win_class.lpszMenuName = 0;											//The menu for the window
    _win_class.hIconSm = LoadIcon(NULL, IDI_APPLICATION);			//Small Icon
}
/*
		Initializes the windows class and sets appropriate defaults
*/
Win_Class::Win_Class(WNDPROC win_proc, wchar_t* const class_name, HINSTANCE h_instance )
{
	ZeroMemory( &_win_class, sizeof( _win_class ) );
	_win_class.cbSize = sizeof( WNDCLASSEX );
    _win_class.lpszClassName = class_name;								//Name of the class
	_win_class.hInstance = h_instance;									//create instance using WinMain() hInstance
	_win_class.lpfnWndProc = win_proc;									//Name of the windows procedure attached 
	Set_Defaults();
}
/*
		Sets customized icons from external resource files
*/
void Win_Class::Set_Icons( int icon_id )
{
	_win_class.hIcon = LoadIcon( _win_class.hInstance, MAKEINTRESOURCE( icon_id ) );
    _win_class.hIconSm = LoadIcon( _win_class.hInstance, MAKEINTRESOURCE( icon_id ) );
}
/*
		Sets customized icons from external resource files
*/
void Win_Class::Set_Color( int color )
{
	_win_class.hbrBackground = reinterpret_cast<HBRUSH>( color + 1 );
}
/*
		Sets customized cursor from external resource files
*/
void Win_Class::Set_Cursor( int cursor )
{
    _win_class.hCursor = LoadCursor( _win_class.hInstance, MAKEINTRESOURCE( cursor ) );
}
/*
		Sets customized menu from external resource files
*/
void Win_Class::Set_Menu( int menu )
{
    _win_class.lpszMenuName = MAKEINTRESOURCE( menu );
}
/*
		Registers the class
*/
void Win_Class::Register()
{
	RegisterClassEx( &_win_class );
}
/*
		Returns the instance registerd to this class
*/
inline HINSTANCE Win_Class::Get_HINSTANCE()
{
	return _win_class.hInstance;
}
/*
		Returns the class name
*/
inline wchar_t* Win_Class::Get_Class_Name() const
{
	return const_cast<wchar_t*>( _win_class.lpszClassName );
}
/*
		Window creates the actual window to be displayed
*/
class Window
{
private:
	wchar_t* _class_name;
	HWND _h_window;
	DWORD _ex_style;
	wchar_t const* _window_name;
	DWORD _win_style;
	int _x_pos;
	int _y_pos;
	int _width;
	int _height;
	HWND _h_parent;
	HMENU _h_menu;
	void* _data;
public:
	Window();
	Window( wchar_t* class_name, wchar_t* window_name );
	void Show( int cmd_show );
	void Create( HINSTANCE h_instance );
	void Create( HINSTANCE h_instance, int left, int top, int width, int height, int type );
	void Set_Size( int left = CW_USEDEFAULT, int top = CW_USEDEFAULT, int right = 640, int bottom = 480 );
	void Make_Default();
	void Make_Child( HWND h_parent );
	void Make_Combo_Box( HWND h_parent);
	void Add_String( wchar_t* option );
	HWND Get_HWND();
};
/*
		Initializes a blank Window
*/
Window::Window( )
	: _h_window(0),
	_class_name(0),
	_ex_style(0),											//extended window style
	_window_name(0),						//pointer to window name
	_win_style( 0 ),						//window style
	_x_pos( CW_USEDEFAULT ),							//horizontal position of window
	_y_pos( CW_USEDEFAULT ),							//verticle position of window
	_width( CW_USEDEFAULT ),							//width
	_height( CW_USEDEFAULT ),						//height
	_h_parent( 0 ),										//handle to  parent
	_h_menu(0),											//handle to menu or child_window
	_data(0)												//window creation data
{
}
/*
		Initializes Window with default, reasonable values
*/
Window::Window( wchar_t* class_name, wchar_t* window_name )
	: _h_window(0),
	_class_name( class_name ),
	_ex_style(0),											//extended window style
	_window_name( window_name ),						//pointer to window name
	_win_style( 0 ),						//window style
	_x_pos( CW_USEDEFAULT ),							//horizontal position of window
	_y_pos( CW_USEDEFAULT ),							//verticle position of window
	_width( CW_USEDEFAULT ),							//width
	_height( CW_USEDEFAULT ),						//height
	_h_parent( 0 ),										//handle to  parent
	_h_menu(0),											//handle to menu or child_window
	_data(0)												//window creation data
{
}
/*
		Sets the window to the default style
*/
void Window::Make_Default()
{
	_win_style = WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN;
}
/*
		Makes this window a child of another window
*/
void Window::Make_Child( HWND h_parent )
{
	_win_style = WS_BORDER | WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS;
	_h_parent = h_parent;
}
/*
		Turns this window into a combo box
*/
void Window::Make_Combo_Box( HWND h_parent )
{
	_win_style = WS_CHILD | WS_VISIBLE | WS_TABSTOP | CBS_DROPDOWNLIST | WS_VSCROLL;
	_h_parent = h_parent;
}

/*
		Shows and displays the window
*/
void Window::Show( int cmd_show )
{
	ShowWindow( _h_window, cmd_show );
	UpdateWindow( _h_window );
}
/*
		Sets the coordinates and the size of the window
*/
void Window::Set_Size( int x_pos, int y_pos, int width, int height  )
{
	_x_pos = x_pos;
	_y_pos = y_pos;
	_width = width;
	_height = height;
}
/*
		Creates the window with the given values
*/
void Window::Create( HINSTANCE h_instance )
{
	_h_window = CreateWindowEx(
		_ex_style,
		_class_name,
		_window_name,
		_win_style,
		_x_pos,
		_y_pos,
		_width,
		_height,
		_h_parent,
		_h_menu,
		h_instance,
		_data );
}
/*
		Returns the handle to this particular window
*/
HWND Window::Get_HWND()
{ 
	return _h_window;
}

#endif