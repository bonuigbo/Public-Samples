#include "I_Windows.h"
#include "I_DirectX.h"
#include "Simulator.h"


int WINAPI wWinMain( HINSTANCE h_instance, HINSTANCE h_prev_instance,
	LPWSTR lp_cmd_line, int cmd_show )
{
	wchar_t* class_name = L"BoNu Graphics";
	wchar_t* window_name = L"BoNu Graphics";

	// Register the main window class, with the icons, mouse, and cursor
	Win_Class win_class( Win_Proc, class_name, h_instance );
	win_class.Set_Icons( IDI_ICON1 );
	win_class.Set_Cursor( IDI_CURSOR1 );
	win_class.Register();

	// create the main window
	Window main_window( class_name, window_name );
	main_window.Set_Size( 100, 30, 1200, 1000);
	main_window.Make_Default();
	main_window.Create( h_instance );

	// show the main window
	main_window.Show( cmd_show);
	MSG msg = {0};
	try
	{

		
		I_DirectX* graphics_engine = new I_DirectX();
		graphics_engine->InitializeDevice( main_window.Get_HWND() );
		graphics_engine-> InitializeBuffer();

		DynamicBuffer* buffer = graphics_engine->Buffer();		

		ObjectManager* world = new ObjectManager(buffer);
		Simulator* simulator = new Simulator(world);
		simulator->Simulate();


		// Main message loop

		while( WM_QUIT != msg.message )
		{
			if( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) )
			{
				TranslateMessage( &msg );
				DispatchMessage( &msg );
			}
			else
			{
				simulator->Simulate();
				graphics_engine->Render();
			}
		}
		delete simulator;
		delete graphics_engine;

	}
	catch(GraphicsError e)
	{
		MessageBox(main_window.Get_HWND(), (LPCWSTR)e.Message(), L"Graphics Error", 0 );
	}
	catch(DXError e)
	{
		MessageBox(main_window.Get_HWND(), (LPCWSTR)e.Get_Error(), L"DX Error", 0 );
	}
	return ( int )msg.wParam;
}
