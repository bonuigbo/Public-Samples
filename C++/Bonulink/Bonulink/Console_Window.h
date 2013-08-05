#ifndef CONSOLE_WINDOW
#define CONSOLE_WINDOW

#include "I_Window.h"

class Console_Window : public I_Window
{
public:
	void Print_Message( string message ) { cout << message; }
};


#endif