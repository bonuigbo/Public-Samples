#ifndef I_WINDOW
#define I_WINDOW
#include <string>
#include <iostream>
#include <sstream>
using namespace std;


template <class T> inline std::string To_String( const T& t )
{
	std::stringstream ss;
	ss << t;
	return ss.str();
}

class I_Window
{
public:
	virtual void Print_Message( string message ) = 0;
};

#endif