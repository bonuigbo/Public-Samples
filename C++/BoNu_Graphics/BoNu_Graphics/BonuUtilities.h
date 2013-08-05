#ifndef BONUUTILITIES
#define BONUUTILITIES

#include <string>
#include <vector>
#include <math.h>
#include <map>
using namespace std;


class GraphicsError
{
	const wchar_t* message;
public:
	GraphicsError(const wchar_t* message) : message(message) {}
	const wchar_t* Message() const;
};

inline const wchar_t* GraphicsError::Message() const
{
	return message;
}

#endif