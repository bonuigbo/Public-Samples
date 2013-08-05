#ifndef SERVER_ERROR
#define SERVER_ERROR
//======================================================================
// This class is used to print and throw socket errors that may occur
//======================================================================
class Server_Error
{
	const char* _error_message;
public:
	Server_Error(const char* message) : _error_message(message) {}
	const char* Get_Error() const;
};

inline const char* Server_Error::Get_Error() const
{
	return _error_message;
}

#endif