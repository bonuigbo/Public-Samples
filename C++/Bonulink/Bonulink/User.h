#ifndef USER
#define USER

class User
{
	char* _name;
	char* _password;
public:
	User() : _name("default"), _password("default") {}
	User( char* name, char* password ) :
	_name(name), _password(password) {}
	~User() {}
	char* Get_Name() const { return _name; }
	char* Get_Password() const { return _password; }
	void Set_Name(char* name) { _name = name };
	void Set_Password(char* password) { _password = password; }
};

#endif