#include "TetrisObject.h"

//---------------------------------------------------------------------------
// Generates a random object at a fixed position.
//---------------------------------------------------------------------------
TetrisObject::TetrisObject()
{
	srand( time( NULL ) );
	_type = static_cast<_objects>( rand() % 7 );
	_orientation = UP;
	Generate_Object( 4, 4 );
}

//---------------------------------------------------------------------------
// Generates the object based on the type, position, and orientation, which is always up ( 0 )
// in this case. Mostly used for testing
//---------------------------------------------------------------------------
TetrisObject::TetrisObject(int type, int pos0x, int pos0y)
{
	_type = static_cast<_objects>( type );
	_orientation = UP;
	Generate_Object( pos0x, pos0y );
}

//---------------------------------------------------------------------------
// Calculates the coordinates of the tetris object based on the orientation of the object with 
// respect to the world. A very brute force style method
//---------------------------------------------------------------------------
void TetrisObject::Generate_Object( int x, int y )
{
	_pos0x = x;
	_pos0y = y;
	if( _orientation == UP )
	{
		switch( _type )
		{
			case BLOCK:
				_pos1x = _pos0x + 1;
				_pos1y = _pos0y;
				_pos2x = _pos0x;
				_pos2y = _pos0y - 1;
				_pos3x = _pos0x + 1;
				_pos3y = _pos0y - 1;
				break;
			case LINE:
				_pos1x = _pos0x - 1;
				_pos1y = _pos0y;
				_pos2x = _pos0x + 1;
				_pos2y = _pos0y;
				_pos3x = _pos0x + 2;
				_pos3y = _pos0y;
				break;
			case T_BLOCK:
				_pos1x = _pos0x - 1;
				_pos1y = _pos0y;
				_pos2x = _pos0x;
				_pos2y = _pos0y + 1;
				_pos3x = _pos0x + 1;
				_pos3y = _pos0y;
				break;
			case ZR_BLOCK:
				_pos1x = _pos0x - 1;
				_pos1y = _pos0y;
				_pos2x = _pos0x;
				_pos2y = _pos0y + 1;
				_pos3x = _pos0x + 1;
				_pos3y = _pos0y + 1;
				break;
			case ZL_BLOCK:
				_pos1x = _pos0x + 1;
				_pos1y = _pos0y;
				_pos2x = _pos0x;
				_pos2y = _pos0y + 1;
				_pos3x = _pos0x - 1;
				_pos3y = _pos0y + 1;
				break;
			case JR_BLOCK:
				_pos1x = _pos0x + 1;
				_pos1y = _pos0y ;
				_pos2x = _pos0x - 1;
				_pos2y = _pos0y;
				_pos3x = _pos0x + 1;
				_pos3y = _pos0y + 1;
				break;
			case JL_BLOCK:
				_pos1x = _pos0x + 1;
				_pos1y = _pos0y ;
				_pos2x = _pos0x - 1;
				_pos2y = _pos0y;
				_pos3x = _pos0x - 1;
				_pos3y = _pos0y + 1;
				break;
		}
	}
	else if( _orientation == RIGHT )
	{
		switch( _type )
		{
			case BLOCK:
				_pos1x = _pos0x + 1;
				_pos1y = _pos0y;
				_pos2x = _pos0x;
				_pos2y = _pos0y - 1;
				_pos3x = _pos0x + 1;
				_pos3y = _pos0y - 1;
				break;
			case LINE:
				_pos1x = _pos0x;
				_pos1y = _pos0y - 1;
				_pos2x = _pos0x;
				_pos2y = _pos0y + 1;
				_pos3x = _pos0x;
				_pos3y = _pos0y - 2;
				break;
			case T_BLOCK:
				_pos1x = _pos0x;
				_pos1y = _pos0y - 1;
				_pos2x = _pos0x + 1;
				_pos2y = _pos0y;
				_pos3x = _pos0x;
				_pos3y = _pos0y + 1;
				break;
			case ZR_BLOCK:
				_pos1x = _pos0x;
				_pos1y = _pos0y + 1;
				_pos2x = _pos0x + 1;
				_pos2y = _pos0y;
				_pos3x = _pos0x + 1;
				_pos3y = _pos0y - 1;
				break;
			case ZL_BLOCK:
				_pos1x = _pos0x;
				_pos1y = _pos0y + 1;
				_pos2x = _pos0x - 1;
				_pos2y = _pos0y;
				_pos3x = _pos0x - 1;
				_pos3y = _pos0y - 1;
				break;
			case JR_BLOCK:
				_pos1x = _pos0x;
				_pos1y = _pos0y + 1;
				_pos2x = _pos0x;
				_pos2y = _pos0y - 1;
				_pos3x = _pos0x + 1;
				_pos3y = _pos0y  - 1;
				break;
			case JL_BLOCK:
				_pos1x = _pos0x;
				_pos1y = _pos0y + 1;
				_pos2x = _pos0x;
				_pos2y = _pos0y - 1;
				_pos3x = _pos0x + 1;
				_pos3y = _pos0y + 1;
				break;
		}
	}
	else if(_orientation == DOWN )
	{
		switch( _type )
		{
			case BLOCK:
				_pos1x = _pos0x + 1;
				_pos1y = _pos0y;
				_pos2x = _pos0x;
				_pos2y = _pos0y - 1;
				_pos3x = _pos0x + 1;
				_pos3y = _pos0y - 1;
				break;
			case LINE:
				_pos1x = _pos0x - 1;
				_pos1y = _pos0y;
				_pos2x = _pos0x + 1;
				_pos2y = _pos0y;
				_pos3x = _pos0x + 2;
				_pos3y = _pos0y;
				break;
			case T_BLOCK:
				_pos1x = _pos0x - 1;
				_pos1y = _pos0y ;
				_pos2x = _pos0x;
				_pos2y = _pos0y - 1;
				_pos3x = _pos0x + 1;
				_pos3y = _pos0y;
				break;
			case ZR_BLOCK:
				_pos1x = _pos0x - 1;
				_pos1y = _pos0y;
				_pos2x = _pos0x;
				_pos2y = _pos0y + 1;
				_pos3x = _pos0x + 1;
				_pos3y = _pos0y + 1;
				break;
			case ZL_BLOCK:
				_pos1x = _pos0x + 1;
				_pos1y = _pos0y;
				_pos2x = _pos0x;
				_pos2y = _pos0y + 1;
				_pos3x = _pos0x - 1;
				_pos3y = _pos0y + 1;
				break;
			case JR_BLOCK:
				_pos1x = _pos0x + 1;
				_pos1y = _pos0y;
				_pos2x = _pos0x - 1;
				_pos2y = _pos0y;
				_pos3x = _pos0x - 1;
				_pos3y = _pos0y - 1;
				break;
			case JL_BLOCK:
				_pos1x = _pos0x + 1;
				_pos1y = _pos0y;
				_pos2x = _pos0x - 1;
				_pos2y = _pos0y;
				_pos3x = _pos0x + 1;
				_pos3y = _pos0y - 1;
				break;
		}
	}
	else if( _orientation = LEFT )
	{
		switch( _type )
		{
			case BLOCK:
				_pos1x = _pos0x + 1;
				_pos1y = _pos0y;
				_pos2x = _pos0x;
				_pos2y = _pos0y - 1;
				_pos3x = _pos0x + 1;
				_pos3y = _pos0y - 1;
				break;
			case LINE:
				_pos1x = _pos0x;
				_pos1y = _pos0y - 1;
				_pos2x = _pos0x;
				_pos2y = _pos0y + 1;
				_pos3x = _pos0x;
				_pos3y = _pos0y - 2;
				break;
			case T_BLOCK:
				_pos1x = _pos0x;
				_pos1y = _pos0y - 1;
				_pos2x = _pos0x - 1;
				_pos2y = _pos0y ;
				_pos3x = _pos0x;
				_pos3y = _pos0y + 1;
				break;
			case ZR_BLOCK:
				_pos1x = _pos0x;
				_pos1y = _pos0y + 1;
				_pos2x = _pos0x + 1;
				_pos2y = _pos0y;
				_pos3x = _pos0x + 1;
				_pos3y = _pos0y - 1;
				break;
			case ZL_BLOCK:
				_pos1x = _pos0x;
				_pos1y = _pos0y + 1;
				_pos2x = _pos0x - 1;
				_pos2y = _pos0y;
				_pos3x = _pos0x - 1;
				_pos3y = _pos0y - 1;
				break;
			case JR_BLOCK:
				_pos1x = _pos0x;
				_pos1y = _pos0y + 1;
				_pos2x = _pos0x;
				_pos2y = _pos0y - 1;
				_pos3x = _pos0x - 1;
				_pos3y = _pos0y + 1;
				break;
			case JL_BLOCK:
				_pos1x = _pos0x;
				_pos1y = _pos0y + 1;
				_pos2x = _pos0x;
				_pos2y = _pos0y - 1;
				_pos3x = _pos0x - 1;
				_pos3y = _pos0y - 1;
				break;
		}
	}
}
//---------------------------------------------------------------------------
// The next two methods rotate the object by changing the orientation, and then generating
// the new style of cube
//---------------------------------------------------------------------------
void TetrisObject::Rotate_Left()
{
	switch (_orientation)
	{
	case UP:
		_orientation = LEFT;
		break;
	case LEFT:
		_orientation = DOWN;
		break;
	case DOWN:
		_orientation = RIGHT;
		break;
	case RIGHT:
		_orientation = UP;
		break;
	}
	Generate_Object( _pos0x, _pos0y );
}
void TetrisObject::Rotate_Right()
{
	switch (_orientation)
	{
	case UP:
		_orientation = RIGHT;
		break;
	case RIGHT:
		_orientation = DOWN;
		break;
	case DOWN:
		_orientation = LEFT;
		break;
	case LEFT:
		_orientation = UP;
		break;
	}
	Generate_Object( _pos0x, _pos0y );
}
//---------------------------------------------------------------------------
// This method changes the coordinates of the object
//---------------------------------------------------------------------------
void TetrisObject::Set_Coordinates( int _0x, int _0y )
{
	_pos0x = _0x;
	_pos0y = _0y;
	Generate_Object( _pos0x, _pos0y );
}


//---------------------------------------------------------------------------
// Accesor methods
//---------------------------------------------------------------------------
int TetrisObject::Get_Type()
{
	return _type;
}
int TetrisObject::Get_Orientation()
{
	return _orientation;
}
int TetrisObject::Get_0x()
{
	return _pos0x;
}
int TetrisObject::Get_1x()
{
	return _pos1x;
}
int TetrisObject::Get_2x()
{
	return _pos2x;
}
int TetrisObject::Get_3x()
{
	return _pos3x;
}
int TetrisObject::Get_0y()
{
	return _pos0y;
}
int TetrisObject::Get_1y()
{
	return _pos1y;
}
int TetrisObject::Get_2y()
{
	return _pos2y;
}
int TetrisObject::Get_3y()
{
	return _pos3y;
}