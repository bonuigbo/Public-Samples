#include "TetrisSimulator.h"

//---------------------------------------------------------------------------
// Constructor
//---------------------------------------------------------------------------
TetrisSimulator::TetrisSimulator()
{
	_object = new TetrisObject();
	_object->Set_Coordinates( 6, 19 );
	_next_object = new TetrisObject();
	_game_speed = 500;
	_lines = 0;
	_score = 0;
	_level = 0;
	_need_new_level = false;
	_game_over = false;
}

TetrisSimulator::~TetrisSimulator()
{
	delete _object;
	delete _next_object;
}

//---------------------------------------------------------------------------
// Deletes the old object and creates a new one
//---------------------------------------------------------------------------
void TetrisSimulator::Create_Object()
{
	delete _object;
	Free_Object();
	_object = _next_object;
	_object->Set_Coordinates( 6, 19 );
	_next_object = new TetrisObject();
	if( Object_Blocked_Below() )
	{
		_game_over = true;
	}
	Draw_Object();
}

void TetrisSimulator::Create_Object(int type, int x, int y )
{
	delete _object;
	Free_Object();
	_object = _next_object;
	_object->Set_Coordinates( 6, 19 );
	_next_object = new TetrisObject( type, x, y );
	Draw_Object();
}
//---------------------------------------------------------------------------
// Checks the object boundaries and redraws the object one square below
//---------------------------------------------------------------------------
void TetrisSimulator::Move_Down()
{
	if( !Object_Blocked_Below() && !_game_over)
	{
		Free_Object();
		_object->Set_Coordinates( _object->Get_0x(), _object->Get_0y() - 1 );
		Draw_Object();
	}
}
//---------------------------------------------------------------------------
// Checks the object boundaries and moves the objects to the left
//---------------------------------------------------------------------------
void TetrisSimulator::Move_Left()
{
	if( !Object_Blocked_Left() && !_game_over )
	{
		Free_Object();
		_object->Set_Coordinates( _object->Get_0x() - 1, _object->Get_0y() );
	}
	Draw_Object();
}
//---------------------------------------------------------------------------
// Checks the object boundaries and moves the objects to the right
//---------------------------------------------------------------------------
void TetrisSimulator::Move_Right()
{
	if( !Object_Blocked_Right() && !_game_over )
	{
		Free_Object();
		_object->Set_Coordinates( _object->Get_0x() + 1, _object->Get_0y() );
	}
	Draw_Object();
}

//---------------------------------------------------------------------------
// Rotates the object by calling the objects rotate function
//---------------------------------------------------------------------------
void TetrisSimulator::Rotate_Left()
{
	Free_Object();
	_object->Rotate_Left();
	if( Object_In_Wall() )
		_object->Rotate_Right();
	Draw_Object();
}
void TetrisSimulator::Rotate_Right()
{
	Free_Object();
	_object->Rotate_Right();
	if( Object_In_Wall() )
		_object->Rotate_Left();
	Draw_Object();
}
//---------------------------------------------------------------------------
// Drops the object rapidly
//---------------------------------------------------------------------------
void TetrisSimulator::Drop_Object()
{
	while( !Object_Blocked_Below() )
		Move_Down();
}
//---------------------------------------------------------------------------
// Draws the object to the world by activating all the spots on the world from the coordinates
//---------------------------------------------------------------------------
void TetrisSimulator::Draw_Object()
{
	_world.Draw_Spot( _object->Get_0x(), _object->Get_0y() );
	_world.Draw_Spot( _object->Get_1x(), _object->Get_1y() );
	_world.Draw_Spot( _object->Get_2x(), _object->Get_2y() );
	_world.Draw_Spot( _object->Get_3x(), _object->Get_3y() );

	_next_shape.Draw_Spot( _next_object->Get_0x(), _next_object->Get_0y() );
	_next_shape.Draw_Spot( _next_object->Get_1x(), _next_object->Get_1y() );
	_next_shape.Draw_Spot( _next_object->Get_2x(), _next_object->Get_2y() );
	_next_shape.Draw_Spot( _next_object->Get_3x(), _next_object->Get_3y() );
}
//---------------------------------------------------------------------------
// Frees the object from the world
//---------------------------------------------------------------------------
void TetrisSimulator::Free_Object()
{
	_world.Free_Spot( _object->Get_0x(), _object->Get_0y() );
	_world.Free_Spot( _object->Get_1x(), _object->Get_1y() );
	_world.Free_Spot( _object->Get_2x(), _object->Get_2y() );
	_world.Free_Spot( _object->Get_3x(), _object->Get_3y() );

	_next_shape.Free_Spot( _next_object->Get_0x(), _next_object->Get_0y() );
	_next_shape.Free_Spot( _next_object->Get_1x(), _next_object->Get_1y() );
	_next_shape.Free_Spot( _next_object->Get_2x(), _next_object->Get_2y() );
	_next_shape.Free_Spot( _next_object->Get_3x(), _next_object->Get_3y() );
}
//---------------------------------------------------------------------------
// If the objects cannot be spawned in the starting location, it is game over
//---------------------------------------------------------------------------
bool TetrisSimulator::Game_Over()
{
	return _game_over;
}
//---------------------------------------------------------------------------
// These methods check tosee if the object is able to move in the input direction, or to rotate
// in the proper direction, by checking the object coordinates against actively drawn squares
//---------------------------------------------------------------------------
bool TetrisSimulator::Object_Blocked_Below()
{
	Free_Object();
	bool blocked = false;
	if( _world.Get_Spot( _object->Get_0x(), _object->Get_0y() - 1 ) == 1 ||
		_world.Get_Spot( _object->Get_1x(), _object->Get_1y() - 1 ) == 1 ||
		_world.Get_Spot( _object->Get_2x(), _object->Get_2y() - 1 ) == 1 ||
		_world.Get_Spot( _object->Get_3x(), _object->Get_3y() - 1 ) == 1  )
	{
		blocked = true;
	}
	Draw_Object();
	return blocked;
}
bool TetrisSimulator::Object_Blocked_Right()
{
	Free_Object();
	bool blocked = false;
	if( _world.Get_Spot( _object->Get_0x() + 1, _object->Get_0y() ) == 1 ||
		_world.Get_Spot( _object->Get_1x() + 1, _object->Get_1y() ) == 1 ||
		_world.Get_Spot( _object->Get_2x() + 1, _object->Get_2y() ) == 1 ||
		_world.Get_Spot( _object->Get_3x() + 1, _object->Get_3y() ) == 1  )
	{
		blocked = true;
	}
	Draw_Object();
	return blocked;
}
bool TetrisSimulator::Object_Blocked_Left()
{
	Free_Object();
	bool blocked = false;
	if( _world.Get_Spot( _object->Get_0x() - 1, _object->Get_0y() ) == 1 ||
		_world.Get_Spot( _object->Get_1x() - 1, _object->Get_1y() ) == 1 ||
		_world.Get_Spot( _object->Get_2x() - 1, _object->Get_2y() ) == 1 ||
		_world.Get_Spot( _object->Get_3x() - 1, _object->Get_3y() ) == 1  )
	{
		blocked = true;
	}
	Draw_Object();
	return blocked;
}

bool TetrisSimulator::Object_In_Wall()
{
	bool wall = false;
	if( _world.Get_Spot( _object->Get_0x(), _object->Get_0y() ) == 1 ||
		_world.Get_Spot( _object->Get_1x(), _object->Get_1y() ) == 1 ||
		_world.Get_Spot( _object->Get_2x(), _object->Get_2y() ) == 1 ||
		_world.Get_Spot( _object->Get_3x(), _object->Get_3y() ) == 1  )
	{
		wall = true;
	}
	return wall;
}
//---------------------------------------------------------------------------
// Checks to see if the particular row is filled
//---------------------------------------------------------------------------
bool TetrisSimulator::Row_Filled(int j)
{
	bool row_filled = true;
	int width = _world.Get_Width();
	for( int i = 1; i < width - 1 ; i++ )
	{
		if( _world.Get_Spot( i, j ) == 0 )
			row_filled = false;
	}
	return row_filled;
}
//---------------------------------------------------------------------------
// Goes through each row and checks to see if the row is filled. If the row is filled, it empties
// the row, and increments the score counter, which then determines the score the player should
// earn.
//---------------------------------------------------------------------------
void TetrisSimulator::Remove_Rows()
{
	int removed_rows = 0;
	int curr_height = _world.Get_Height();
	for( int j = 1; j < curr_height - 1; j++ )
	{
		while( Row_Filled(j) )
		{
			Shift_Rows( j );
			_lines++;
			removed_rows++;
		}
	}
	switch ( removed_rows )
	{
	case 1:
		_score += 100;
		break;
	case 2:
		_score += 250;
		break;
	case 3:
		_score += 550;
		break;
	case 4:
		_score += 1000;
		break;
	}
}
//---------------------------------------------------------------------------
// This row takes the current row, and shifts all rows above the row downwards
//---------------------------------------------------------------------------
void TetrisSimulator::Shift_Rows( int j )
{
	int curr_width = _world.Get_Width();
	int curr_height = _world.Get_Height();
	for( int k = j; k < curr_height - 1; k++ )
	{
		for( int i = 1; i < curr_width - 1; i++ )
		{
			int curr_pixel = _world.Get_Spot( i, k+1 );
			if( curr_pixel == 1 )
				_world.Draw_Spot( i, k );
			else
				_world.Free_Spot( i, k );
		}
	}
}

void TetrisSimulator::Set_Game_Speed(int speed)
{
	_game_speed = speed;
}

void TetrisSimulator::Increase_Levels()
{
	if( _lines > 0 && _lines % 10 == 0 && _need_new_level == false )
	{
		_need_new_level = true;
		_level++;
		_game_speed -= _game_speed/2;
	}
	if ( _game_speed < 0 )
	{
		_game_speed = 0;
	}
	if( _lines > 0 && _lines % 10 > 0 && _lines % 10 < 9 )
	{
		_need_new_level = false;
	}
}
//---------------------------------------------------------------------------
// This function simulates the tetris game
//---------------------------------------------------------------------------
void TetrisSimulator::simulate()
{
	if( Object_Blocked_Below() && !_game_over )
	{
		_score += 13;
		Remove_Rows();
		Increase_Levels();
		Create_Object();
	}
	Move_Down();
}
//---------------------------------------------------------------------------
// Accesor methods
//---------------------------------------------------------------------------
TetrisWorld TetrisSimulator::Get_World()
{
	return _world;
}

Tetris_Next_Shape TetrisSimulator::Get_Next_Shape()
{
	return _next_shape;
}
TetrisObject* TetrisSimulator::Get_Object()
{
	return _object;
}

TetrisObject* TetrisSimulator::Get_Next_Object()
{
	return _next_object;
}

int TetrisSimulator::Get_Game_Speed()
{
	return _game_speed;
}

int TetrisSimulator::Get_Lines()
{
	return _lines;
}

int TetrisSimulator::Get_Score()
{
	return _score;
}

int TetrisSimulator::Get_Level()
{
	return _level;
}