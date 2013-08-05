#include "Tetris_Next_Shape.h"

//--------------------------------------------------------------------------------
// Initialize all the spots on the tetris world to equal zero
//--------------------------------------------------------------------------------
Tetris_Next_Shape::Tetris_Next_Shape()
{
	for( int i = 0; i < width ; i++ )
	{
		for( int j = 0; j < height; j++ )
		{
			_spot[i][j] = free;
		}
	}
}
//--------------------------------------------------------------------------------
// Checks to see whether the spot is free or drawn
//--------------------------------------------------------------------------------
int Tetris_Next_Shape::Get_Spot( int i, int j )
{
	if( i >= 0 && i < ( width - 1 ) && j >= 0 && j < ( height ) )
		return _spot[i][j];
	return free;
}
//--------------------------------------------------------------------------------
// Draws a tile on the spot
//--------------------------------------------------------------------------------
void Tetris_Next_Shape::Draw_Spot( int i, int j )
{
	if( i >= 0 && i < ( width - 1 ) && j >= 0 && j < ( height ) )
		_spot[i][j] = drawn;
}
//--------------------------------------------------------------------------------
// Frees the block
//--------------------------------------------------------------------------------
void Tetris_Next_Shape::Free_Spot( int i, int j )
{
	if( i >= 0 && i < ( width - 1 ) && j >= 0 && j < ( height ) )
		_spot[i][j] = free;
}
//--------------------------------------------------------------------------------
// Gets the width and the height
//--------------------------------------------------------------------------------
const int Tetris_Next_Shape::Get_Width()
{
	return width;
}
const int Tetris_Next_Shape::Get_Height()
{
	return height;
}