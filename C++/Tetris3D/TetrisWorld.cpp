#include "TetrisWorld.h"

//--------------------------------------------------------------------------------
// Initialize all the spots on the tetris world to equal zero
//--------------------------------------------------------------------------------
TetrisWorld::TetrisWorld()
{
	for( int i = 0; i < width ; i++ )
	{
		for( int j = 0; j < height; j++ )
		{
			if( i == 0 || i == ( width - 1 ) || j == 0 )
			{
				_spot[i][j] = drawn;
			}
			else
				_spot[i][j] = free;
		}
	}
}
//--------------------------------------------------------------------------------
// Checks to see whether the spot is free or drawn
//--------------------------------------------------------------------------------
int TetrisWorld::Get_Spot( int i, int j )
{
	if( i > 0 && i < ( width - 1 ) && j > 0 && j < ( height ) )
		return _spot[i][j];
	return drawn;
}
//--------------------------------------------------------------------------------
// Draws a tile on the spot
//--------------------------------------------------------------------------------
void TetrisWorld::Draw_Spot( int i, int j )
{
	if( i > 0 && i < ( width - 1 ) && j > 0 && j < ( height ) )
		_spot[i][j] = drawn;
}
//--------------------------------------------------------------------------------
// Frees the block
//--------------------------------------------------------------------------------
void TetrisWorld::Free_Spot( int i, int j )
{
	if( i > 0 && i < ( width - 1 ) && j > 0 && j < ( height ) )
		_spot[i][j] = free;
}
//--------------------------------------------------------------------------------
// Gets the width and the height
//--------------------------------------------------------------------------------
const int TetrisWorld::Get_Width()
{
	return width;
}
const int TetrisWorld::Get_Height()
{
	return height;
}