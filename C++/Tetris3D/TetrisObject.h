#include <stdlib.h>
#include <time.h>
//-------------------------------------------------------------------------------
// The tetris object is composed of 4 blocks, which are positioned based on the type of block it is.
// There are 7 total types: block, line, t-block, z-right, z-left, l-right, and l-left, and each 
// type has a corresponding integer value of _type. 
//-------------------------------------------------------------------------------
class TetrisObject
{
private:
	enum _objects { BLOCK, LINE, T_BLOCK, ZR_BLOCK, ZL_BLOCK, JR_BLOCK, JL_BLOCK } _type;
	int _pos0x;
	int _pos0y;
	int _pos1x;
	int _pos1y;
	int _pos2x;
	int _pos2y;
	int _pos3x;
	int _pos3y;
	enum _orientations { UP, RIGHT, DOWN, LEFT } _orientation;
public:
	TetrisObject();
	TetrisObject(int type, int pos0x, int pos0y);
	void Set_Coordinates( int _0x, int _0y );
	void Generate_Object( int x, int y );
	void Rotate_Left();
	void Rotate_Right();
	int Get_Type();
	int Get_Orientation();
	int Get_0x();
	int Get_0y();
	int Get_1x();
	int Get_1y();
	int Get_2x();
	int Get_2y();
	int Get_3x();
	int Get_3y();
};