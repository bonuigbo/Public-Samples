
//--------------------------------------------------------------------------------
// Tetris World, where unoccupied spots are represented by a 0, and occupied spots are represented
// by a 1
//--------------------------------------------------------------------------------
class TetrisWorld
{
private:
	static const int width = 12;
	static const int height = 20;
	static const int free = 0;
	static const int drawn = 1;
	int _spot[width][height];


public:
	TetrisWorld();
	int Get_Spot( int i, int j );
	void Draw_Spot( int i, int j );
	void Free_Spot( int i, int j );
	const int Get_Width();
	const int Get_Height();
};