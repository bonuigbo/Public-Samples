#include "TetrisWorld.h"
#include "TetrisObject.h"
#include "Tetris_Next_Shape.h"


class TetrisSimulator
{
private:
	TetrisObject* _object;
	TetrisObject* _next_object;
	TetrisWorld _world;
	Tetris_Next_Shape _next_shape;
	int _game_speed;
	int _lines;
	int _score;
	int _level;
	bool _need_new_level;
	bool _game_over;

public:
	TetrisSimulator();
	~TetrisSimulator();
	bool Row_Filled( int j );
	void Remove_Rows();
	void Shift_Rows( int j );
	void Create_Object();
	void Create_Object( int type, int x, int y );
	void Move_Down();
	void Move_Left();
	void Move_Right();
	void Rotate_Left();
	void Rotate_Right();
	void simulate();
	void Draw_Object();
	void Free_Object();
	bool Game_Over();
	bool Object_Blocked_Below();
	bool Object_Blocked_Right();
	bool Object_Blocked_Left();
	bool Object_In_Wall();
	void Drop_Object();
	void TetrisSimulator::Increase_Levels();	
	TetrisWorld Get_World();
	Tetris_Next_Shape Get_Next_Shape();
	TetrisObject* Get_Object();
	TetrisObject* Get_Next_Object();
	int Get_Game_Speed();
	int Get_Lines();
	int Get_Score();
	int Get_Level();
	void Set_Game_Speed(int speed);
};
