#ifndef SIMULATOR
#define SIMULATOR

#include "ObjectManager.h"

/*
		This extensive class will deal with simulating the objects in the program
*/
class Simulator
{
	ObjectManager* manager;
	BlockDude* block_dude1;
	BlockDude* block_dude2;
	BlockDude* block_dude3;
	BlockDude* block_dude4;
	BlockDude* block_dude5;
	BlockDude* block_dude6;
	Octogon* octogon;

public:

	Simulator(ObjectManager* external) : manager(external)
	{

		CreateFloor();
		CreateBlockDude();
		//BlockTest();
		

	}
	~Simulator() { delete manager; }
	void Simulate()
	{

		BlockDudeTest();
	}

	void BlockTest();
	void CreateFloor();
	void CreateBlockDude();
	void BlockDudeTest();
};
void Simulator::BlockDudeTest()
{
    static float t = 0.0f;
	static DWORD dwTimeStart = 0;
	DWORD dwTimeCur = GetTickCount();
	if( dwTimeStart == 0 )
		dwTimeStart = dwTimeCur;
	t = ( dwTimeCur - dwTimeStart ) / 1000.0f;

	block_dude1->Simulate();
	block_dude1->Translate(-5.0f, 5.0f, 0.0f);
	block_dude2->Simulate();
	block_dude2->Translate(5.0f, 5.0f, 0.0f);
	block_dude3->Simulate();
	block_dude3->Translate(-10.0f, 5.0f, -5.0f);
	block_dude4->Simulate();
	block_dude4->Translate(10.0f, 5.0f, -5.0f);
	block_dude5->Simulate();
	block_dude5->Translate(-13.0f, 5.0f, -7.0f);
	block_dude6->Simulate();
	block_dude6->Translate(13.0f, 5.0f, -7.0f);
	octogon->State()->RotateY(0.01f);
	octogon->State()->MoveTo(5.0f, 10.0f, 0.00f);
}

void Simulator::CreateBlockDude()
{
	octogon = new Octogon();
	octogon->State()->Translate(0.0f, 10.0f, 0.0f);
	octogon->SetColor(YELLOW);
	manager->AddObject(octogon);
	block_dude1 = new BlockDude();
	manager->AddEntity(block_dude1);
	block_dude2 = new BlockDude();
	manager->AddEntity(block_dude2);
	block_dude3 = new BlockDude();
	manager->AddEntity(block_dude3);
	block_dude4 = new BlockDude();
	manager->AddEntity(block_dude4);
	block_dude5 = new BlockDude();
	manager->AddEntity(block_dude5);
	block_dude6 = new BlockDude();
	manager->AddEntity(block_dude6);
}
void Simulator::BlockTest()
{
	VertexObject* cube = new Cube();
	cube->State()->Translate( -6.0f, -0.0f, 0.0f);
	cube->SetColor(RED);
	manager->AddObject(cube);

	VertexObject* cube2 = new Cube();
	cube2->State()->Translate( -4.0f, -0.0f, 0.0f);
	cube2->SetColor(ORANGE);
	manager->AddObject(cube2);

	VertexObject* cube3 = new Cube();
	cube3->State()->Translate( -2.0f, -0.0f, 0.0f);
	cube3->SetColor(YELLOW);
	manager->AddObject(cube3);

	VertexObject* cube4 = new Cube();
	cube4->State()->Translate( 0.0f, -0.0f, 0.0f);
	cube4->SetColor(GREEN);
	manager->AddObject(cube4);

	VertexObject* cube5 = new Cube();
	cube5->State()->Translate( 2.0f, -0.0f, 0.0f);
	cube5->SetColor(BLUE);
	manager->AddObject(cube5);

	VertexObject* cube6 = new Cube();
	cube6->State()->Translate( 4.0f, -0.0f, 0.0f);
	cube6->SetColor(VIOLET);
	manager->AddObject(cube6);

	VertexObject* cube7 = new Cube();
	cube7->State()->Translate( 6.0f, -0.0f, 0.0f);
	cube7->SetColor(PINK);
	manager->AddObject(cube7);


	bool swapper = false;
	for(int i = -10; i < 10; i++ )
	{
		for( int j = -10; j < 10; j++)
		{
			VertexObject* current_cube= new Cube();
			current_cube->State()->Translate( static_cast<float>(i), -4.0f, static_cast<float>(j));
			swapper = !swapper;
			if(swapper == false)
			{
				current_cube->SetColor(BLUE);
			}
			else if(swapper == true)
			{
				current_cube->SetColor(WHITE);
			}
			manager->AddObject(current_cube);
		}
		swapper = !swapper;
	}
}
void Simulator::CreateFloor()
{
	
	VertexObject* floor = new Square();
	floor->State()->RotateX(3.14159265f/2.0f);
	floor->State()->Scale(100.0f, 0.0f, 100.0f);
	floor->State()->Translate(0.0f, -4.0f, 0.0f);
	manager->AddObject(floor);
	
	/*
	bool swapper = false;
	for(int i = -10; i < 10; i++ )
	{
		for( int j = -10; j < 10; j++)
		{
			VertexObject* current_square= new Square();
			current_square->State()->Scale(2.0f, 2.0f, 1.0f);
			current_square->State()->RotateX(3.14159265f/2.0f);
			current_square->State()->Translate( 2*static_cast<float>(i), -4.0f, 2*static_cast<float>(j));
			swapper = !swapper;
			if(swapper == false)
			{
				current_square->SetColor(BLUE);
			}
			else if(swapper == true)
			{
				current_square->SetColor(WHITE);
			}
			manager->AddObject(current_square);
		}
		swapper = !swapper;
	}
	*/
}
#endif