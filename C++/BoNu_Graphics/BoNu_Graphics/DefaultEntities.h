#ifndef DEFAULTENTITIES
#define DEFAULTENTITIES

/*
		Defines a standard block dude
		List of Things To Implement
		1. Stand
		2. Walk
		3. Run
		4. Jump
		5. Jumping Jacks
		6. Pose
*/
const float pi = 3.14159265f;

class BlockDude : public Entity
{
public:
	BlockDude();
	~BlockDude();
	void InitializeEntity();
	void Simulate();
	void Walk(float);
	void Run(float);
	void SwingArm1(float);
	void SwingArm2(float);
	void SwingHead(float);
	void MoveTorso(float);
	void BlockDude::TranslateAllObjects(float, float, float);
	void BlockDude::FinalizeState();
};

BlockDude::BlockDude() : Entity()
{	
	Cube* arm1 = new Cube();
	arm1->SetColor(BLUE);
	AddPart(arm1, "arm1");


	Cube* arm2 = new Cube();
	arm2->SetColor(BLUE);
	AddPart(arm2, "arm2");

	Cube* torso = new Cube();
	torso->SetColor(GREEN);
	AddPart(torso, "torso");

	Cube* leg1 = new Cube();
	leg1->SetColor(ORANGE);
	AddPart(leg1, "leg1");

	Cube* leg2 = new Cube();
	leg2->SetColor(ORANGE);
	AddPart(leg2, "leg2");

	Cube* head = new Cube();
	head->SetColor(VIOLET);
	AddPart(head, "head");

	InitializeEntity();
}

BlockDude::~BlockDude()
{
	
}

/*
		Eventually this functionality will be moved to the individual objects
		Initializes the sizes and the relative positons of the objects
*/
void BlockDude::InitializeEntity()
{
	VertexObject* arm = ObjectDict()["arm1"];
	arm->State()->Reset();
	arm->State()->Scale(1.0f, 3.0f, 1.0f);
	arm->State()->Translate(0.0f, -1.0f, 0.0f);

	VertexObject* arm2 = ObjectDict()["arm2"];
	arm2->State()->Reset();
	arm2->State()->Scale(1.0f, 3.0f, 1.0f);
	arm2->State()->Translate(0.0f, -1.0f, 0.0f);

	VertexObject* torso = ObjectDict()["torso"];
	torso->State()->Reset();
	torso->State()->Scale(2.0f, 3.0f, 1.0f);

	VertexObject* leg1 = ObjectDict()["leg1"];
	leg1->State()->Reset();
	leg1->State()->Scale(1.0f, 3.0f, 1.0f);
	leg1->State()->Translate(0.0f, -1.0f, 0.0f);

	VertexObject* leg2 = ObjectDict()["leg2"];
	leg2->State()->Reset();
	leg2->State()->Scale(1.0f, 3.0f, 1.0f);
	leg2->State()->Translate(0.0f, -1.0f, 0.0f);

	VertexObject* head = ObjectDict()["head"];
	head->State()->Reset();
	head->State()->Scale(1.5f, 1.5f, 1.5f);
}

/*
		Endlessly simulates the block dude
*/
void BlockDude::Simulate()
{
	static float t = 0.0f;
	static DWORD dwTimeStart = 0;
	DWORD dwTimeCur = GetTickCount();
	if( dwTimeStart == 0 )
		dwTimeStart = dwTimeCur;
	t = ( dwTimeCur - dwTimeStart ) / 1000.0f;

	InitializeEntity();

	SwingArm1(t);
	SwingArm2(t);
	MoveTorso(t);
	Walk(t);
	SwingHead(t);

	FinalizeState();
}

void BlockDude::Walk(float time)
{
	VertexObject* leg1 = ObjectDict()["leg1"];
	leg1->State()->RotateX(sin(time));

	VertexObject* leg2 = ObjectDict()["leg2"];
	leg2->State()->RotateX(cos(time));
}
void BlockDude::Run(float time)
{

}

void BlockDude::MoveTorso(float time)
{

}
void BlockDude::SwingArm1(float time)
{
	VertexObject* arm = ObjectDict()["arm1"];
	arm->State()->RotateZ(-pi/8);
	// Define the constants of this function
	static bool simulating = true;
	static float current_time = time;
	static float inc = -1.0f;

	
	if( time < 2 + current_time )
	{	
			arm->State()->RotateX( (pi/2)*time - (pi/2));
	}
	else
	{
		arm->State()->RotateX(time);
	}
}
void BlockDude::SwingArm2(float time)
{
	VertexObject* arm2 = ObjectDict()["arm2"];
	arm2->State()->RotateX(time);
}
void BlockDude::SwingHead(float time )
{
	VertexObject* head = ObjectDict()["head"];
	head->State()->RotateY(cos(2*time));
}

/*
		Moves all the objects into their final positions from their own world matrices
*/
void BlockDude::FinalizeState()
{
	VertexObject* arm = ObjectDict()["arm1"];
	arm->State()->Translate(-1.5, 1.0, 0.0);
	VertexObject* arm2 = ObjectDict()["arm2"];
	arm2->State()->Translate(1.5, 1.0, 0.0);
	VertexObject* head = ObjectDict()["head"];
	head->State()->Translate(0.0f, 2.25f, 0.0f);
	VertexObject* leg1 = ObjectDict()["leg1"];
	leg1->State()->Translate(-0.5, -2.0, 0.0);
	VertexObject* leg2 = ObjectDict()["leg2"];
	leg2->State()->Translate(0.5, -2.0, 0.0);
}
#endif