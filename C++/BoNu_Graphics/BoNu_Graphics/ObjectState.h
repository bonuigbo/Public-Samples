#ifndef OBJECTSTATE
#define OBJECTSTATE

#include <xnamath.h>
#include "BonuUtilities.h"
/*
		This class manages the movement of an object through its world-space via matrix transformations	

		Notes:
		1. This classes matrices could be improved significantly if the XMMATRIX could be stored directly
		2. The Matrix Transformations themselves are eventaully going to have to be separated out
		into a separate thread/physics engine
*/

class ObjectState
{
	/*

			Members:

			world: A matrix representing the objects current worldspace
			dimensions: The length, width, and height of the object
			position: The current position of the object's origin relative to (0,0,0)
			center_point: The location of the center of the object relative to (0,0,0)
			universe: The location of the object based on another object's worldspace
			transforms: A list storing the matrices of all the transforms done on this object
								by subsequent objects
	*/
protected:
	float world[16];
	float dimensions[3];
	float position[16];
	float center_point[3];
	float universe[16];
	float angles[3];
	vector<float*> transforms;
public:
	ObjectState();
	~ObjectState();
	XMMATRIX& World();
	float* Position();
	virtual void Translate(float x, float y, float z);
	virtual void Scale(float x, float y, float z);
	virtual void RotateX(float t);
	virtual void RotateY(float t);
	virtual void RotateZ(float t);
	virtual void MoveTo(float x, float y, float z);
	void TranslateToLocalState();
	void Reset();
	float* Angles();
	void MapFrom(XMMATRIX& matrix);
	void MapTo(XMMATRIX& matrix );
	void MapFrom(XMMATRIX&, float*);
	void MapTo(XMMATRIX&, float*);
	void SetDimensions(float, float, float);
	float* Dimensions();
	void TranslateToOrigin();
	void AddTransform(float*);
	vector<float*> Transforms();
	void ResetTransforms();
	XMMATRIX& Universe();
	void MapToUniverse(XMMATRIX&);
};

/*
		Sets the worldspace to the identity matrix, and the local
		centerpoint to 0,0,0
*/
ObjectState::ObjectState()
{
	XMMATRIX matrix = XMMatrixIdentity();
	MapFrom(matrix);
	XMMATRIX tmatrix = XMMatrixTranslation(0.0f, 0.0f, 0.0f);
	center_point[0] =0.0f; 	center_point[1] =0.0f; 	center_point[2] =0.0f;
	angles[0] = angles[1] = angles[2] = 0.0f;
	MapFrom(tmatrix, position);
}

ObjectState::~ObjectState()
{

}
/*
		Removes all modifications to the original vertices of the object
*/
void ObjectState::Reset()
{
	XMMATRIX matrix = XMMatrixIdentity();
	MapFrom(matrix);
	XMMATRIX tmatrix = XMMatrixTranslation(0.0f, 0.0f, 0.0f);
	center_point[0] =0.0f; 	center_point[1] =0.0f; 	center_point[2] =0.0f;
	//angles[0] = angles[1] = angles[2] = 0.0f;
	MapFrom(tmatrix, position);
}
/*
		Returns the worldspace matrix
*/
XMMATRIX& ObjectState::World()
{
	XMMATRIX world_m = XMMatrixIdentity();
	MapTo(world_m, world);
	return world_m;
}

/*
		Returns the position of the objects center relative to 0,0,0
*/
float* ObjectState::Position()
{
	return position;
}

/*
		Moves the object from its current position to a new destination,
		and updates the position of the object's center accordingly
*/
void ObjectState::Translate(float x, float y, float z)
{
	position[0] += x; position[1] += y; position[2] += z;
	center_point[0] += x; center_point[1] += y; center_point[2] += z;
	XMMATRIX matrix = XMMatrixIdentity();
	MapTo(matrix);
	matrix = matrix * XMMatrixTranslation(x, y, z);
	MapFrom(matrix);
}

/*
		Translates the object back to 0,0,0, then to the given destination
*/
void ObjectState::MoveTo(float x, float y, float z)
{
	Translate( -center_point[0], -center_point[1], -center_point[2]);
	Translate(x, y, z);
}
/*
		Scales the object in the relative dimensions
*/
void ObjectState::Scale(float x, float y, float z)
{
	dimensions[0] *= x;
	dimensions[1] *= y;
	dimensions[2] *= z;
	XMMATRIX matrix = XMMatrixIdentity();
	MapTo(matrix);
	matrix = matrix * XMMatrixScaling(x, y, z);
	MapFrom(matrix);
}

/*
		Rotates the object around the X axis	 by the given number of radians
*/
void ObjectState::RotateX(float t)
{
	XMMATRIX matrix = XMMatrixIdentity();
	MapTo(matrix);
	matrix = matrix * XMMatrixRotationX(t);
	angles[0] += t;
	MapFrom(matrix);
}
/*
		Rotates the object around the Y axis	 by the given number of radians
*/
void ObjectState::RotateY(float t)
{
	XMMATRIX matrix = XMMatrixIdentity();
	MapTo(matrix);
	matrix = matrix * XMMatrixRotationY(t);
	angles[1] += t;
	MapFrom(matrix);
}
/*
		Rotates the object around the Z axis	 by the given number of radians	
*/
void ObjectState::RotateZ(float t)
{
	XMMATRIX matrix = XMMatrixIdentity();
	MapTo(matrix);
	matrix = matrix * XMMatrixRotationZ(t);
	angles[2] += t;
	MapFrom(matrix);
}
/*
		Maps from the DirectX defined matrix to a local one
*/
void ObjectState::MapFrom(XMMATRIX& matrix)
{
	world[0] = matrix._11;
	world[1] = matrix._12;	
	world[2] = matrix._13;
	world[3] = matrix._14;
	world[4] = matrix._21;
	world[5] = matrix._22;
	world[6] = matrix._23;
	world[7] = matrix._24;
	world[8] = matrix._31;
	world[9] = matrix._32;
	world[10] = matrix._33;
	world[11] = matrix._34;
	world[12] = matrix._41;
	world[13] = matrix._42;
	world[14] = matrix._43;
	world[15] = matrix._44;
}
void ObjectState::MapTo(XMMATRIX& matrix )
{
	matrix._11 = world[0];
	matrix._12 = world[1];
	matrix._13 = world[2];
	matrix._14 = world[3];
	matrix._21 = world[4];
	matrix._22 = world[5];
	matrix._23 = world[6];
	matrix._24 = world[7];
	matrix._31 = world[8];
	matrix._32 = world[9];
	matrix._33 = world[10];
	matrix._34 = world[11];
	matrix._41 = world[12];
	matrix._42 = world[13];
	matrix._43 = world[14];
	matrix._44 = world[15];
}

void ObjectState::MapFrom(XMMATRIX& matrix, float* values)
{
	values[0] = matrix._11;
	values[1] = matrix._12;	
	values[2] = matrix._13;
	values[3] = matrix._14;
	values[4] = matrix._21;
	values[5] = matrix._22;
	values[6] = matrix._23;
	values[7] = matrix._24;
	values[8] = matrix._31;
	values[9] = matrix._32;
	values[10] = matrix._33;
	values[11] = matrix._34;
	values[12] = matrix._41;
	values[13] = matrix._42;
	values[14] = matrix._43;
	values[15] = matrix._44;
}
void ObjectState::MapTo(XMMATRIX& matrix, float* values )
{
	matrix._11 = values[0];
	matrix._12 = values[1];
	matrix._13 = values[2];
	matrix._14 = values[3];
	matrix._21 = values[4];
	matrix._22 = values[5];
	matrix._23 = values[6];
	matrix._24 = values[7];
	matrix._31 = values[8];
	matrix._32 = values[9];
	matrix._33 = values[10];
	matrix._34 = values[11];
	matrix._41 = values[12];
	matrix._42 = values[13];
	matrix._43 = values[14];
	matrix._44 = values[15];
}
/*
		The dimensions of the object are stored to allow referencing
		when positioning
*/
void ObjectState::SetDimensions(float width, float height, float length)
{
	dimensions[0] = width;
	dimensions[1] = height;
	dimensions[2] = length;
}
/*
		Returns the dimensions
*/
float*  ObjectState::Dimensions()
{
	return dimensions;
}

/*
		Moves the object so that its loweest -x, -y, -z  index starts at 0, 0, 0
*/
void ObjectState::TranslateToOrigin()
{
	MoveTo(dimensions[0]/2.0f, dimensions[1]/2.0f, dimensions[2]/2.0f);
}
/*
		Adds the matrix transform to this object's
*/
void ObjectState::AddTransform(float* entity_transform)
{
	transforms.push_back(entity_transform);
}
/*
		Returns a list of all the transform matrices
*/
vector<float*> ObjectState::Transforms()
{
	return transforms;
}
/*
		Clears out the list of transform matrices
*/
void ObjectState::ResetTransforms()
{
	// Since they are pointers, make sure to clear their memory
	for(unsigned int i = 0; i < transforms.size(); i++ )
	{
		delete transforms[i];
	}
	transforms.clear();
}
/*
		Applies all the matrix transforms to the object, and returns that local worldspace
*/
XMMATRIX& ObjectState::Universe()
{

		XMMATRIX universe = XMMatrixIdentity();
		MapTo(universe);		
		
		XMMATRIX transform = XMMatrixIdentity();
		for(unsigned int i = 0; i < Transforms().size(); i++ )
		{
			
			MapTo(transform, Transforms()[i]);	
			universe *= transform;
			
		}
		ResetTransforms();
		return universe;

}
/*
		Ugh
*/
void ObjectState::MapToUniverse(XMMATRIX& matrix)
{
	MapTo(matrix, universe);
}

float* ObjectState::Angles()
{
	return angles;
}
#endif

