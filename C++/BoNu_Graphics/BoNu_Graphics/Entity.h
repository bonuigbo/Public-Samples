#ifndef ENTITY
#define ENTITY

#include "VertexObject.h"

/*
		Entities are collection of objects and used mainly to apply matrix transformations
		to all the objects uniformly within a local world space
*/
class Entity : public ObjectState
{
private:
	map<string, VertexObject*> object_dict;
	vector<VertexObject*> objects;
	vector<string> object_ids;
public:
	Entity();
	~Entity();
	void AddPart(VertexObject*, string);
	VertexObject* Get(string id);
	vector<VertexObject*> Objects();
	vector<string> ObjectIds();
	map<string, VertexObject*> ObjectDict();
	virtual void InitializeEntity() = 0;
	virtual void Simulate() = 0;
	virtual void Translate(float x, float y, float z);
	virtual void Scale(float x, float y, float z);
	virtual void RotateX(float t);
	virtual void RotateY(float t);
	virtual void RotateZ(float t);
	virtual void MoveTo(float x, float y, float z);
};

Entity::Entity()
{

}

Entity::~Entity()
{

}

void Entity::AddPart(VertexObject* object, string id)
{
	object_dict[id] = object;
	objects.push_back(object);
	object_ids.push_back(id);
}

VertexObject* Entity::Get(string id)
{
	return object_dict[id];
}
/*
		Iterates through all the elements of the map and adds the parts of this
		Entity to a list, and then returns that list
*/
vector<VertexObject*> Entity::Objects()
{
	return objects;
}

map<string, VertexObject*> Entity::ObjectDict()
{
	return object_dict;
}
vector<string> Entity::ObjectIds()
{
	return object_ids;
}
/*
		When manipulating entities, all the objects must have their values updated uniformly
*/
void Entity::Translate(float x, float y, float z)
{
	position[0] += x; position[1] += y; position[2] += z;
	center_point[0] += x; center_point[1] += y; center_point[2] += z;
	for( map<string, VertexObject*>::iterator it =object_dict.begin(); it != object_dict.end(); it++)
	{
		float* f_transform = new float[16];
		XMMATRIX transform = XMMatrixTranslation(x, y, z);
		MapFrom(transform, f_transform);
		VertexObject* current_object = object_dict[(*it).first];
		current_object->State()->AddTransform(f_transform);
	}

}
void Entity::Scale(float x, float y, float z)
{
	dimensions[0] *= x; dimensions[1] *= y; dimensions[2] *= z;
	for( map<string, VertexObject*>::iterator it =object_dict.begin(); it != object_dict.end(); it++)
	{
		float* f_transform = new float[16];
		XMMATRIX transform = XMMatrixScaling(x, y, z);
		MapFrom(transform, f_transform);
		VertexObject* current_object = object_dict[(*it).first];
		current_object->State()->AddTransform(f_transform);
	}
}
void Entity::RotateX(float t)
{
	for( map<string, VertexObject*>::iterator it =object_dict.begin(); it != object_dict.end(); it++)
	{
		float* f_transform = new float[16];
		XMMATRIX transform = XMMatrixRotationX(t);
		MapFrom(transform, f_transform);
		VertexObject* current_object = object_dict[(*it).first];
		current_object->State()->AddTransform(f_transform);
	}
}
void Entity::RotateY(float t)
{
	for( map<string, VertexObject*>::iterator it =object_dict.begin(); it != object_dict.end(); it++)
	{
		float* f_transform = new float[16];
		XMMATRIX transform = XMMatrixRotationY(t);
		MapFrom(transform, f_transform);
		VertexObject* current_object = object_dict[(*it).first];
		current_object->State()->AddTransform(f_transform);
	}
}
void Entity::RotateZ(float t)
{
	for( map<string, VertexObject*>::iterator it =object_dict.begin(); it != object_dict.end(); it++)
	{
		float* f_transform = new float[16];
		XMMATRIX transform = XMMatrixRotationZ(t);
		MapFrom(transform, f_transform);
		VertexObject* current_object = object_dict[(*it).first];
		current_object->State()->AddTransform(f_transform);
	}
}
void Entity::MoveTo(float x, float y, float z)
{
	Translate( -position[0], -position[1], -position[2]);
	Translate(x, y, z);
}
#include "DefaultEntities.h"
#endif