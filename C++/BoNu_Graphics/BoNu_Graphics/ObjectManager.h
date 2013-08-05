#ifndef OBJECTMANAGER
#define OBJECTMANAGER

#include <vector>
#include "Entity.h"
#include "Buffer.h"
#include "BonuUtilities.h"

using namespace std;
/*
		This class manages the creation and deletion of objects on the screen being rendered

*/
class ObjectManager
{
	DynamicBuffer* buffer;
	const ObjectState DEFAULTSTATE;
	vector<VertexObject*> objects;
public:
	ObjectManager(DynamicBuffer* external_buffer) : 
	  buffer(external_buffer) {}
	~ObjectManager()
	{

	}
	void Remove(VertexObject*);
	void CreateDefaultState();
	VertexObject* AddObject(VertexObject*);
	vector<VertexObject*> Objects();
	Entity* AddEntity(Entity* entity);
	DynamicBuffer* Buffer()
	{
		return buffer;
	}

};

/*
		Places an object at a given location in the world
*/

VertexObject* ObjectManager::AddObject(VertexObject* object)
{
	buffer->AddObject(object);
	return object;
}
/*
		Function defines a cube 
*/


void ObjectManager::Remove(VertexObject* object)
{
	buffer->RemoveObject(object);
}


Entity* ObjectManager::AddEntity(Entity* entity)
{
	for(unsigned int i = 0; i < entity->Objects().size(); i++ )
	{
		buffer->AddObject(entity->Objects()[i]);
	}
	return entity;
}
#endif