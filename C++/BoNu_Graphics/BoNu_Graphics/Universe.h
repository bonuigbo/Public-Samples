#ifndef _UNIVERSE_
#define _UNIVERSE_

#include "ObjectManager.h"
/*
		Every object is born and exists here.
*/
class Universe
{
	vector<VertexObject*> entities;
public:
	Universe(ObjectManager* manager);
	~Universe();
	vector<VertexObject*> Entities();
	void AddEntity(Entity*);
};
#endif