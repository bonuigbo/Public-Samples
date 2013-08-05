#ifndef VERTEXOBJECT
#define VERTEXOBJECT

#include <xnamath.h>
#include "BonuUtilities.h"
#include "ObjectState.h"

/*
		Standard color values G B A R
*/
/*
const XMFLOAT4 RED = XMFLOAT4( 0.0f, 0.0f, 0.0f, 1.0f );
const XMFLOAT4 ORANGE = XMFLOAT4( 0.647f, 0.0f, 0.0f, 1.0f );
const XMFLOAT4 YELLOW = XMFLOAT4( 1.0f, 0.0f, 0.0f, 1.0f );
const XMFLOAT4 GREEN = XMFLOAT4( 1.0f, 0.0f, 0.0f, 0.0f );
const XMFLOAT4 BLUE = XMFLOAT4( 0.0f, 1.0f, 0.0f, 0.0f );
const XMFLOAT4 VIOLET = XMFLOAT4( 0.125f, 0.941f, 0.0, 0.627f );
const XMFLOAT4 WHITE = XMFLOAT4( 1.0f, 1.0f, 1.0f, 1.0f );
const XMFLOAT4 BLACK = XMFLOAT4( 0.0f, 0.0f, 0.0f, 1.0f );
const XMFLOAT4 BROWN = XMFLOAT4( 0.270f, 0.74f, 0.0f, 0.545f );
const XMFLOAT4 PINK = XMFLOAT4( 0.753f, 0.796f, 0.0, 1.0f );
*/

/*
		Standard color values R G B A for pixel shader
*/
const XMFLOAT4 RED = XMFLOAT4( 1.0f, 0.0f, 0.0f, 0.0f );
const XMFLOAT4 ORANGE = XMFLOAT4( 1.0f, 0.647f, 0.0f, 0.0f );
const XMFLOAT4 YELLOW = XMFLOAT4( 1.0f, 1.0f, 0.0f, 0.0f );
const XMFLOAT4 GREEN = XMFLOAT4( 0.0f, 1.0f, 0.0f, 0.0f );
const XMFLOAT4 BLUE = XMFLOAT4( 0.0f, 0.0f, 1.0f, 0.0f );
const XMFLOAT4 VIOLET = XMFLOAT4( 0.627f, 0.125f, 0.941f, 0.0f );
const XMFLOAT4 WHITE = XMFLOAT4( 1.0f, 1.0f, 1.0f, 0.0f );
const XMFLOAT4 BLACK = XMFLOAT4( 0.0f, 0.0f, 0.0f, 0.0f );
const XMFLOAT4 BROWN = XMFLOAT4( 0.545f, 0.270f, 0.74f, 0.0f );
const XMFLOAT4 PINK = XMFLOAT4( 1.0f, 0.753f, 0.796f, 0.0f );

/*
		Describes a Vertex based on its vertex_index and color
*/
struct Vertex
{
	XMFLOAT3 pos;
	XMFLOAT4 color;
	XMFLOAT2 texture;
	Vertex() 
		: pos( XMFLOAT3( 0.0f, 0.0f, 0.0f ) ), color( XMFLOAT4( 0.0f, 0.0f, 0.0f, 0.0f ) ),
		texture( XMFLOAT2( 0.0f, 0.0f) )
	{}
	Vertex( XMFLOAT3 vertex_index, XMFLOAT4 color ) :
		pos( vertex_index ), color ( color )
		{ 
		}
};
/*
		VertexObjects represent any object being rendered on the screen. It stores
		the list of vertices which define the object, the number of vertices, a state
		representing its vertex_index in the world, and an index representing the
		objects location within the buffer for rendering
*/
class VertexObject
{
	protected:
		static int id;
		vector<Vertex> vertex_list;
		string type;
		int size;
		ObjectState* state;
		int vertex_index;
		WORD* indices;
	public:
		VertexObject();
		~VertexObject();
		void AddVertex(Vertex vertex);
		void AddVertices(VertexObject* vertexObject);
		void AddVertices(Vertex* vertices, int size);
		void ClearVertices();
		XMFLOAT4 Color();
		int Size();
		string Type();
		void SetType(string);
		Vertex* Vertices();
		ObjectState* State();
		int VertexIndex();
		int Id();
		void SetVertexIndex(int);
		void SetColor(XMFLOAT4);
};

int VertexObject::id = 0;

VertexObject::VertexObject()
{
	indices = 0;
	id++;
	size = 0;
	type = "DYNAMIC";
	state = new ObjectState();
	vertex_index = 0;
}

VertexObject::~VertexObject()
{
	delete state;
	vertex_list.clear();
}

/*
		Adds an individual vertex to the vertex object
*/
void VertexObject::AddVertex(Vertex vertex)
{
	vertex_list.push_back(vertex);
	size++;
}

/*
		Appends all the vertice objects from another vertexObject
*/
void VertexObject::AddVertices(VertexObject* vertexObject)
{
	Vertex* vertices = vertexObject->Vertices();
	for(int i = 0; i < vertexObject->Size(); i++ )
	{
		AddVertex(vertices[i]);
	}
}

/*
		Adds the vertices from a list, with an int indicating the list's size
*/
void VertexObject::AddVertices(Vertex* vertices, int size)
{
	for( int i = 0; i < size; i++ )
	{
		AddVertex(vertices[i]);
	}
}

/* 
		Gets the Color of the object
*/
XMFLOAT4 VertexObject::Color()
{
	return vertex_list[0].color;
}
/*
		Gets the number of vertices in the object
*/
int VertexObject::Size()
{
	return size;
}

/*
		Gets the string identifying the object
*/
string VertexObject::Type()
{
	return type;
}

/*
	Sets the stirng identifying the object
*/
void VertexObject::SetType(string _type)
{
	this->type = _type;
}
/*
		Gets a pointer to all the vertices in the object
*/
Vertex* VertexObject::Vertices()
{
	Vertex* vertices = new Vertex[vertex_list.size()];
	for(unsigned int i = 0; i < vertex_list.size(); i++)
	{
		vertices[i] = vertex_list[i];
	}
	return vertices;
}
/*
		Returns the object's state
*/
ObjectState* VertexObject::State()
{
	return state;
}
/*
		Gets the object's index within the buffer
*/
int VertexObject::VertexIndex()
{
	return vertex_index;
}
/*
		Return the objects unique Id
*/
int VertexObject::Id()
{
	return id;
}
/*
		Sets the vertex's location within the buffer
*/
void VertexObject::SetVertexIndex(int pos)
{
	vertex_index = pos;
}
/*
		Remaps all of the color variables of each vertex in the list
*/
void VertexObject::SetColor(XMFLOAT4 color)
{
	for(unsigned int i = 0; i < vertex_list.size(); i++)
	{
		XMFLOAT3 current_position = vertex_list[i].pos;
		Vertex updatedVertex = Vertex( current_position, color);
		vertex_list[i] = updatedVertex;
	}
}
/*
		Enables simulation of the object within its own worldspace
*/
#include "DefaultShapes.h"
#endif