#ifndef DYNAMICBUFFER
#define DYNAMICBUFFER

#include <xnamath.h>
#include <d3d11.h>
#include <d3dx11.h>
#include <d3dcompiler.h>
#include "VertexObject.h"
#include <map>

struct ConstantBuffer
{
	XMMATRIX mWorld;
	XMMATRIX mView;
	XMMATRIX mProjection;
	XMFLOAT4 vOutputColor;
};
/*
		This is an adjustable buffer that will store all the different objects to be rendered as well as their
		positions within the buffer. Eventually it will manage removing unused items from the buffer
		as well

		NOTES:
		1. Ideally, any object would be added directly to the buffer, at any time, no ifs, ands or buts. That
		will be a longer term goal
*/

class DynamicBuffer
{
	/*
		object_list: A list of all the objects to be rendered. Need to remove from here
		type_dict: Each object has a string id, and their id is stored so that we arent adding
							multiple copies of the same object to the buffer
		buffer_index: This is the index within the buffer where the objects first vertex starts
		int num_of_vertices: The total number of vertices to render
	*/
private:

	vector<VertexObject*> object_list;
	map<string, int> type_dict;
	int buffer_index;
	int num_of_vertices;
public:
	Vertex* Vertices();
	ID3D11Buffer* p_vertex_buffer;
	ID3D11Buffer* p_const_buffer;	
	ID3D11DeviceContext* p_device_context; 
	ID3D11Device* p_d3d_device;
	D3D11_BUFFER_DESC	 vertex_buffer_desc, const_buffer_desc;
	D3D11_SUBRESOURCE_DATA vertex_sub_data, index_sub_data;

	DynamicBuffer(ID3D11DeviceContext* _p_device_context, ID3D11Device* _p_d3d_device);
	~DynamicBuffer();
	void CloseBuffers();
	void AddObject(VertexObject* object);
	void RemoveObject(VertexObject*);
	void RemoveObject(int object_id);
	void CreateNewBuffer();
	void InitializeNewBuffer(int size);
	void SetBuffer();
	void InitializeBuffers();
	void UpdateBuffers();
	vector<VertexObject*> Objects();
	void Reload();
	int NumberOfObjects();
	int NumberOfVertices();
	int Size();
	void CreateIndexBuffer(VertexObject*);
};

/*
		Constructor and deconstructor

*/

DynamicBuffer::DynamicBuffer(ID3D11DeviceContext* _p_device_context, ID3D11Device* _p_d3d_device)
{
	buffer_index = 0;
	num_of_vertices = 0;
	p_device_context = _p_device_context;
	p_d3d_device = _p_d3d_device;
}

DynamicBuffer::~DynamicBuffer()
{
	if( p_vertex_buffer )  p_vertex_buffer->Release();
	if( p_const_buffer )  p_const_buffer->Release();	
}

/*
		Releases the buffers;
*/
void DynamicBuffer::CloseBuffers()
{
	if( p_vertex_buffer ) 
	{
		p_vertex_buffer->Release();
		p_vertex_buffer = 0;
	}
	if( p_const_buffer ) 
	{
		p_const_buffer->Release();
		p_const_buffer = 0;
	}
}
/*
		Adds the object and a dictionary mapping
		the object to its type just incase it doesnt
		need to be renfered
*/
void DynamicBuffer::AddObject(VertexObject* object)
{
	object_list.push_back(object);
	this->num_of_vertices += object->Size();
	// Determine if that vertex is already in the buffer
	if( type_dict.count( object->Type() ) != 0 )
	{
		object->SetVertexIndex(type_dict[object->Type()]);
	}
	else
	{
		type_dict[object->Type()] = buffer_index;
		object->SetVertexIndex(buffer_index);
		buffer_index += object->Size();
		CreateNewBuffer();
		SetBuffer();
	}

}
/* 
	Gets an array holding all of the vertices in the buffer		
*/
Vertex* DynamicBuffer::Vertices()
{
	int vertices_index = 0;
	Vertex* vertices = new Vertex[this->num_of_vertices];
	for(unsigned int object_list_index = 0; object_list_index < this->object_list.size(); object_list_index++ )
	{		
		VertexObject* current_object = this->object_list[object_list_index];
		Vertex* current_vertices = current_object->Vertices();
		for(int current_vertices_index = 0; current_vertices_index < current_object->Size(); current_vertices_index++)
		{
			vertices[vertices_index] = current_vertices[current_vertices_index];
			vertices_index++;
		}
	}
	return vertices;
}
vector<VertexObject*> DynamicBuffer::Objects()
{
	return object_list;
}

/*
		This function creates a new buffer of a given size and sets these buffers to the new ones
*/
void DynamicBuffer::CreateNewBuffer()
{
	try
	{
		// Redefine all the buffer inco
		ID3D11Buffer* p_current_vertex_buffer;
		ID3D11Buffer* p_current_const_buffer;
		D3D11_BUFFER_DESC	 current_vertex_buffer_desc, current_const_buffer_desc;
		D3D11_SUBRESOURCE_DATA current_vertex_sub_data;

		// Set up the current_vertex buffer
		ZeroMemory( &current_vertex_buffer_desc, sizeof(current_vertex_buffer_desc) );
		current_vertex_buffer_desc.Usage = D3D11_USAGE_DYNAMIC;  // How often the resource will be updated
		current_vertex_buffer_desc.ByteWidth = sizeof( Vertex ) * this->num_of_vertices; // Initial size of buffer in bytes
		current_vertex_buffer_desc.BindFlags = D3D11_BIND_VERTEX_BUFFER; // Bind this as a current_vertex buffer
		current_vertex_buffer_desc.CPUAccessFlags = D3D11_CPU_ACCESS_WRITE;
		current_vertex_buffer_desc.MiscFlags = 0;

		// fill in the subresource data
		ZeroMemory( &current_vertex_sub_data, sizeof(current_vertex_sub_data) );
		current_vertex_sub_data.pSysMem = this->Vertices();

		// Create the current_vertex buffer
		p_d3d_device->CreateBuffer( &current_vertex_buffer_desc, &current_vertex_sub_data, &p_current_vertex_buffer );
	
		// Create the current_constant buffer
		ZeroMemory( &current_const_buffer_desc, sizeof(current_const_buffer_desc) );
		current_const_buffer_desc.Usage = D3D11_USAGE_DEFAULT;
		current_const_buffer_desc.ByteWidth = sizeof(ConstantBuffer);
		current_const_buffer_desc.BindFlags = D3D11_BIND_CONSTANT_BUFFER;
		current_const_buffer_desc.CPUAccessFlags = 0;
		p_d3d_device->CreateBuffer( &current_const_buffer_desc, NULL, &p_current_const_buffer );
		
		// Set the old buffers to the new one
		this->p_vertex_buffer= p_current_vertex_buffer;
		this-> p_const_buffer = p_current_const_buffer;
		this->const_buffer_desc = current_const_buffer_desc;
		this->vertex_buffer_desc =	 current_vertex_buffer_desc;
		this->vertex_sub_data =  current_vertex_sub_data;
	}
	catch(...)
	{
		throw GraphicsError(L"Method CreateNewBuffer failed");
	}
}
/*
		Sets the buffer up as a triange list
*/
void DynamicBuffer::SetBuffer()
{
	UINT stride = sizeof( Vertex );
    UINT offset = 0;
    p_device_context->IASetVertexBuffers( 0, 1, &p_vertex_buffer, &stride, &offset );
	p_device_context->IASetPrimitiveTopology( D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST );
	//p_device_context->IASetPrimitiveTopology( D3D11_PRIMITIVE_TOPOLOGY_POINTLIST );
}



/*
		Whenever an object has its core vertices updated, we want the buffer to account
		for those updates
*/

void DynamicBuffer::UpdateBuffers()
{
	try
	{
		D3D11_MAPPED_SUBRESOURCE mappedResource;
		p_device_context->Map(p_vertex_buffer, 0, D3D11_MAP_WRITE_DISCARD, 0, &mappedResource);
		Vertex* verticesPtr = (Vertex*)mappedResource.pData;
		memcpy(verticesPtr, (void*)this->Vertices(), (sizeof(Vertex) * this->NumberOfVertices()));	  
		p_device_context->Unmap(p_vertex_buffer, 0);
	}
	catch(...)
	{
		throw GraphicsError(L"Method UpdateBuffers failed");
	}
}

/*
		Removes an object from the buffer, adjusts the relative location of other objects
*/
void DynamicBuffer::RemoveObject(VertexObject* object)
{
	try
	{

		buffer_index -= object->Size();
		int current_index = object->VertexIndex();
		int current_size = object->Size();
		int current_id = object->Id();
		int object_position;
		// Iterate through the object list and remove the object
		for(unsigned int index = 0; index < object_list.size(); index++)
		{
			if (object_list[index]->Id() == current_id)
			{
				object_position = index;
			}
		}
		// Now delete the object and clear its memory
		delete object;
		object_list.erase(object_list.begin() + object_position);
	}
	catch(...)
	{
		throw GraphicsError(L"Method RemoveObject failed");
	}
}
int DynamicBuffer::NumberOfObjects()
{
	return object_list.size();
}
int DynamicBuffer::NumberOfVertices()
{
	return num_of_vertices;
}

#endif