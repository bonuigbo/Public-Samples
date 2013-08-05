#ifndef I_DIRECTX
#define I_DIRECTX


// General Directx Interface. Any updates can go here

#include <xnamath.h>
#include <d3d11.h>
#include <d3dx11.h>
#include <d3dcompiler.h>
#include "Buffer.h"
#include <math.h>

/*
		Standard exception class which throws whenever a function from the DX interface doesn't work properly
*/

class DXError
{
	wchar_t* const _error;
public:
	DXError( wchar_t* error ) : _error( error )
	{
	}
	wchar_t* const Get_Error() const
	{
		return _error;
	}
};


/*
		This class handles initializing and maintaining all the DirectX related devices
*/

class I_DirectX
{
	// Structures
	struct ConstantBuffer
	{
		XMMATRIX mWorld;
		XMMATRIX mView;
		XMMATRIX mProjection;
		XMFLOAT4 vOutputColor;
	};
	
	// Typical device items to be initialized
	D3D_DRIVER_TYPE				driver_type;
	D3D_FEATURE_LEVEL			feature_level;
	ID3D11Device*				device;
	ID3D11DeviceContext*		device_context;
	IDXGISwapChain*				p_swap_chain;
	ID3D11RenderTargetView*	p_render_target_view;
	ID3D11Texture2D*			p_depth_stencil;
	ID3D11DepthStencilView*		p_depth_stencil_view;
	ID3D11VertexShader*		vertex_shader;
	ID3D11PixelShader*			pixel_shader;
	ID3D11InputLayout*			p_vertex_layout;

	DynamicBuffer* dynamic_buffer;
	XMMATRIX					_view;
	XMMATRIX					_projection;

public:
	I_DirectX();
	~I_DirectX();
	void CreateSwapChain(HWND h_window);
	void SetViewAndProjectionMatrices(int width, int height);
	void InitializeDevice( HWND h_window );
	void InitializeBuffer();
	void SetBuffer();
	void Render();
	void DrawObject(VertexObject* current_object);
	void Set_View_Matrix();
	HRESULT Compile_Shader_From_File( WCHAR* sz_file_name,
		LPCSTR sz_entry_point, LPCSTR sz_shader_model, ID3DBlob** pp_blob_out );
	ID3D11Device* Device();
	ID3D11DeviceContext* DeviceContext();
	DynamicBuffer* Buffer()
	{
		return dynamic_buffer;
	}
};

/* 
		Constructor initializes the types
*/
I_DirectX::I_DirectX()
{
	
	driver_type = D3D_DRIVER_TYPE_NULL;
	feature_level = D3D_FEATURE_LEVEL_11_0;
	device = NULL;
	device_context = NULL;
	p_swap_chain = NULL;
	p_render_target_view = NULL;
	p_depth_stencil = NULL;
	p_depth_stencil_view = NULL;
	vertex_shader = NULL;
	pixel_shader = NULL;
	p_vertex_layout = NULL;
}

/* 
		Releases all the memory held by the devices
*/
I_DirectX::~I_DirectX()
{

    if( device_context ) device_context->ClearState();
	if( p_vertex_layout ) p_vertex_layout->Release();
	if( vertex_shader ) vertex_shader->Release();
	if( pixel_shader ) pixel_shader->Release();
    if( p_render_target_view ) p_render_target_view->Release();
	if( p_depth_stencil ) p_depth_stencil->Release();
	if( p_depth_stencil_view ) p_depth_stencil_view->Release();
    if( p_swap_chain ) p_swap_chain->Release();
    if( device_context ) device_context->Release();
    if( device ) device->Release();
}

/*
		This function initializes all the basic functions and devices such as the hardware feature level, the device
		context, and the various shaders
*/
void I_DirectX::InitializeDevice( HWND h_window )
{
	try
	{
		HRESULT hr = S_OK;

		// Currently rendering this onto the entire screen of the window. Window management is inevitable
		
		RECT rc;
		GetClientRect( h_window, &rc );
		UINT width = rc.right - rc.left;
		UINT height = rc.bottom - rc.top;

		CreateSwapChain(h_window);

		// Create a render target view

		ID3D11Texture2D* pBackBuffer = NULL;
		hr = p_swap_chain->GetBuffer( 0, __uuidof( ID3D11Texture2D ), ( LPVOID* )&pBackBuffer );
		if( FAILED( hr ) )
		{
			throw DXError( L"The back buffer failed to initialize." );
		}

		hr = device->CreateRenderTargetView( pBackBuffer, NULL, &p_render_target_view );
		pBackBuffer->Release();
		if( FAILED( hr ) )
		{
			throw DXError( L"The render target view failed to initialize." );
		}

		// Create the depth stencil texture

		D3D11_TEXTURE2D_DESC desc_depth;
		ZeroMemory( &desc_depth, sizeof(desc_depth) );
		desc_depth.Width = width;
		desc_depth.Height = height;
		desc_depth.MipLevels = 1;
		desc_depth.ArraySize = 1;
		desc_depth.Format = DXGI_FORMAT_D24_UNORM_S8_UINT;
		desc_depth.SampleDesc.Count = 1;
		desc_depth.SampleDesc.Quality = 0;
		desc_depth.Usage = D3D11_USAGE_DEFAULT;
		desc_depth.BindFlags = D3D11_BIND_DEPTH_STENCIL;
		desc_depth.CPUAccessFlags = 0;
		desc_depth.MiscFlags = 0;
		hr = device->CreateTexture2D( &desc_depth, NULL, &p_depth_stencil );
		if( FAILED( hr ) )
		{	
			throw DXError( L"The depth stencil texture failed to initialize." );
		}

		// Create the depth stencil view

		D3D11_DEPTH_STENCIL_VIEW_DESC desc_dsv;
		ZeroMemory( &desc_dsv, sizeof(desc_dsv) );
		desc_dsv.Format = desc_depth.Format;
		desc_dsv.ViewDimension = D3D11_DSV_DIMENSION_TEXTURE2D;
		desc_dsv.Texture2D.MipSlice = 0;
		hr = device->CreateDepthStencilView( p_depth_stencil, &desc_dsv, &p_depth_stencil_view );
		if( FAILED( hr ) )
		{
			throw DXError( L"The depth stencil texture failed to initialize." );
		}

		device_context->OMSetRenderTargets( 1, &p_render_target_view, p_depth_stencil_view );

		// Setup the viewport

		D3D11_VIEWPORT vp;
		vp.Width = (FLOAT)width;
		vp.Height = (FLOAT)height;
		vp.MinDepth = 0.0f;
		vp.MaxDepth = 1.0f;
		vp.TopLeftX = 0;
		vp.TopLeftY = 0;
		device_context->RSSetViewports( 1, &vp );

		// Compile the vertex shader

		ID3DBlob* pVSBlob = NULL;
		hr = Compile_Shader_From_File( L"Shaders.fx", "VS", "vs_4_0", &pVSBlob );
		if( FAILED( hr ) )
		{
			throw DXError( L"The FX file cannot be compiled.  Please run this executable from the directory that contains the FX file." );
		}

		// Create the vertex shader

		hr = device->CreateVertexShader( pVSBlob->GetBufferPointer(), pVSBlob->GetBufferSize(), NULL, &vertex_shader );
		if( FAILED( hr ) )
		{	
			pVSBlob->Release();
			throw DXError( L"Vertex shader creation failed.");
		}

		// The input layout specifies how the GPU will get the vertex information from the vertex buffer. The 5th
		// parameter in particular specifies where the GPU should start looking for that particular type of vertex

		D3D11_INPUT_ELEMENT_DESC layout[] =
		{
			{ "POSITION", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 0, D3D11_INPUT_PER_VERTEX_DATA, 0 },
			{ "COLOR", 0, DXGI_FORMAT_R32G32B32A32_FLOAT, 0, 12, D3D11_INPUT_PER_VERTEX_DATA, 0 },
		};
		UINT numElements = ARRAYSIZE( layout );

		// Create and set the input layout

		hr = device->CreateInputLayout( layout, numElements, pVSBlob->GetBufferPointer(),
											  pVSBlob->GetBufferSize(), &p_vertex_layout );
		pVSBlob->Release();
		if( FAILED( hr ) )
		{
			throw DXError( L"Input layout creation failed" );
		}

		device_context->IASetInputLayout( p_vertex_layout );

		// Compile the pixel shader

		ID3DBlob* pPSBlob = NULL;
		hr = Compile_Shader_From_File( L"shaders.fx", "PS", "ps_4_0", &pPSBlob );
		if( FAILED( hr ) )
		{
			throw DXError( L"The FX file cannot be compiled.  Please run this executable from the directory that contains the FX file." );
		}

		// Create the pixel layout

		hr = device->CreatePixelShader( pPSBlob->GetBufferPointer(), pPSBlob->GetBufferSize(), NULL, &pixel_shader );
		pPSBlob->Release();
		if( FAILED( hr ) )
		{
			throw DXError( L"Pixel layout creation failed. ");
		}
		SetViewAndProjectionMatrices(width, height);

	}
	catch( DXError e )
	{
		MessageBox( NULL, e.Get_Error(), L"Error", MB_OK );
	}
}
/*
		Settings for the Device and Swap Chain
*/
void I_DirectX::CreateSwapChain(HWND h_window)
{
		// Set up the swap chain and device
		HRESULT hr = S_OK;

		// Currently rendering this onto the entire screen of the window. Window management is inevitable
		RECT rc;
		GetClientRect( h_window, &rc );
		UINT width = rc.right - rc.left;
		UINT height = rc.bottom - rc.top;

		UINT createDeviceFlags = 0;

		D3D_DRIVER_TYPE driverTypes[] =
		{
			D3D_DRIVER_TYPE_HARDWARE,
			D3D_DRIVER_TYPE_WARP,
			D3D_DRIVER_TYPE_REFERENCE,
		};
		UINT numDriverTypes = ARRAYSIZE( driverTypes );

		D3D_FEATURE_LEVEL featureLevels[] =
		{
			D3D_FEATURE_LEVEL_11_0,
			D3D_FEATURE_LEVEL_10_1,
			D3D_FEATURE_LEVEL_10_0,
		};
		UINT numFeatureLevels = ARRAYSIZE( featureLevels );
		DXGI_SWAP_CHAIN_DESC swap_chain_desc;
		ZeroMemory( &swap_chain_desc, sizeof( swap_chain_desc ) );
		swap_chain_desc.BufferCount = 1;
		swap_chain_desc.BufferDesc.Width = width;
		swap_chain_desc.BufferDesc.Height = height;
		swap_chain_desc.BufferDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
		swap_chain_desc.BufferDesc.RefreshRate.Numerator = 60;
		swap_chain_desc.BufferDesc.RefreshRate.Denominator = 1;
		swap_chain_desc.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
		swap_chain_desc.OutputWindow = h_window;
		swap_chain_desc.SampleDesc.Count = 1;
		swap_chain_desc.SampleDesc.Quality = 0;
		swap_chain_desc.Windowed = TRUE;

		for( UINT driverTypeIndex = 0; driverTypeIndex < numDriverTypes; driverTypeIndex++ )
		{
			driver_type = driverTypes[driverTypeIndex];
			hr = D3D11CreateDeviceAndSwapChain( NULL, 
				driver_type, 
				NULL, 
				createDeviceFlags, 
				featureLevels, 
				numFeatureLevels,
				D3D11_SDK_VERSION,
				&swap_chain_desc,
				&p_swap_chain,
				&device,
				&feature_level, 
				&device_context );
			if( SUCCEEDED( hr ) )
				break;
		}
		if( FAILED( hr ) )
		{
			throw DXError( L"The driver type failed to initialize." );
		}
}

void I_DirectX::SetViewAndProjectionMatrices(int width, int height)
{
	// Initialize the view matrix

	Set_View_Matrix();

	// Initialize the projection matrix

	_projection = XMMatrixPerspectiveFovLH( XM_PIDIV2, width / (FLOAT)height, 0.01f, 100.0f );
}
/*
	Initializes the buffer
*/
void I_DirectX::InitializeBuffer()
{

	dynamic_buffer = new DynamicBuffer(device_context, device);
}

/*
		Manages the viewing matrix, will make more dynamic later
*/
void I_DirectX::Set_View_Matrix()
{
	
	XMVECTOR Eye = XMVectorSet( 0.0f, 10.0f, -20.0f, 0.0f );
	XMVECTOR At = XMVectorSet( 0.0f, 0.0f, 0.0f, 0.0f );
	XMVECTOR Up = XMVectorSet( 0.0f, 1.0f, 0.0f, 0.0f );

	_view = XMMatrixLookAtLH( Eye, At, Up );
}

/*
		Manages drawing the main screen. Is rendered each time the message loop is called
*/
void I_DirectX::Render()
{
   // Update our time
    static float t = 0.0f;
    if( driver_type == D3D_DRIVER_TYPE_REFERENCE )
    {
        t += ( float )XM_PI * 0.0125f;
    }
    else
    {
        static DWORD dwTimeStart = 0;
        DWORD dwTimeCur = GetTickCount();
        if( dwTimeStart == 0 )
            dwTimeStart = dwTimeCur;
        t = ( dwTimeCur - dwTimeStart ) / 1000.0f;
    }
    //
    // Clear the back buffer
    //
    float ClearColor[4] = { 0.0f, 0.0f, 0.0f, 1.0f }; //red, green, blue, alpha
    device_context->ClearRenderTargetView( p_render_target_view, ClearColor );
    //
    // Clear the depth buffer to 1.0 (max depth)
    //
    device_context->ClearDepthStencilView( p_depth_stencil_view, D3D11_CLEAR_DEPTH, 1.0f, 0 );
	
	// Render the positions of all the objects in the universe
	for(unsigned int i = 0; i < dynamic_buffer->Objects().size(); i++)
	{
		DrawObject(dynamic_buffer->Objects()[i]);
	}

    // Present our back buffer to our front buffer    
    p_swap_chain->Present( 0, 0 );
}


// This function compiles the shaders from the file

HRESULT I_DirectX::Compile_Shader_From_File( WCHAR* sz_file_name,
		LPCSTR sz_entry_point, LPCSTR sz_shader_model, ID3DBlob** pp_blob_out )
{
	HRESULT hr = S_OK;
	DWORD dw_shader_flags = D3DCOMPILE_ENABLE_STRICTNESS;
	ID3DBlob* p_error_blob;
	hr = D3DX11CompileFromFile( sz_file_name, NULL, NULL, sz_entry_point,
		sz_shader_model, dw_shader_flags, 0, NULL, pp_blob_out, &p_error_blob, NULL );
	if( FAILED(hr ) )
	{
        if( p_error_blob != NULL )
            OutputDebugStringA( (char*)p_error_blob->GetBufferPointer() );
        if( p_error_blob ) p_error_blob->Release();
        return hr;
    }
    if( p_error_blob ) p_error_blob->Release();
	return hr;
}
/*
		This function draws an object by rendering each primitive triangle that constitutes the particular object,
		and all the transformations that place the object in its final place
*/

void I_DirectX::DrawObject(VertexObject* current_object)
{
	    static float t = 0.0f;
    if( driver_type == D3D_DRIVER_TYPE_REFERENCE )
    {
        t += ( float )XM_PI * 0.0125f;
    }
    else
    {
        static DWORD dwTimeStart = 0;
        DWORD dwTimeCur = GetTickCount();
        if( dwTimeStart == 0 )
            dwTimeStart = dwTimeCur;
        t = ( dwTimeCur - dwTimeStart ) / 1000.0f;
    }

		XMMATRIX world = current_object->State()->Universe();
		// Map the current objects variables to a constant buffer
		ConstantBuffer object_buffer;
		object_buffer.mWorld = XMMatrixTranspose( world );
		object_buffer.mView = XMMatrixTranspose( _view );
		object_buffer.mProjection = XMMatrixTranspose( _projection );
		object_buffer.vOutputColor = current_object->Color();
		// Update the buffers constant buffer with this one, and draw the item
		device_context->UpdateSubresource( dynamic_buffer->p_const_buffer, 0, NULL, &object_buffer, 0, 0 );
		device_context->VSSetShader( vertex_shader, NULL, 0 );
		device_context->VSSetConstantBuffers( 0, 1, &dynamic_buffer->p_const_buffer );
		device_context->PSSetShader( pixel_shader, NULL, 0 );
		device_context->Draw( current_object->Size(), current_object->VertexIndex() );
}

ID3D11Device* I_DirectX::Device()
{
	return device;
}
ID3D11DeviceContext* I_DirectX::DeviceContext()
{
	return device_context;
}
#endif