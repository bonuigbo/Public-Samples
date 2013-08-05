//--------------------------------------------------------------------------------------
// Tetris3D
//--------------------------------------------------------------------------------------
#include <windows.h>
#include <d3d10.h>
#include <d3dx10.h>
#include "TetrisSimulator.h"
#include <stdio.h>

//--------------------------------------------------------------------------------------
// structures
//--------------------------------------------------------------------------------------
struct SimpleVertex
{
   D3DXVECTOR3 Pos;
   D3DXVECTOR4 Color;
};
//--------------------------------------------------------------------------------------
// Global Variables
//--------------------------------------------------------------------------------------
HINSTANCE                   g_hInst = NULL;
HWND                        g_hWnd = NULL;
D3D10_DRIVER_TYPE           g_driverType = D3D10_DRIVER_TYPE_NULL;
ID3D10Device*               g_pd3dDevice = NULL;
IDXGISwapChain*             g_pSwapChain = NULL;
ID3D10RenderTargetView*     g_pRenderTargetView = NULL;
ID3D10Texture2D*            g_pDepthStencil = NULL;
ID3D10DepthStencilView*     g_pDepthStencilView = NULL;
ID3D10Effect*               g_pEffect = NULL;
ID3D10EffectTechnique*      g_pTechnique = NULL;
ID3D10InputLayout*          g_pVertexLayout = NULL;
ID3D10Buffer*               g_pVertexBuffer = NULL;
ID3D10Buffer*               g_pIndexBuffer = NULL;
ID3D10EffectMatrixVariable* g_pWorldVariable = NULL;
ID3D10EffectMatrixVariable* g_pViewVariable = NULL;
ID3D10EffectMatrixVariable* g_pProjectionVariable = NULL;
D3DXMATRIX                  g_View;
D3DXMATRIX                  g_Projection;

//--------------------------------------------------------------------------------------
// Custom Declarations
//--------------------------------------------------------------------------------------
ID3D10EffectVectorVariable*   tetris_color_variable = NULL;
D3DXVECTOR4			tetris_color( 0.0f, 0.5f, 0.4f, 1.0f );
ID3DX10Font*			g_pFont = NULL;
ID3DX10Font*			g_pGameOver = NULL;
ID3DX10Sprite*		g_pSprite = NULL;
ID3D10DepthStencilState* g_pDepthStencilState = NULL;
TetrisSimulator		_simulator;


//--------------------------------------------------------------------------------------
// Forward declarations
//--------------------------------------------------------------------------------------
HRESULT InitWindow( HINSTANCE hInstance, int nCmdShow );
HRESULT InitDevice();
void CleanupDevice();
LRESULT CALLBACK    WndProc( HWND, UINT, WPARAM, LPARAM );
void Render();
void Draw_Tetris_Shape( float posx, float posy, float posz);
void Render_Words();


//--------------------------------------------------------------------------------------
// Entry point to the program. Initializes everything and goes into a message processing 
// loop. Idle time is used to render the scene.
//--------------------------------------------------------------------------------------
int WINAPI wWinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance, LPWSTR lpCmdLine, int nCmdShow )
{
	//--------------------------------------------------------------------------------------
	// Simulator controls which squares on the tetris world will be lit
	//--------------------------------------------------------------------------------------

    UNREFERENCED_PARAMETER( hPrevInstance );
    UNREFERENCED_PARAMETER( lpCmdLine );

    if( FAILED( InitWindow( hInstance, nCmdShow ) ) )
        return 0;

    if( FAILED( InitDevice() ) )
    {
        CleanupDevice();
        return 0;
    }

	// Set up the simulator variables
	int time_interval = _simulator.Get_Game_Speed();
	int time_increment = 0;
    // Main message loop
    MSG msg = {0};
    while( WM_QUIT != msg.message )
    {
        if( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) )
        {
            TranslateMessage( &msg );
            DispatchMessage( &msg );
        }
        else
        {
			// Simulate the program, and then render the screen
			time_increment++;
			if( time_increment > _simulator.Get_Game_Speed() )
			{
				_simulator.simulate();
				time_increment -= _simulator.Get_Game_Speed();
			}
            Render();
        }
    }
    CleanupDevice();

    return ( int )msg.wParam;
}


//--------------------------------------------------------------------------------------
// Register class and create window
//--------------------------------------------------------------------------------------
HRESULT InitWindow( HINSTANCE hInstance, int nCmdShow )
{
    // Register class
    WNDCLASSEX wcex;
	ZeroMemory( &wcex, sizeof( wcex ) );
    wcex.cbSize = sizeof( WNDCLASSEX );
    wcex.style = CS_HREDRAW | CS_VREDRAW;
    wcex.lpfnWndProc = WndProc;
    wcex.cbClsExtra = 0;
    wcex.cbWndExtra = 0;
    wcex.hInstance = hInstance;
    wcex.hIcon = 0;
    wcex.hCursor = LoadCursor( NULL, IDC_ARROW );
    wcex.hbrBackground = ( HBRUSH )( COLOR_WINDOW + 1 );
    wcex.lpszMenuName = NULL;
    wcex.lpszClassName = "Tetris3DClass";
    wcex.hIconSm = 0;
    if( !RegisterClassEx( &wcex ) )
        return E_FAIL;
    // Create window
    g_hInst = hInstance;
    RECT rc = { 0, 0, 640, 480 };
    AdjustWindowRect( &rc, WS_OVERLAPPEDWINDOW, FALSE );
    g_hWnd = CreateWindow( "Tetris3DClass", "Tetris3D", WS_OVERLAPPEDWINDOW,
                           CW_USEDEFAULT, CW_USEDEFAULT, rc.right - rc.left, rc.bottom - rc.top, NULL, NULL, hInstance,
                           NULL );
    if( !g_hWnd )
        return E_FAIL;

    ShowWindow( g_hWnd, nCmdShow );

    return S_OK;
}


//--------------------------------------------------------------------------------------
// Create Direct3D device and swap chain
//--------------------------------------------------------------------------------------
HRESULT InitDevice()
{
    HRESULT hr = S_OK;

    RECT rc;
    GetClientRect( g_hWnd, &rc );
    UINT width = rc.right - rc.left;
    UINT height = rc.bottom - rc.top;

    UINT createDeviceFlags = 0;
#ifdef _DEBUG
    createDeviceFlags |= D3D10_CREATE_DEVICE_DEBUG;
#endif

    D3D10_DRIVER_TYPE driverTypes[] =
    {
        D3D10_DRIVER_TYPE_HARDWARE,
        D3D10_DRIVER_TYPE_REFERENCE,
    };
    UINT numDriverTypes = sizeof( driverTypes ) / sizeof( driverTypes[0] );

    DXGI_SWAP_CHAIN_DESC sd;
    ZeroMemory( &sd, sizeof( sd ) );
    sd.BufferCount = 1;
    sd.BufferDesc.Width = width;
    sd.BufferDesc.Height = height;
    sd.BufferDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
    sd.BufferDesc.RefreshRate.Numerator = 60;
    sd.BufferDesc.RefreshRate.Denominator = 1;
    sd.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
    sd.OutputWindow = g_hWnd;
    sd.SampleDesc.Count = 1;
    sd.SampleDesc.Quality = 0;
    sd.Windowed = TRUE;

    for( UINT driverTypeIndex = 0; driverTypeIndex < numDriverTypes; driverTypeIndex++ )
    {
        g_driverType = driverTypes[driverTypeIndex];
        hr = D3D10CreateDeviceAndSwapChain( NULL, g_driverType, NULL, createDeviceFlags,
                                            D3D10_SDK_VERSION, &sd, &g_pSwapChain, &g_pd3dDevice );
        if( SUCCEEDED( hr ) )
            break;
    }
    if( FAILED( hr ) )
        return hr;

    // Create a render target view
    ID3D10Texture2D* pBuffer;
    hr = g_pSwapChain->GetBuffer( 0, __uuidof( ID3D10Texture2D ), ( LPVOID* )&pBuffer );
    if( FAILED( hr ) )
        return hr;

    hr = g_pd3dDevice->CreateRenderTargetView( pBuffer, NULL, &g_pRenderTargetView );
    pBuffer->Release();
    if( FAILED( hr ) )
        return hr;

    // Create depth stencil texture
    D3D10_TEXTURE2D_DESC descDepth;
    descDepth.Width = width;
    descDepth.Height = height;
    descDepth.MipLevels = 1;
    descDepth.ArraySize = 1;
    descDepth.Format = DXGI_FORMAT_D32_FLOAT;
    descDepth.SampleDesc.Count = 1;
    descDepth.SampleDesc.Quality = 0;
    descDepth.Usage = D3D10_USAGE_DEFAULT;
    descDepth.BindFlags = D3D10_BIND_DEPTH_STENCIL;
    descDepth.CPUAccessFlags = 0;
    descDepth.MiscFlags = 0;
    hr = g_pd3dDevice->CreateTexture2D( &descDepth, NULL, &g_pDepthStencil );
    if( FAILED( hr ) )
        return hr;

    // Create the depth stencil view
    D3D10_DEPTH_STENCIL_VIEW_DESC descDSV;
    descDSV.Format = descDepth.Format;
    descDSV.ViewDimension = D3D10_DSV_DIMENSION_TEXTURE2D;
    descDSV.Texture2D.MipSlice = 0;
    hr = g_pd3dDevice->CreateDepthStencilView( g_pDepthStencil, &descDSV, &g_pDepthStencilView );
    if( FAILED( hr ) )
        return hr;

    g_pd3dDevice->OMSetRenderTargets( 1, &g_pRenderTargetView, g_pDepthStencilView );

    // Setup the viewport
    D3D10_VIEWPORT vp;
    vp.Width = width;
    vp.Height = height;
    vp.MinDepth = 0.0f;
    vp.MaxDepth = 1.0f;
    vp.TopLeftX = 0;
    vp.TopLeftY = 0;
    g_pd3dDevice->RSSetViewports( 1, &vp );

    // Create the effect
    DWORD dwShaderFlags = D3D10_SHADER_ENABLE_STRICTNESS;
#if defined( DEBUG ) || defined( _DEBUG )
    // Set the D3D10_SHADER_DEBUG flag to embed debug information in the shaders.
    // Setting this flag improves the shader debugging experience, but still allows 
    // the shaders to be optimized and to run exactly the way they will run in 
    // the release configuration of this program.
    dwShaderFlags |= D3D10_SHADER_DEBUG;
    #endif
	hr = D3DX10CreateEffectFromFile( "Tetris3D.fx", NULL, NULL, "fx_4_0", dwShaderFlags, 0, g_pd3dDevice, NULL,
                                         NULL, &g_pEffect, NULL, NULL );
    if( FAILED( hr ) )
    {
        MessageBox( NULL,
                    "The FX file cannot be located.  Please run this executable from the directory that contains the FX file.", "Error", MB_OK );
        return hr;
    }

    // Obtain the technique
    g_pTechnique = g_pEffect->GetTechniqueByName( "Render" );

    // Obtain the variables
    g_pWorldVariable = g_pEffect->GetVariableByName( "World" )->AsMatrix();
    g_pViewVariable = g_pEffect->GetVariableByName( "View" )->AsMatrix();
    g_pProjectionVariable = g_pEffect->GetVariableByName( "Projection" )->AsMatrix();

	tetris_color_variable = g_pEffect->GetVariableByName( "tetris_color" )->AsVector();

    // Define the input layout
    D3D10_INPUT_ELEMENT_DESC layout[] =
    {
        { "POSITION", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 0, D3D10_INPUT_PER_VERTEX_DATA, 0 },
        { "COLOR", 0, DXGI_FORMAT_R32G32B32A32_FLOAT, 0, 12, D3D10_INPUT_PER_VERTEX_DATA, 0 },
    };
    UINT numElements = sizeof( layout ) / sizeof( layout[0] );

    // Create the input layout
    D3D10_PASS_DESC PassDesc;
    g_pTechnique->GetPassByIndex( 0 )->GetDesc( &PassDesc );
    hr = g_pd3dDevice->CreateInputLayout( layout, numElements, PassDesc.pIAInputSignature,
                                          PassDesc.IAInputSignatureSize, &g_pVertexLayout );
    if( FAILED( hr ) )
        return hr;

    // Set the input layout
    g_pd3dDevice->IASetInputLayout( g_pVertexLayout );
	SimpleVertex tetris_vertices[] =
	{
		// Square block that makes up most of the tetris world
		{ D3DXVECTOR3( 0.0f, 0.0f, 0.0f ), D3DXVECTOR4( 0.0f, 0.5f, 1.0f, 1.0f ) },
		{ D3DXVECTOR3( 1.0f, 0.0f, 0.0f ), D3DXVECTOR4( 0.0f, 0.1f, 1.0f, 1.0f ) },
		{ D3DXVECTOR3( 1.0f, 0.0f, 1.0f ), D3DXVECTOR4( 0.0f, 0.5f, 1.0f, 1.0f ) },
		{ D3DXVECTOR3( 0.0f, 0.0f, 1.0f ), D3DXVECTOR4( 0.0f, 0.1f, 1.0f, 1.0f ) },
		{ D3DXVECTOR3( 0.0f, 1.0f, 0.0f ), D3DXVECTOR4( 0.0f, 0.1f, 0.0f, 1.0f ) },
		{ D3DXVECTOR3( 1.0f, 1.0f, 0.0f ), D3DXVECTOR4( 0.0f, 0.1f, 1.0f, 1.0f ) },
		{ D3DXVECTOR3( 1.0f, 1.0f, 1.0f ), D3DXVECTOR4( 0.0f, 0.5f, 1.0f, 1.0f ) },
		{ D3DXVECTOR3( 0.0f, 1.0f, 1.0f ), D3DXVECTOR4( 0.0f, 0.1f, 1.0f, 1.0f ) },
	};

	DWORD tetris_indices[] =
	{
		//Block
		7, 5, 4, 
		6, 5, 7, 
		4, 1, 0, 
		5, 1, 4, 
		5, 2, 1, 
		6, 2, 5, 
		6, 3, 2, 
		7, 3, 6, 
		7, 0, 3, 
		4, 0, 7, 
		0, 2, 3, 
		1, 2, 0, 
	}; 

    D3D10_BUFFER_DESC bd;
    bd.Usage = D3D10_USAGE_DEFAULT;
	bd.ByteWidth = sizeof( SimpleVertex ) * 8;
    bd.BindFlags = D3D10_BIND_VERTEX_BUFFER;
    bd.CPUAccessFlags = 0;
    bd.MiscFlags = 0;
    D3D10_SUBRESOURCE_DATA InitData;
	InitData.pSysMem = tetris_vertices;
    g_pd3dDevice->CreateBuffer( &bd, &InitData, &g_pVertexBuffer );

    // Set vertex buffer
    UINT stride = sizeof( SimpleVertex );
    UINT offset = 0;
    g_pd3dDevice->IASetVertexBuffers( 0, 1, &g_pVertexBuffer, &stride, &offset );

    // Create index buffer
    // Create vertex buffer

    bd.Usage = D3D10_USAGE_DEFAULT;
    bd.ByteWidth = sizeof( DWORD ) * 36;
    bd.BindFlags = D3D10_BIND_INDEX_BUFFER;
    bd.CPUAccessFlags = 0;
    bd.MiscFlags = 0;
    InitData.pSysMem = tetris_indices;
    g_pd3dDevice->CreateBuffer( &bd, &InitData, &g_pIndexBuffer );

    // Initialize the view matrix
    D3DXVECTOR3 Eye( 6.0f, 10.0f, -27.0f );
    D3DXVECTOR3 At( 6.0f, 10.0f, 0.0f );
    D3DXVECTOR3 Up( 0.0f, 1.0f, 0.0f );
    D3DXMatrixLookAtLH( &g_View, &Eye, &At, &Up );

    // Initialize the projection matrix
    D3DXMatrixPerspectiveFovLH( &g_Projection, ( float )D3DX_PI * 0.25f, width / ( FLOAT )height, 0.1f, 100.0f );

    // Set index buffer
    g_pd3dDevice->IASetIndexBuffer( g_pIndexBuffer, DXGI_FORMAT_R32_UINT, 0 );

    // Set primitive topology
    g_pd3dDevice->IASetPrimitiveTopology( D3D10_PRIMITIVE_TOPOLOGY_TRIANGLELIST );

	// Create the font object
	D3DX10CreateSprite( g_pd3dDevice, 512, &g_pSprite );
	D3DX10CreateFont( g_pd3dDevice, 20, 20, FW_BOLD, 1, FALSE, DEFAULT_CHARSET,
                                OUT_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH | FF_DONTCARE,
                                "ComicSans", &g_pFont );
	D3DX10CreateFont( g_pd3dDevice, 40, 25, FW_BOLD, 1, TRUE, DEFAULT_CHARSET,
                                OUT_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH | FF_DONTCARE,
                                "ComicSans", &g_pGameOver );
    return TRUE;

}


//--------------------------------------------------------------------------------------
// Clean up the objects we've created
//--------------------------------------------------------------------------------------
void CleanupDevice()
{
    if( g_pd3dDevice ) g_pd3dDevice->ClearState();

    if( g_pVertexBuffer ) g_pVertexBuffer->Release();
    if( g_pIndexBuffer ) g_pIndexBuffer->Release();
    if( g_pVertexLayout ) g_pVertexLayout->Release();
    if( g_pEffect ) g_pEffect->Release();
    if( g_pRenderTargetView ) g_pRenderTargetView->Release();
    if( g_pDepthStencil ) g_pDepthStencil->Release();
    if( g_pDepthStencilView ) g_pDepthStencilView->Release();
	if( g_pFont ) g_pFont->Release();
	if( g_pGameOver ) g_pGameOver->Release();
	if( g_pSprite ) g_pSprite->Release();
    if( g_pSwapChain ) g_pSwapChain->Release();
    if( g_pd3dDevice ) g_pd3dDevice->Release();


}


//--------------------------------------------------------------------------------------
// Called every time the application receives a message
//--------------------------------------------------------------------------------------
LRESULT CALLBACK WndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
    PAINTSTRUCT ps;
    HDC hdc = 0;

    switch( message )
    {
        case WM_PAINT:
            hdc = BeginPaint( hWnd, &ps );
            EndPaint( hWnd, &ps );
            break;

        case WM_DESTROY:
            PostQuitMessage( 0 );
            break;
		case WM_KEYDOWN:
			switch( wParam )
			{
			case VK_LEFT:
				_simulator.Move_Left();
				break;
			case VK_RIGHT:
				_simulator.Move_Right();
				break;
			case VK_UP:
				_simulator.Rotate_Right();
				break;
			case VK_DOWN:
				_simulator.Move_Down();
				break;
			case VK_SPACE:
				_simulator.Drop_Object();
				break;
			}
			break;

        default:
            return DefWindowProc( hWnd, message, wParam, lParam );
    }
    return 0;
}

//--------------------------------------------------------------------------------------
// Render a frame
//--------------------------------------------------------------------------------------
void Render()
{
	//--------------------------------------------------------------------------------
    // Update our time
	//--------------------------------------------------------------------------------
    static float t = 0.0f;
    if( g_driverType == D3D10_DRIVER_TYPE_REFERENCE )
    {
        t += static_cast<float>( D3DX_PI * 0.0125f );
    }
    else
    {
        static DWORD dwTimeStart = 0;
        DWORD dwTimeCur = GetTickCount();
        if( dwTimeStart == 0 )
            dwTimeStart = dwTimeCur;
        t = ( dwTimeCur - dwTimeStart ) / 1000.0f;
    }
    //--------------------------------------------------------------------------------
    // Clear the back buffer
    //--------------------------------------------------------------------------------
    float ClearColor[4] = { 0.1f, 0.0f, 0.2f, 1.0f }; //red, green, blue, alpha
    g_pd3dDevice->ClearRenderTargetView( g_pRenderTargetView, ClearColor );
    //--------------------------------------------------------------------------------
    // Clear the depth buffer to 1.0 (max depth)
    //--------------------------------------------------------------------------------
    g_pd3dDevice->ClearDepthStencilView( g_pDepthStencilView, D3D10_CLEAR_DEPTH, 1.0f, 0 );


	//----------------------------------------------------------------------
	// Render the world space
	//----------------------------------------------------------------------
	int width = _simulator.Get_World().Get_Width();
	int height = _simulator.Get_World().Get_Height();
	for( int i = 0; i < width; i++ )
	{
		for( int j = 0; j < height; j++ )
		{
			if( _simulator.Get_World().Get_Spot( i, j ) == 1 )
				Draw_Tetris_Shape( static_cast<float>(i), static_cast<float>(j), 0.0 );
		}
	}
	width = _simulator.Get_Next_Shape().Get_Width();
	height = _simulator.Get_Next_Shape().Get_Height();
	for( int i = 0; i < width; i++ )
	{
		for( int j = 0; j < height; j++ )
		{
			if( _simulator.Get_Next_Shape().Get_Spot( i, j ) == 1 )
				Draw_Tetris_Shape( static_cast<float>(i - 9), static_cast<float>(j + 8), 0.0 );
		}
	}
	 //--------------------------------------------------------------------------------
    // Render words and sprites
    //--------------------------------------------------------------------------------
	Render_Words();

	//--------------------------------------------------------------------------------
    // Present our back buffer to our front buffer
	//--------------------------------------------------------------------------------
    g_pSwapChain->Present( 0, 0 );
}

//--------------------------------------------------------------------------------
// This function manages drawing the particular blocks to the screen
//--------------------------------------------------------------------------------
void Draw_Tetris_Shape( float posx = 0.0, float posy = 0.0, float posz = 0.0 )
{
	D3D10_TECHNIQUE_DESC techDesc;
    g_pTechnique->GetDesc( &techDesc );
	D3DXMATRIX block_world;
    D3DXMATRIX mTranslate;
	D3DXMatrixTranslation( &mTranslate, posx, posy, posz );
	D3DXMatrixIdentity( &block_world );
	D3DXMatrixMultiply( &block_world, &block_world, &mTranslate );
	g_pWorldVariable->SetMatrix( ( float* )&block_world );
	g_pViewVariable->SetMatrix( ( float* )&g_View );
	g_pProjectionVariable->SetMatrix( ( float* )&g_Projection );
	// Can change the color 
	tetris_color = D3DXVECTOR4( 0.0f, 0.0f, 0.0f, 1.0f );
	tetris_color_variable->SetFloatVector( (float*)tetris_color );
	for( UINT p = 0; p < techDesc.Passes; ++p )
	{
		g_pTechnique->GetPassByIndex( p )->Apply( 0 );
		g_pd3dDevice->DrawIndexed( 36, 0, 0 );
	}
}

void Render_Words()
{

	RECT tetris_rec = { 10, 20, 200, 40 };
	D3DXMATRIX spriteWorld;
	D3DXMatrixIdentity( &spriteWorld );
	D3DX10_SPRITE sprite = { spriteWorld, D3DXVECTOR2( 1.0f, 1.0f ), D3DXVECTOR2( 10.0f, 20.0f ), D3DXCOLOR(0.0f, 1.0f, 1.0f, 1.0f), NULL, 0 };
	
	g_pSprite->Begin( 0x08 );
	int level = _simulator.Get_Level();
	int score = _simulator.Get_Score();
	int lines = _simulator.Get_Lines();
	char level_str[10];
	char score_str[10] ;
	char lines_str[10];
	int level_num = 0;
	int score_num = 0;
	int lines_num = 0;

	g_pFont->DrawText( g_pSprite, "TETRIS", 6, &tetris_rec, DT_CENTER, D3DXCOLOR( 0.9f, 0.9f, 1.0f, 1.0f )  );
	SetRect( &tetris_rec, 10, 40, 200, 60  );
	g_pFont->DrawText( g_pSprite, "3D", 2, &tetris_rec, DT_CENTER, D3DXCOLOR( 0.9f, 0.9f, 1.0f, 1.0f )  );

	level_num = wsprintf( level_str, "%d", level);
	SetRect( &tetris_rec, 460, 20, 600, 40  );
	g_pFont->DrawText( g_pSprite, "Level", 5, &tetris_rec, DT_CENTER, D3DXCOLOR( 0.9f, 0.9f, 1.0f, 1.0f )  );
	SetRect( &tetris_rec, 460, 40, 600, 460  );
	g_pFont->DrawText( g_pSprite, (LPCSTR)level_str , level_num, &tetris_rec, DT_CENTER, D3DXCOLOR( 0.9f, 0.9f, 1.0f, 1.0f )  );


	score_num = wsprintf( score_str, "%d", score);
	SetRect( &tetris_rec, 460, 120, 600, 140  );
	g_pFont->DrawText( g_pSprite, "Score", 5, &tetris_rec, DT_CENTER, D3DXCOLOR( 0.9f, 0.9f, 1.0f, 1.0f )  );
	SetRect( &tetris_rec, 460, 140, 600, 160  );
	g_pFont->DrawText( g_pSprite, (LPCSTR)score_str, score_num, &tetris_rec, DT_CENTER, D3DXCOLOR( 0.9f, 0.9f, 1.0f, 1.0f )  );

	lines_num = wsprintf( lines_str, "%d", lines);
	SetRect( &tetris_rec, 460, 220, 600, 240  );
	g_pFont->DrawText( g_pSprite, "Lines", 5, &tetris_rec, DT_CENTER, D3DXCOLOR( 0.9f, 0.9f, 1.0f, 1.0f )  );
	SetRect( &tetris_rec, 460, 240, 600, 260  );
	g_pFont->DrawText( g_pSprite, (LPCSTR)lines_str, lines_num, &tetris_rec, DT_CENTER, D3DXCOLOR( 0.9f, 0.9f, 1.0f, 1.0f )  );


	if ( _simulator.Game_Over() )
	{
		SetRect( &tetris_rec, 100, 100, 500, 500 );
		g_pGameOver->DrawText(g_pSprite, "GAME OVER", 9, &tetris_rec, DT_CENTER, D3DXCOLOR( 0.9f, 0.9f, 1.0f, 1.0f ) );
	}

	g_pSprite->End();	
	g_pd3dDevice->OMSetDepthStencilState( g_pDepthStencilState, 1);
}