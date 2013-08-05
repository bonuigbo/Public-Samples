#ifndef DEFAULTSHAPES
#define DEFAULTSHAPES

class Cube : public VertexObject
{
public:
	Cube();
};

Cube::Cube()
	: VertexObject()
{
	Vertex* cube_vertices = new Vertex[36];
	Vertex A =  Vertex( XMFLOAT3( -0.5f, -0.5f, -0.5f ), WHITE );
	Vertex B =  Vertex( XMFLOAT3( -0.5f, 0.5f, -0.5f ), WHITE );
	Vertex C =  Vertex( XMFLOAT3( 0.5f, -0.5f, -0.5f ), WHITE );
	Vertex D =  Vertex( XMFLOAT3( 0.5f, 0.5f, -0.5f ), WHITE );
	Vertex E =  Vertex( XMFLOAT3( -0.5f, -0.5f, 0.5f ), WHITE );
	Vertex F =  Vertex( XMFLOAT3( -0.5f, 0.5f, 0.5f ), WHITE );
	Vertex G =  Vertex( XMFLOAT3( 0.5f, -0.5f, 0.5f ), WHITE );
	Vertex H =  Vertex( XMFLOAT3( 0.5f, 0.5f, 0.5f ), WHITE );
	cube_vertices[0] = A;
	cube_vertices[1] = B;
	cube_vertices[2] = C;
	cube_vertices[3] = C;
	cube_vertices[4] = B;
	cube_vertices[5] = D;
	// second face
	cube_vertices[6] = C;
	cube_vertices[7] = D;
	cube_vertices[8] = G;
	cube_vertices[9] = G;
	cube_vertices[10] = D;
	cube_vertices[11] = H;
	// third face
	cube_vertices[12] =   G;
	cube_vertices[13] =   H;
	cube_vertices[14] =   E;
	cube_vertices[15] =  E;
	cube_vertices[16] =   H;
	cube_vertices[17] =   F;
	// 4th face
	cube_vertices[18] = E;
	cube_vertices[19] = F;
	cube_vertices[20] = A;
	cube_vertices[21] = A;
	cube_vertices[22] = F;
	cube_vertices[23] = B;
	// top face
	cube_vertices[24] = E;
	cube_vertices[25] = A;
	cube_vertices[26] = G;
	cube_vertices[27] = G;
	cube_vertices[28] = A;
	cube_vertices[29] = C;
	// bottom face
	 cube_vertices[30] = B;
	 cube_vertices[31] = F;
	 cube_vertices[32] = D;
	 cube_vertices[33] = D;
	 cube_vertices[34] = F;
	 cube_vertices[35] = H;

	AddVertices(cube_vertices, 36);
	State()->SetDimensions(1.0f, 1.0f, 1.0f);
	SetType("CUBE");
}

class Octogon : public VertexObject
{
public:
	Octogon();
};

Octogon::Octogon() : VertexObject()
{
	Vertex* vertices = new Vertex[132];

	Vertex A = Vertex( XMFLOAT3(-0.5f, -0.5f, -1.20711f), WHITE );
	Vertex B = Vertex( XMFLOAT3( -0.5f, 0.5f, -1.20711f), WHITE );
	Vertex C = Vertex( XMFLOAT3(0.5f, 0.5f, -1.20711f), WHITE );
	Vertex D = Vertex( XMFLOAT3(0.5f, -0.5f, -1.20711f), WHITE );
	Vertex E = Vertex( XMFLOAT3(-0.5f, -1.20711f, -0.5f), WHITE );
	Vertex F = Vertex( XMFLOAT3(0.5f, -1.20711f, -0.5f), WHITE );
	Vertex G = Vertex( XMFLOAT3(1.20711f, -0.5f, -0.5f ), WHITE );
	Vertex H = Vertex( XMFLOAT3( 1.20711f, 0.5f, -0.5f ), WHITE );
	Vertex I = Vertex( XMFLOAT3( 0.5f, 1.20711f, -0.5f), WHITE );
	Vertex J = Vertex( XMFLOAT3( -0.5f, 1.20711f, -0.5f), WHITE );
	Vertex K = Vertex( XMFLOAT3(  -1.20711f, 0.5f, -0.5f ), WHITE );
	Vertex L = Vertex( XMFLOAT3( -1.20711f, -0.5f, -0.5f  ), WHITE );
	Vertex M = Vertex( XMFLOAT3(-0.5f, -0.5f, 1.20711f), WHITE );
	Vertex N = Vertex( XMFLOAT3( -0.5f, 0.5f, 1.20711f), WHITE );
	Vertex O = Vertex( XMFLOAT3(0.5f, 0.5f, 1.20711f), WHITE );
	Vertex P = Vertex( XMFLOAT3(0.5f, -0.5f, 1.20711f), WHITE );
	Vertex Q = Vertex( XMFLOAT3(-0.5f, -1.20711f, 0.5f), WHITE );
	Vertex R = Vertex( XMFLOAT3(0.5f, -1.20711f, 0.5f), WHITE );
	Vertex S = Vertex( XMFLOAT3(1.20711f, -0.5f, 0.5f ), WHITE );
	Vertex T = Vertex( XMFLOAT3( 1.20711f, 0.5f, 0.5f ), WHITE );
	Vertex U = Vertex( XMFLOAT3( 0.5f, 1.20711f, 0.5f), WHITE );
	Vertex V = Vertex( XMFLOAT3( -0.5f, 1.20711f, 0.5f), WHITE );
	Vertex W = Vertex( XMFLOAT3(  -1.20711f, 0.5f, 0.5f ), WHITE );
	Vertex X = Vertex( XMFLOAT3( -1.20711f, -0.5f, 0.5f  ), WHITE );

	vertices[0] = A;
	vertices[1] = B;
	vertices[2] = D;
	vertices[3] = D;
	vertices[4] = B;
	vertices[5] = C;

	vertices[6] = B;
	vertices[7] = J;
	vertices[8] = C;
	vertices[9] = C;
	vertices[10] = J;
	vertices[11] = I;

	vertices[12] = E;
	vertices[13] = A;
	vertices[14] = F;
	vertices[15] = F;
	vertices[16] = A;
	vertices[17] = D;

	vertices[18] = L;
	vertices[19] = K;
	vertices[20] = A;
	vertices[21] = A;
	vertices[22] = K;
	vertices[23] = B;

	vertices[24] = D;
	vertices[25] =C;
	vertices[26] = G;
	vertices[27] = G;
	vertices[28] = C;
	vertices[29] = H;

	vertices[30] = O;
	vertices[31] = N;
	vertices[32] = P;
	vertices[33] = P;
	vertices[34] = N;
	vertices[35] = M;

	vertices[36] = U;
	vertices[37] = V;
	vertices[38] = O;
	vertices[39] = O;
	vertices[40] = V;
	vertices[41] = N;

	vertices[42] = P;
	vertices[43] = M;
	vertices[44] = R;
	vertices[45] = R;
	vertices[46] = M;
	vertices[47] = Q;

	vertices[48] = N;
	vertices[49] = W;
	vertices[50] = M;
	vertices[51] = M;
	vertices[52] = W;
	vertices[53] = X;

	vertices[54] = T;
	vertices[55] = O;
	vertices[56] = S;
	vertices[57] = S;
	vertices[58] = O;
	vertices[59] = P;

	vertices[60] = J;
	vertices[61] = V;
	vertices[62] = I;
	vertices[63] = I;
	vertices[64] = V;
	vertices[65] = U;

	vertices[66] = H;
	vertices[67] = I;
	vertices[68] =T;
	vertices[69] = T;
	vertices[70] = I;
	vertices[71] = U;

	vertices[72] = G;
	vertices[73] = H;
	vertices[74] = S;
	vertices[75] = S;
	vertices[76] = H;
	vertices[77] = T;

	vertices[78] = F;
	vertices[79] = G;
	vertices[80] = R;
	vertices[81] = R;
	vertices[82] = G;
	vertices[83] = S;

	vertices[84] = E;
	vertices[85] = F;
	vertices[86] = Q;
	vertices[87] = Q;
	vertices[88] = F;
	vertices[89] = R;

	vertices[90] = Q;
	vertices[91] = X;
	vertices[92] = E;
	vertices[93] = E;
	vertices[94] = X;
	vertices[95] = L;

	vertices[96] = X;
	vertices[97] = W;
	vertices[98] = L;
	vertices[99] = L;
	vertices[100] = W;
	vertices[101] = K;

	vertices[102] = W;
	vertices[103] = V;
	vertices[104] = K;
	vertices[105] = K;
	vertices[106] = V;
	vertices[107] = J;

	vertices[108] = L;
	vertices[109] = A;
	vertices[110] = E;

	vertices[111] = J;
	vertices[112] = B;
	vertices[113] = K;

	vertices[114] = H;
	vertices[115] = C;
	vertices[116] = I;

	vertices[117] = F;
	vertices[118] = D;
	vertices[119] = G;

	vertices[120] = Q;
	vertices[121] = M;
	vertices[122] = X;

	vertices[123] = W;
	vertices[124] = N;
	vertices[125] = V;

	vertices[126] = U;
	vertices[127] = O;
	vertices[128] = T;

	vertices[129] = S;
	vertices[130] = P;
	vertices[131] = R;


	AddVertices(vertices, 132);
	State()->SetDimensions(1.20711f, 1.20711f, 1.20711f);
	SetType("OCTOGON");
}

class Square : public VertexObject
{
public:
	Square();
};

Square::Square()
	: VertexObject()
{
	Vertex* square_vertices = new Vertex[12];
	Vertex A =  Vertex( XMFLOAT3( -0.5f, -0.5f, 0.0f ), WHITE );
	Vertex B =  Vertex( XMFLOAT3( -0.5f, 0.5f, 0.0f ), WHITE );
	Vertex C =  Vertex( XMFLOAT3( 0.5f, -0.5f, 0.0f ), WHITE );
	Vertex D =  Vertex( XMFLOAT3( 0.5f, 0.5f, 0.0f ), WHITE );

	square_vertices[0] = A;
	square_vertices[1] = B;
	square_vertices[2] = C;
	square_vertices[3] = C;
	square_vertices[4] = B;
	square_vertices[5] = D;
	square_vertices[6] = C;
	square_vertices[7] = B;
	square_vertices[8] = A;
	square_vertices[9] = D;
	square_vertices[10] = B;
	square_vertices[11] = C;

	State()->SetDimensions(1.0f, 1.0f, 1.0f);
	AddVertices(square_vertices, 12);
	SetType("SQUARE");
}

class Triangle : public VertexObject
{
public:
	Triangle();
};

Triangle::Triangle()
	: VertexObject()
{
	Vertex* triangle_vertices = new Vertex[6];
	Vertex A =  Vertex( XMFLOAT3( -1.0f, -1.0f, 0.0f ), WHITE );
	Vertex B =  Vertex( XMFLOAT3( -1.0f, 1.0f, 0.0f ), WHITE );
	Vertex C =  Vertex( XMFLOAT3( 1.0f, -1.0f, 0.0f ), WHITE );

	triangle_vertices[0] = A;
	triangle_vertices[1] = B;
	triangle_vertices[2] = C;
	triangle_vertices[3] = C;
	triangle_vertices[4] = B;
	triangle_vertices[5] = A;

	State()->SetDimensions(2.0f, 2.0f, 2.0f);
	AddVertices(triangle_vertices, 6);
	SetType("TRIANGLE");
}

#endif