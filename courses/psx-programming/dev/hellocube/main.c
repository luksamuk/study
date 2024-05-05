// For more info, see https://github.com/ABelliqueux/nolibgs_hello_worlds/tree/main/hello_cube

#include <sys/types.h>
#include <libgte.h>
#include <libgpu.h>
#include <libetc.h>
#include <stdio.h>

// Sample vector model
/* ========================================== */
SVECTOR modelCube_mesh[] = {
    { -128,128,128 },
    { 128,128,128 },
    { 128,128,-128 },
    { -128,128,-128 },
    { -128,-128,128 },
    { 128,-128,128 },
    { 128,-128,-128 },
    { -128,-128,-128 }
};

SVECTOR modelCube_normal[] = {
    2365,-2365,-2365, 0,
    -2365,-2365,-2365, 0,
    -2365,-2365,2365, 0,
    2365,-2365,2365, 0,
    2365,2365,-2365, 0,
    -2365,2365,-2365, 0,
    -2365,2365,2365, 0,
    2365,2365,2365, 0
};

CVECTOR modelCube_color[] = {
    255,237,0, 0,
    255,235,0, 0,
    255,236,0, 0,
    255,2,0, 0,
    254,3,0, 0,
    255,8,0, 0,
    229,0,255, 0,
    229,0,255, 0,
    229,0,255, 0,
    5,16,250, 0,
    0,12,255, 0,
    0,12,255, 0,
    4,251,25, 0,
    0,255,26, 0,
    0,255,26, 0,
    0,248,255, 0,
    0,248,255, 0,
    0,248,255, 0,
    255,237,0, 0,
    255,237,0, 0,
    255,235,0, 0,
    255,2,0, 0,
    255,6,2, 0,
    254,3,0, 0,
    229,0,255, 0,
    232,21,232, 0,
    229,0,255, 0,
    5,16,250, 0,
    2,13,253, 0,
    0,12,255, 0,
    4,251,25, 0,
    0,255,26, 0,
    0,255,26, 0,
    0,248,255, 0,
    0,248,255, 0,
    0,248,255, 0
};

int modelCube_index[] = {
    0,2,3,
    7,5,4,
    4,1,0,
    5,2,1,
    2,7,3,
    0,7,4,
    0,1,2,
    7,6,5,
    4,5,1,
    5,6,2,
    2,6,7,
    0,3,7
};

TMESH modelCube = {
    modelCube_mesh,
    modelCube_normal,
    0,
    modelCube_color,
    12
};

SVECTOR modelCube1_mesh[] = {
    { -128,128,128 },
    { 128,128,128 },
    { 128,128,-128 },
    { -128,128,-128 },
    { -128,-128,128 },
    { 128,-128,128 },
    { 128,-128,-128 },
    { -128,-128,-128 }
};

SVECTOR modelCube1_normal[] = {
    2365,-2365,-2365, 0,
    -2365,-2365,-2365, 0,
    -2365,-2365,2365, 0,
    2365,-2365,2365, 0,
    2365,2365,-2365, 0,
    -2365,2365,-2365, 0,
    -2365,2365,2365, 0,
    2365,2365,2365, 0
};

CVECTOR modelCube1_color[] = {
    255,237,0, 0,
    255,235,0, 0,
    255,236,0, 0,
    255,2,0, 0,
    254,3,0, 0,
    255,8,0, 0,
    229,0,255, 0,
    229,0,255, 0,
    229,0,255, 0,
    5,16,250, 0,
    0,12,255, 0,
    0,12,255, 0,
    4,251,25, 0,
    0,255,26, 0,
    0,255,26, 0,
    0,248,255, 0,
    0,248,255, 0,
    0,248,255, 0,
    255,237,0, 0,
    255,237,0, 0,
    255,235,0, 0,
    255,2,0, 0,
    255,6,2, 0,
    254,3,0, 0,
    229,0,255, 0,
    232,21,232, 0,
    229,0,255, 0,
    5,16,250, 0,
    2,13,253, 0,
    0,12,255, 0,
    4,251,25, 0,
    0,255,26, 0,
    0,255,26, 0,
    0,248,255, 0,
    0,248,255, 0,
    0,248,255, 0
};

int modelCube1_index[] = {
    0,2,3,
    7,5,4,
    4,1,0,
    5,2,1,
    2,7,3,
    0,7,4,
    0,1,2,
    7,6,5,
    4,5,1,
    5,6,2,
    2,6,7,
    0,3,7
};

TMESH modelCube1 = {
    modelCube1_mesh,
    modelCube1_normal,
    0,
    modelCube1_color,
    12
};
/* ========================================== */

#define VMODE       0
#define SCREENXRES 320
#define SCREENYRES 240
#define CENTERX     SCREENXRES/2
#define CENTERY     SCREENYRES/2
#define OTLEN       2048        // Maximum number of OT entries
#define PRIMBUFFLEN 32768       // Maximum number of POLY_GT3 primitives

// Display and draw environments, double buffered
DISPENV disp[2];
DRAWENV draw[2];

// Ordering table (contains addresses to primitives)
u_long      ot[2][OTLEN];

// Primitive list
// That's our prim buffer
char primbuff[2][PRIMBUFFLEN];
// Primitive counter
char *nextpri = primbuff[0];

// Current buffer counter
short db  = 0;

// Prototypes
void init(void);
void display(void);

void
init(void)
{
    // Reset the GPU before doing anything and the controller
    PadInit(0);
    ResetGraph(0);
    
    // Initialize and setup the GTE
    InitGeom();
    // x, y offset
    SetGeomOffset(CENTERX, CENTERY);
    // Distance between eye and screen
    SetGeomScreen(CENTERX);
    // Set the display and draw environments
    SetDefDispEnv(&disp[0], 0, 0         , SCREENXRES, SCREENYRES);
    SetDefDispEnv(&disp[1], 0, SCREENYRES, SCREENXRES, SCREENYRES);
    SetDefDrawEnv(&draw[0], 0, SCREENYRES, SCREENXRES, SCREENYRES);
    SetDefDrawEnv(&draw[1], 0, 0, SCREENXRES, SCREENYRES);
    
    if (VMODE) {
        SetVideoMode(MODE_PAL);
        disp[0].screen.y += 8;
        disp[1].screen.y += 8;
    }

    // Display on screen
    SetDispMask(1);
    setRGB0(&draw[0], 0, 128, 255);
    setRGB0(&draw[1], 0, 128, 255);
    draw[0].isbg = 1;
    draw[1].isbg = 1;
    PutDispEnv(&disp[db]);
    PutDrawEnv(&draw[db]);
    
    // Init font system
    FntLoad(960, 0);
    FntOpen(16, 16, 196, 64, 0, 256);
}

void
display(void)
{
    DrawSync(0);
    VSync(0);
    PutDispEnv(&disp[db]);
    PutDrawEnv(&draw[db]);
    DrawOTag(&ot[db][OTLEN - 1]);
    db = !db;
    nextpri = primbuff[db];
}

int
main(void)
{
    int     i;
    int     PadStatus;
    int     TPressed=0;
    int     AutoRotate=1;

    // t == vertex count
    // p == depth cueing interpolation value
    // OTz ==  value to create Z-ordered OT
    // Flag == see LibOver47.pdf, p.143
    long    t, p, OTz, Flag;

    // pointer to a POLY_G4
    POLY_G3 *poly = {0};

    // Rotation coordinates
    SVECTOR Rotate={ 232, 232, 0, 0 };

    // Translation coordinates
    VECTOR Trans = { 0, 0, CENTERX * 3, 0 };

    // Scaling coordinates
    VECTOR Scale = { ONE/2, ONE/2, ONE/2, 0 }; // ONE == 4096
    
    // Matrix data for the GTE
    MATRIX Matrix = {0};
    
    init();

    while (1) {
        // Read pad status
        PadStatus = PadRead(0);
        
        if (AutoRotate == 0) {
            if (PadStatus & PADL1)      Trans.vz -= 4;
            if (PadStatus & PADR1)      Trans.vz += 4;
            if (PadStatus & PADL2)      Rotate.vz -= 8;
            if (PadStatus & PADR2)      Rotate.vz += 8;
            if (PadStatus & PADLup)     Rotate.vx -= 8;
            if (PadStatus & PADLdown)   Rotate.vx += 8;
            if (PadStatus & PADLleft)   Rotate.vy -= 8;
            if (PadStatus & PADLright)  Rotate.vy += 8;
            if (PadStatus & PADRup)     Trans.vy -= 2;
            if (PadStatus & PADRdown)   Trans.vy += 2;
            if (PadStatus & PADRleft)   Trans.vx -= 2;
            if (PadStatus & PADRright)  Trans.vx += 2;
            if (PadStatus & PADselect) {
                Rotate.vx = Rotate.vy = Rotate.vz = 0;
                Scale.vx = Scale.vy = Scale.vz = ONE/2;
                Trans.vx = Trans.vy = 0;
                Trans.vz = CENTERX * 3;
            }
        }
        
        if (PadStatus & PADstart) {
            if (TPressed == 0) {
                AutoRotate = (AutoRotate + 1) & 1;
                Rotate.vx = Rotate.vy = Rotate.vz = 0;
                Scale.vx = Scale.vy = Scale.vz = ONE/2;
                Trans.vx = Trans.vy = 0;
                Trans.vz = CENTERX * 3;
            }
            TPressed = 1;
        } else {
            TPressed = 0;
        }
        
        if (AutoRotate) {
            Rotate.vy += 28; // Pan
            Rotate.vx += 28; // Tilt
            // Rotate.vz += 8; // Roll
        }
        
        // Clear the current OT
        ClearOTagR(ot[db], OTLEN);
        
        // Convert and set the matrixes
        RotMatrix(&Rotate, &Matrix);
        TransMatrix(&Matrix, &Trans);
        ScaleMatrix(&Matrix, &Scale);
        SetRotMatrix(&Matrix);
        SetTransMatrix(&Matrix);
        
        // Render the sample vector model
        t=0;
        // modelCube is a TMESH
        // len member == # vertices, but here it's # of triangle...
        // So, for each tri * 3 vertices ...
        for (i = 0; i < (modelCube.len*3); i += 3) {               
            poly = (POLY_G3 *)nextpri;
            
            // Initialize the primitive and set its color values
            SetPolyG3(poly);
            setRGB0(poly,
                    modelCube.c[i].r,
                    modelCube.c[i].g,
                    modelCube.c[i].b);
            setRGB1(poly,
                    modelCube.c[i+2].r,
                    modelCube.c[i+2].g,
                    modelCube.c[i+2].b);
            setRGB2(poly,
                    modelCube.c[i+1].r,
                    modelCube.c[i+1].g,
                    modelCube.c[i+1].b);
            
            // Rotate, translate, and project the vectors and output the
            // results into a primitive
            OTz = RotTransPers(
                &modelCube_mesh[modelCube_index[t]],
                (long*)&poly->x0, &p, &Flag);
            OTz += RotTransPers(
                &modelCube_mesh[modelCube_index[t+2]],
                (long*)&poly->x1, &p, &Flag);
            OTz += RotTransPers(
                &modelCube_mesh[modelCube_index[t+1]],
                (long*)&poly->x2, &p, &Flag);
            
            // Sort the primitive into the OT
            OTz /= 3;
            if ((OTz > 0) && (OTz < OTLEN))
                AddPrim(&ot[db][OTz-2], poly);
            nextpri += sizeof(POLY_G3);
            t += 3;
        }
        
        FntPrint("Hello gouraud shaded cube!\n");
        FntFlush(-1);
        display();
    }

    return 0;
}
