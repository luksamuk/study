#include <stdlib.h>
#include <libgte.h>
#include <libetc.h>
#include <libgpu.h>

#define VIDEO_MODE 0
#define SCREEN_RES_X 320
#define SCREEN_RES_Y 240
#define SCREEN_CENTER_X (SCREEN_RES_X >> 1)
#define SCREEN_CENTER_Y (SCREEN_RES_Y >> 1)
#define SCREEN_Z 320

#define OT_LENGTH    2048

#define NUM_VERTICES  8
#define NUM_FACES    12

#define NUM_QUAD_FACES 6
#define NUM_FLOOR_FACES 2

SVECTOR vertices[] = {
    { -64, -64, -64 },
    {  64, -64, -64 },
    {  64, -64,  64 },
    { -64, -64,  64 },
    { -64,  64, -64 },
    {  64,  64, -64 },
    {  64,  64,  64 },
    { -64,  64,  64 }
};

short faces[] = {
    0, 3, 2, // top
    0, 2, 1,
    4, 0, 1, // front
    4, 1, 5,
    7, 4, 5, // bottom
    7, 5, 6,
    5, 1, 2, // right
    5, 2, 6,
    2, 3, 7, // back
    2, 7, 6,
    0, 4, 7, // left
    0, 7, 3
};

short quad_faces[] = {
    2, 1, 3, 0, // top
    1, 5, 0, 4, // front
    5, 6, 4, 7, // bottomn
    2, 6, 1, 5, // right
    7, 6, 3, 2, // back
    7, 3, 4, 0  // left
};

SVECTOR floor_vertices[] = {
    { -900,  0, -900 },
    { -900,  0,  900 },
    {  900,  0, -900 },
    {  900,  0,  900 }
};

short floor_faces[] = {
    0, 1, 2,
    1, 3, 2
};

typedef struct {
    DRAWENV draw[2];
    DISPENV disp[2];
} DoubleBuff;

DoubleBuff screen;
short currbuff;

u_long ot[2][OT_LENGTH];
char primbuff[2][2048];
char *nextprim;

POLY_G3 *poly;
POLY_G4 *qpoly;

SVECTOR rotation    = {0, 0, 0};
VECTOR  translation = {-160, 0, 900};
VECTOR  scale       = {ONE, ONE, ONE};

SVECTOR quadrot     = {0, 0, 0};

MATRIX  world = {0};

VECTOR vel = {0, 0, 0};
VECTOR acc = {0, 0, 0};
VECTOR pos = {0, 0, 0};

SVECTOR floor_rotation    = {0, 0, 0};
VECTOR  floor_translation = {0, 450, 1800};
VECTOR  floor_scale       = {ONE, ONE, ONE};

void
screen_init(void)
{
    // Reset GPU
    ResetGraph(0);

    // Set display and draw areas of buffers
    SetDefDispEnv(&screen.disp[0], 0, 0, SCREEN_RES_X, SCREEN_RES_Y);
    SetDefDrawEnv(&screen.draw[0], 0, 240, SCREEN_RES_X, SCREEN_RES_Y);
    SetDefDispEnv(&screen.disp[1], 0, 240, SCREEN_RES_X, SCREEN_RES_Y);
    SetDefDrawEnv(&screen.draw[1], 0, 0, SCREEN_RES_X, SCREEN_RES_Y);

    // Set draw buffers as background buffers
    screen.draw[0].isbg = 1;
    screen.draw[1].isbg = 1;

    // Set background clear color
    setRGB0(&screen.draw[0], 63, 0, 127);
    setRGB0(&screen.draw[1], 63, 0, 127);

    // Set current initial buffer
    currbuff = 0;
    PutDispEnv(&screen.disp[currbuff]);
    PutDrawEnv(&screen.draw[currbuff]);

    // Initialize and setup the GTE geometry offsets
    InitGeom();
    SetGeomOffset(SCREEN_CENTER_X, SCREEN_CENTER_Y);
    SetGeomScreen(SCREEN_Z);

    // Enable display
    SetDispMask(1);
}

void
display_frame(void)
{
    DrawSync(0);
    VSync(0);

    PutDispEnv(&screen.disp[currbuff]);
    PutDrawEnv(&screen.draw[currbuff]);

    // Sort objects in ordering table
    DrawOTag(ot[currbuff] + OT_LENGTH - 1);

    currbuff = !currbuff;

    // Reset next primitive pointer to the start of the primitive buffer
    nextprim = primbuff[currbuff];
}

void
setup(void)
{
    screen_init();

    // Reset next primitive pointer to the start of the primitive buffer
    nextprim = primbuff[currbuff];

    acc.vx = 0;
    acc.vy = 1;
    acc.vz = 0;

    vel.vx = 0;
    vel.vy = 0;
    vel.vy = 0;

    pos.vx = 320;
    pos.vy = -400;
    pos.vz = 1800;
}

void
update(void)
{
    int i, nclip;
    long otz, p, flg;

    // Empty the ordering table
    ClearOTagR(ot[currbuff], OT_LENGTH);

    vel.vx += acc.vx;
    vel.vy += acc.vy;
    vel.vz += acc.vz;

    pos.vx += (vel.vx >> 1);
    pos.vy += (vel.vy >> 1);
    pos.vz += (vel.vz >> 1);

    if(pos.vy + 128 > floor_translation.vy) {
        pos.vy = floor_translation.vy - 128;
        vel.vy = -60;
    }

    /* Cube rendering with triangles */
    // Populate world matrix with rotation, translation, scale values
    RotMatrix(&rotation, &world);
    TransMatrix(&world, &translation);
    ScaleMatrix(&world, &scale);

    // Sets world matrix to be used by the GTE for rotation and translation
    SetRotMatrix(&world);
    SetTransMatrix(&world);

    // Loop over all triangle faces
    for(i = 0; i < NUM_FACES * 3; i += 3) {
        poly = (POLY_G3*)nextprim;
        setPolyG3(poly);
        setRGB0(poly, 255, 0, 255);
        setRGB1(poly, 255, 255, 0);
        setRGB2(poly, 0, 255, 255);

        nclip = RotAverageNclip3(
            &vertices[faces[i + 0]],
            &vertices[faces[i + 1]],
            &vertices[faces[i + 2]],
            (long*)&poly->x0,
            (long*)&poly->x1,
            (long*)&poly->x2,
            &p, &otz, &flg);

        if(nclip <= 0) continue;

        if((otz > 0) && (otz < OT_LENGTH)) {
            addPrim(ot[currbuff][otz], poly);
            nextprim += sizeof(POLY_G3);
        }
    }

    /* Cube rendering with quads */
    RotMatrix(&quadrot, &world);
    TransMatrix(&world, &pos);
    ScaleMatrix(&world, &scale);
    SetRotMatrix(&world);
    SetTransMatrix(&world);

    for(i = 0; i < NUM_QUAD_FACES * 4; i += 4) {
        qpoly = (POLY_G4*)nextprim;
        setPolyG4(qpoly);
        setRGB0(qpoly, 255, 0, 0);
        setRGB1(qpoly, 0, 255, 0);
        setRGB2(qpoly, 0, 0, 255);
        setRGB3(qpoly, 255, 255, 255);

        nclip = RotAverageNclip4(
            &vertices[quad_faces[i + 0]],
            &vertices[quad_faces[i + 1]],
            &vertices[quad_faces[i + 2]],
            &vertices[quad_faces[i + 3]], 
            (long*)&qpoly->x0,
            (long*)&qpoly->x1,
            (long*)&qpoly->x2,
            (long*)&qpoly->x3,
            &p, &otz, &flg);
        
        if(nclip <= 0) continue;

        if((otz > 0) && (otz < OT_LENGTH)) {
            addPrim(ot[currbuff][otz], qpoly);
            nextprim += sizeof(POLY_G4);
        }
    }

    /* Floor rendering with triangles */
    RotMatrix(&floor_rotation, &world);
    TransMatrix(&world, &floor_translation);
    ScaleMatrix(&world, &floor_scale);
    SetRotMatrix(&world);
    SetTransMatrix(&world);

    for(i = 0; i < NUM_FLOOR_FACES * 3; i += 3) {
        poly = (POLY_G3*)nextprim;
        setPolyG3(poly);
        setRGB0(poly, 128, 128, 0);
        setRGB1(poly, 0, 128, 128);
        setRGB2(poly, 128, 0, 128);

        nclip = RotAverageNclip3(
            &floor_vertices[floor_faces[i + 0]],
            &floor_vertices[floor_faces[i + 1]],
            &floor_vertices[floor_faces[i + 2]],
            (long*)&poly->x0,
            (long*)&poly->x1,
            (long*)&poly->x2,
            &p, &otz, &flg);

        if(nclip <= 0) continue;

        if((otz > 0) && (otz < OT_LENGTH)) {
            addPrim(ot[currbuff][otz], poly);
            nextprim += sizeof(POLY_G3);
        }
    }

    rotation.vx += 6;
    rotation.vy += 8;
    rotation.vz += 12;

    quadrot.vx -= 6;
    quadrot.vy -= 8;
    quadrot.vz -= 12;

    floor_rotation.vy += 5;
}

void
render(void)
{
    display_frame();
}

int
main(void)
{
    setup();

    while(1) {
        update();
        render();
    }

    return 0;
}
