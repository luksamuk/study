#include <stdlib.h>
#include <libgte.h>
#include <libetc.h>
#include <libgpu.h>

#define VIDEO_MODE 0
#define SCREEN_RES_X 320
#define SCREEN_RES_Y 240
#define SCREEN_CENTER_X (SCREEN_RES_X >> 1)
#define SCREEN_CENTER_Y (SCREEN_RES_Y >> 1)
#define SCREEN_Z 400

#define OT_LENGTH 16

typedef struct {
    DRAWENV draw[2];
    DISPENV disp[2];
} DoubleBuff;

DoubleBuff screen;
short currbuff;

u_long ot[2][OT_LENGTH];
char primbuff[2][2048];
char *nextprim;

POLY_F3 *triangle;
TILE    *tile;
POLY_G4 *quadg4;

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
}

void
update(void)
{
    ClearOTagR(ot[currbuff], OT_LENGTH);

    tile = (TILE*)nextprim;
    setTile(tile);
    setXY0(tile, 82, 32);
    setWH(tile, 64, 64);
    setRGB0(tile, 0, 255, 0);
    addPrim(ot[currbuff], tile);
    nextprim += sizeof(TILE);

    triangle = (POLY_F3*)nextprim;
    setPolyF3(triangle);
    setXY3(triangle, 64, 100, 200, 150, 50, 220);
    setRGB0(triangle, 255, 0, 255);
    addPrim(ot[currbuff], triangle);
    nextprim += sizeof(POLY_F3);

    quadg4 = (POLY_G4*)nextprim;
    setPolyG4(quadg4);
    setXY4(quadg4, 240, 60, 240, 180, 80, 60, 80, 180); // inverted N
    setRGB0(quadg4, 255, 0, 0);
    setRGB1(quadg4, 0, 255, 0);
    setRGB2(quadg4, 0, 0, 255);
    setRGB3(quadg4, 255, 255, 255);
    addPrim(ot[currbuff], quadg4);
    nextprim += sizeof(POLY_G4);
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
