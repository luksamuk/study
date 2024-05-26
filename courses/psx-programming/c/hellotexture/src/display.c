#include "display.h"
#include "globals.h"
#include <libetc.h>

static DoubleBuff screen;
static u_short currbuff;

DoubleBuff *
get_screen()
{
    return &screen;
}

u_short
get_curr_buffer()
{
    return currbuff;
}

void
swap_buffers()
{
    currbuff = !currbuff;
}

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
screen_draw()
{
    DrawSync(0);
    VSync(0);

    PutDispEnv(&screen.disp[currbuff]);
    PutDrawEnv(&screen.draw[currbuff]);
}

void
display_frame()
{
    screen_draw();

    // Sort objects in ordering table
    DrawOTag(get_ot_at(get_curr_buffer(), OT_LEN - 1));

    swap_buffers();

    // Reset next primitive pointer to the start of the primitive buffer
    reset_next_prim(get_curr_buffer());
}