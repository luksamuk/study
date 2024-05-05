// Heavily inspired by the hello_world tutorial
// found at: https://github.com/ABelliqueux/nolibgs_hello_worlds

#include <sys/types.h>
#include <stdio.h>
#include <libgte.h>
#include <libetc.h>
#include <libgpu.h>

#define VMODE 0                 // Video Mode : 0 : NTSC, 1: PAL
#define SCREENXRES 320          // Screen width
#define SCREENYRES 240          // Screen height
#define CENTERX SCREENXRES/2    // Center of screen on x 
#define CENTERY SCREENYRES/2    // Center of screen on y
#define MARGINX 0                // margins for text display
#define MARGINY 32
#define FONTSIZE 8 * 7           // Text Field Height

DISPENV disp[2];                 // Double buffered DISPENV and DRAWENV
DRAWENV draw[2];
short db = 0;                      // index of which buffer is used, values 0, 1

void
init(void)
{
    // Initialize drawing engine with a complete reset (0)
    ResetGraph(0);
    // Set display area for both &disp[0] and &disp[1]
    SetDefDispEnv(&disp[0], 0, 0, SCREENXRES, SCREENYRES);
    // &disp[0] is on top  of &disp[1]
    SetDefDispEnv(&disp[1], 0, SCREENYRES, SCREENXRES, SCREENYRES);
    // Set draw for both &draw[0] and &draw[1]
    SetDefDrawEnv(&draw[0], 0, SCREENYRES, SCREENXRES, SCREENYRES);
    // &draw[0] is below &draw[1]
    SetDefDrawEnv(&draw[1], 0, 0, SCREENXRES, SCREENYRES);
    if (VMODE) // PAL
    {
        SetVideoMode(MODE_PAL);
        disp[0].screen.y += 8;  // add offset : 240 + 8 + 8 = 256
        disp[1].screen.y += 8;
    }

    // Display on screen
    SetDispMask(1);
    // set color for first draw area
    setRGB0(&draw[0], 50, 50, 50);
    // set color for second draw area
    setRGB0(&draw[1], 50, 50, 50);
    // set mask for draw areas. 1 means repainting the area with the RGB color each frame 
    draw[0].isbg = 1;
    draw[1].isbg = 1;
    // set the disp and draw environnments
    PutDispEnv(&disp[db]);
    PutDrawEnv(&draw[db]);
    // Load font to vram at 960,0(+128)
    FntLoad(960, 0);
    // FntOpen(x, y, width, height,  black_bg, max. nbr. chars
    FntOpen(MARGINX,
            //SCREENYRES - MARGINY - FONTSIZE,
            20,
            SCREENXRES - MARGINX * 2,
            FONTSIZE,
            0,
            280 );
}


void
display(void)
{
    // Wait for all drawing to terminate
    DrawSync(0);
    // Wait for the next vertical blank
    VSync(0);
    // set alternate disp and draw environnments
    PutDispEnv(&disp[db]);
    PutDrawEnv(&draw[db]);
    // flip db value (0 or 1)
    db = !db;
}

int
main(void)
{
    init();
    while(1) {
        FntPrint("Hello, world!\n\n"
                 "Running C code on PlayStation.\n\n"
                 "Very nice!");
        // Draw printe stream
        FntFlush(-1);
        display();
    }

    return 0;
}
