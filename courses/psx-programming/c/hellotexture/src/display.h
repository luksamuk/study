#ifndef DISPLAY_H
#define DISPLAY_H

#include <libgpu.h>

#define VIDEO_MODE 0
#define SCREEN_RES_X 320
#define SCREEN_RES_Y 240
#define SCREEN_CENTER_X (SCREEN_RES_X >> 1)
#define SCREEN_CENTER_Y (SCREEN_RES_Y >> 1)
#define SCREEN_Z 320

typedef struct {
    DRAWENV draw[2];
    DISPENV disp[2];
} DoubleBuff;

DoubleBuff *get_screen(void);
u_short     get_curr_buffer(void);
void        swap_buffers(void);

void        screen_init(void);
void        screen_draw(void);

void        display_frame(void);

#endif