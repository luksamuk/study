#include <stdio.h>
#include <libgte.h>
#include <libgpu.h>
#include <libcd.h>
#include <inline_n.h> // GTE inline calls
#include <malloc.h>   // Stack and heap initialization

#include "globals.h"
#include "display.h"
#include "joypad.h"
#include "camera.h"
#include "utils.h"

#define NUM_VERTICES  8
#define NUM_FACES 6

extern char __heap_start, __sp;

typedef struct Cube {
    SVECTOR rotation;
    VECTOR position;
    VECTOR scale;
    VECTOR vel;
    VECTOR acc;
    SVECTOR vertices[8];
    short faces[24];
} Cube;

Cube cube = {
    {0, 0, 0},
    {0, -400, 1800},
    {ONE, ONE, ONE},
    {0, 0, 0},
    {0, 1, 0},
    {
        { -128, -128, -128 },
        {  128, -128, -128 },
        {  128, -128,  128 },
        { -128, -128,  128 },
        { -128,  128, -128 },
        {  128,  128, -128 },
        {  128,  128,  128 },
        { -128,  128,  128 }
    },
    {
        2, 1, 3, 0, // top
        1, 5, 0, 4, // front
        5, 6, 4, 7, // bottomn
        2, 6, 1, 5, // right
        7, 6, 3, 2, // back
        7, 3, 4, 0  // left
    }
};

Camera camera;

POLY_F3 *poly;
POLY_G4 *qpoly;

MATRIX  world = {0};
MATRIX  view  = {0};

void
setup(void)
{
    // Stack and heap initialization
    InitHeap3((unsigned long *) (&__heap_start), (&__sp - 0x5000) - &__heap_start);

    screen_init();
    joypad_init();
    CdInit();

    // Reset next primitive pointer to the start of the primitive buffer
    reset_next_prim(get_curr_buffer());

    camera.position.vx = 500;
    camera.position.vy = -1000; // Y grows down
    camera.position.vz = -1500; // Push the camera back further
    camera.lookat = (MATRIX){0};

    char *buffer;
    u_long length;

    buffer = file_read("\\MODEL.BIN;1", &length);
    printf("Read %lu bytes from MODEL.BIN (ptr %p)\n", length, buffer);
}

void
update(void)
{
    int i, nclip;
    long otz, p, flg;

    // Empty the ordering table
    empty_ot(get_curr_buffer());

    joypad_update();

    if(joypad_check(PAD1_LEFT)) {
        camera.position.vx -= 50;
    }

    if(joypad_check(PAD1_RIGHT)) {
        camera.position.vx += 50;
    }

    if(joypad_check(PAD1_UP)) {
        camera.position.vy -= 50;
    }

    if(joypad_check(PAD1_DOWN)) {
        camera.position.vy += 50;
    }

    if(joypad_check(PAD1_CROSS)) {
        camera.position.vz -= 50;
    }

    if(joypad_check(PAD1_CIRCLE)) {
        camera.position.vz += 50;
    }

    // Look at cube
    look_at(&camera, &camera.position, &cube.position, &(VECTOR){0, -ONE, 0});

    /* Cube rendering with quads */
    RotMatrix(&cube.rotation, &world);
    TransMatrix(&world, &cube.position);
    ScaleMatrix(&world, &cube.scale);
    CompMatrixLV(&camera.lookat, &world, &view);
    SetRotMatrix(&view);
    SetTransMatrix(&view);

    for(i = 0; i < NUM_FACES * 4; i += 4) {
        qpoly = (POLY_G4*)get_next_prim();
        setPolyG4(qpoly);
        setRGB0(qpoly, 255, 0, 255);
        setRGB1(qpoly, 255, 255, 0);
        setRGB2(qpoly, 0, 255, 255);
        setRGB3(qpoly, 255, 255, 255);

        // Inline GTE quad calls
        gte_ldv0(&cube.vertices[cube.faces[i + 0]]);
        gte_ldv0(&cube.vertices[cube.faces[i + 0]]);
        gte_ldv1(&cube.vertices[cube.faces[i + 1]]);
        gte_ldv2(&cube.vertices[cube.faces[i + 2]]);
        gte_rtpt();
        gte_nclip();
        gte_stopz(&nclip);
        if(nclip <= 0) continue;
        gte_stsxy0(&qpoly->x0);

        gte_ldv0(&cube.vertices[cube.faces[i + 3]]);
        gte_rtps();
        gte_stsxy3(&qpoly->x1, &qpoly->x2, &qpoly->x3);
        gte_avsz4();
        gte_stotz(&otz);

        if((otz > 0) && (otz < OT_LEN)) {
            addPrim(get_ot_at(get_curr_buffer(), otz), qpoly);
            increment_next_prim(sizeof(POLY_G4));
        }
    }
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
