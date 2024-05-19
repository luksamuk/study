#include <stdlib.h>
#include <libgte.h>
#include <libgpu.h>

#include <inline_n.h> // GTE inline calls

#include "globals.h"
#include "display.h"
#include "joypad.h"

#define NUM_VERTICES  8
#define NUM_FACES    12

#define NUM_QUAD_FACES 6
#define NUM_FLOOR_FACES 2

SVECTOR vertices[] = {
    { -128, -128, -128 },
    {  128, -128, -128 },
    {  128, -128,  128 },
    { -128, -128,  128 },
    { -128,  128, -128 },
    {  128,  128, -128 },
    {  128,  128,  128 },
    { -128,  128,  128 }
};

short faces[] = {
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

POLY_F3 *poly;
POLY_G4 *qpoly;

SVECTOR rotation    = {0, 0, 0};
VECTOR  scale       = {ONE, ONE, ONE};

MATRIX  world = {0};

VECTOR vel = {0, 0, 0};
VECTOR acc = {0, 0, 0};
VECTOR pos = {0, 0, 0};

SVECTOR floor_rotation    = {0, 0, 0};
VECTOR  floor_translation = {0, 450, 1800};
VECTOR  floor_scale       = {ONE, ONE, ONE};

void
setup(void)
{
    screen_init();
    joypad_init();

    // Reset next primitive pointer to the start of the primitive buffer
    reset_next_prim(get_curr_buffer());

    acc.vx = 0;
    acc.vy = 1;
    acc.vz = 0;

    vel.vx = 0;
    vel.vy = 0;
    vel.vy = 0;

    pos.vx = 0;
    pos.vy = -400;
    pos.vz = 1400;
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
        rotation.vy += 24;
    }

    if(joypad_check(PAD1_RIGHT)) {
        rotation.vy -= 24;
    }

    if(joypad_check(PAD1_UP)) {
        rotation.vx -= 24;
    }

    if(joypad_check(PAD1_DOWN)) {
        rotation.vx += 24;
    }

    vel.vx += acc.vx;
    vel.vy += acc.vy;
    vel.vz += acc.vz;

    pos.vx += (vel.vx >> 1);
    pos.vy += (vel.vy >> 1);
    pos.vz += (vel.vz >> 1);

    if(pos.vy + 128 > floor_translation.vy) {
        pos.vy = floor_translation.vy - 128;
        vel.vy = -45;
    }

    /* Cube rendering with quads */
    RotMatrix(&rotation, &world);
    TransMatrix(&world, &pos);
    ScaleMatrix(&world, &scale);
    SetRotMatrix(&world);
    SetTransMatrix(&world);

    for(i = 0; i < NUM_QUAD_FACES * 4; i += 4) {
        qpoly = (POLY_G4*)get_next_prim();
        setPolyG4(qpoly);
        setRGB0(qpoly, 255, 0, 255);
        setRGB1(qpoly, 255, 255, 0);
        setRGB2(qpoly, 0, 255, 255);
        setRGB3(qpoly, 255, 255, 255);

        // Inline GTE quad calls
        gte_ldv0(&vertices[faces[i + 0]]);
        gte_ldv1(&vertices[faces[i + 1]]);
        gte_ldv2(&vertices[faces[i + 2]]);
        gte_rtpt();
        gte_nclip();
        gte_stopz(&nclip);
        if(nclip <= 0) continue;
        gte_stsxy0(&qpoly->x0);

        gte_ldv0(&vertices[faces[i + 3]]);
        gte_rtps();
        gte_stsxy3(&qpoly->x1, &qpoly->x2, &qpoly->x3);
        gte_avsz4();
        gte_stotz(&otz);

        if((otz > 0) && (otz < OT_LEN)) {
            addPrim(get_ot_at(get_curr_buffer(), otz), qpoly);
            increment_next_prim(sizeof(POLY_G4));
        }
    }

    /* Floor rendering with triangles */
    RotMatrix(&floor_rotation, &world);
    TransMatrix(&world, &floor_translation);
    ScaleMatrix(&world, &floor_scale);
    SetRotMatrix(&world);
    SetTransMatrix(&world);

    for(i = 0; i < NUM_FLOOR_FACES * 3; i += 3) {
        poly = (POLY_F3*)get_next_prim();
        setPolyF3(poly);
        setRGB0(poly, 128, 128, 0);

        // Inline GTE calls
        gte_ldv0(&floor_vertices[floor_faces[i + 0]]);
        gte_ldv1(&floor_vertices[floor_faces[i + 1]]);
        gte_ldv2(&floor_vertices[floor_faces[i + 2]]);
        gte_rtpt();
        gte_nclip();
        gte_stopz(&nclip);

        if(nclip >= 0) {
            // Inline GTE calls
            gte_stsxy3(&poly->x0, &poly->x1, &poly->x2);
            gte_avsz3();
            gte_stotz(&otz);

            if((otz > 0) && (otz < OT_LEN)) {
                addPrim(get_ot_at(get_curr_buffer(), otz), poly);
                increment_next_prim(sizeof(POLY_F3));
            }
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
