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
#include "object.h"

extern char __heap_start, __sp;


Camera camera;
Object object;

MATRIX  world = {0};
MATRIX  view  = {0};

POLY_FT4 *poly;

u_long timmode;  // Pixel mode of the TIM
RECT   timprect; // Rectangle of texture
RECT   timcrect; // Rectangle of texture CLUT

void
load_model(char *filename)
{
    char *bytes;
    u_long length;

    bytes = file_read(filename, &length);
    printf("Read %lu bytes from %s (ptr %p)\n", length, filename, bytes);

    u_long b = 0; // Counter of bytes

    object.numverts = get_short_be((u_char *) bytes, &b);
    object.vertices = malloc3(object.numverts * sizeof(SVECTOR));
    for(u_long i = 0; i < object.numverts; i++) {
        object.vertices[i].vx = get_short_be((u_char *) bytes, &b);
        object.vertices[i].vy = get_short_be((u_char *) bytes, &b);
        object.vertices[i].vz = get_short_be((u_char *) bytes, &b);
        printf("VERTEX %ld, X=%d, Y=%d, Z=%d\n",
            i,
            object.vertices[i].vx,
            object.vertices[i].vy,
            object.vertices[i].vz);
    }

    object.numfaces = get_short_be((u_char *) bytes, &b);
    object.faces = malloc3(object.numfaces * 4 * sizeof(short));
    for(u_long i = 0; i < object.numfaces * 4; i += 4) {
        object.faces[i] = get_short_be((u_char *) bytes, &b);
        object.faces[i + 1] = get_short_be((u_char *) bytes, &b);
        object.faces[i + 2] = get_short_be((u_char *) bytes, &b);
        object.faces[i + 3] = get_short_be((u_char *) bytes, &b);
        printf("FACES %ld ~ %ld: %d, %d, %d, %d\n",
            i, i + 3,
            object.faces[i],
            object.faces[i + 1],
            object.faces[i + 2],
            object.faces[i + 3]);
    }

    object.numcolors = (short) get_byte((u_char *) bytes, &b);
    object.colors = malloc3(object.numcolors * sizeof(CVECTOR));
    for(u_long i = 0; i < object.numcolors; i++) {
        object.colors[i].r = get_byte((u_char *) bytes, &b);
        object.colors[i].g = get_byte((u_char *) bytes, &b);
        object.colors[i].b = get_byte((u_char *) bytes, &b);
        object.colors[i].cd = get_byte((u_char *) bytes, &b);
        printf("COLOR %ld: R=%d G=%d B=%d CD=%d\n",
            i,
            object.colors[i].r,
            object.colors[i].g,
            object.colors[i].b,
            object.colors[i].cd);
    }

    free(bytes);
}

void
load_texture(char *filename)
{
    u_long *bytes;
    u_long length;
    TIM_IMAGE tim;

    bytes = (u_long *)file_read(filename, &length);
    printf("Read %lu bytes from %s (ptr %p)\n", length, filename, bytes);

    OpenTIM(bytes);
    ReadTIM(&tim);

    LoadImage(tim.prect, tim.paddr);
    DrawSync(0);

    if(tim.mode & 0x8) {
        LoadImage(tim.crect, tim.caddr);
        DrawSync(0);
    }

    timmode = tim.mode;
    timprect = *tim.prect;
    timcrect = *tim.crect;

    free(bytes);
}

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

    setVector(&object.position, 0, 0, 0);
    setVector(&object.rotation, 0, 0, 0);
    setVector(&object.scale, ONE, ONE, ONE);

    load_model("\\MODEL.BIN;1");
    load_texture("\\BRICKS.TIM;1");
}

void
update(void)
{
    int nclip;
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

    // Look at object
    look_at(&camera, &camera.position, &object.position, &(VECTOR){0, -ONE, 0});

    /* Object rendering */
    RotMatrix(&object.rotation, &world);
    TransMatrix(&world, &object.position);
    ScaleMatrix(&world, &object.scale);
    CompMatrixLV(&camera.lookat, &world, &view);
    SetRotMatrix(&view);
    SetTransMatrix(&view);

    for(int i = 0, j = 0; i < object.numfaces * 4; i += 4, j++) {
        poly = (POLY_FT4*)get_next_prim();
        setPolyFT4(poly);
        setRGB0(poly, 128, 128, 128);

        poly->u0 = 0;   poly->v0 = 0;
        poly->u1 = 63;  poly->v1 = 0;
        poly->u2 = 0;   poly->v2 = 63;
        poly->u3 = 63;  poly->v3 = 63;

        poly->tpage = getTPage(timmode & 0x3, 0, timprect.x, timprect.y);
        poly->clut = getClut(timcrect.x, timcrect.y);

        // Inline GTE quad calls
        gte_ldv0(&object.vertices[object.faces[i + 0]]);
        gte_ldv1(&object.vertices[object.faces[i + 1]]);
        gte_ldv2(&object.vertices[object.faces[i + 2]]);
        gte_rtpt();
        gte_nclip();
        gte_stopz(&nclip);
        if(nclip <= 0) continue;
        gte_stsxy0(&poly->x0);

        gte_ldv0(&object.vertices[object.faces[i + 3]]);
        gte_rtps();
        gte_stsxy3(&poly->x1, &poly->x2, &poly->x3);
        gte_avsz4();
        gte_stotz(&otz);

        if((otz > 0) && (otz < OT_LEN)) {
            addPrim(get_ot_at(get_curr_buffer(), otz), poly);
            increment_next_prim(sizeof(POLY_FT4));
        }
    }

    object.rotation.vy += 20;
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
