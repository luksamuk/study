#include <pspkernel.h>
#include <pspctrl.h>
#include <pspdebug.h>
#include <pspgu.h>
#include <pspdisplay.h>

//PSP_MODULE_INFO is required
PSP_MODULE_INFO("Hello World", 0, 1, 0);
PSP_MAIN_THREAD_ATTR(PSP_THREAD_ATTR_VFPU | PSP_THREAD_ATTR_USER);

#define BUFFER_WIDTH  512
#define BUFFER_HEIGHT 272
#define SCREEN_WIDTH  480
#define SCREEN_HEIGHT BUFFER_HEIGHT

typedef struct {
    unsigned short u, v;
    short x, y, z;
} vec2;


char list[0x20000] __attribute__((aligned(64)));

void
initGu()
{
    sceGuInit();

    // Setup buffers
    sceGuStart(GU_DIRECT, list);
    sceGuDrawBuffer(GU_PSM_8888, (void*)0, BUFFER_WIDTH);
    sceGuDispBuffer(SCREEN_WIDTH, SCREEN_HEIGHT, (void*)0x88000, BUFFER_WIDTH);
    sceGuDepthBuffer((void*)0x110000, BUFFER_WIDTH);

    // Setup viewport
    sceGuOffset(2048 - (SCREEN_WIDTH / 2), 2048 - (SCREEN_HEIGHT / 2));
    sceGuViewport(2048, 2048, SCREEN_WIDTH, SCREEN_HEIGHT);
    sceGuEnable(GU_SCISSOR_TEST);
    sceGuScissor(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT);

    // Use full buffer for depth testing (buffer is reversed)
    sceGuDepthRange(65535, 0);
    //sceGuDepthFunc(GU_GEQUAL); // Depth buffer reversed -> GEQUAL instead of LEQUAL
    //sceGuEnable(GU_DEPTH_TEST);

    sceGuFinish();
    sceGuDisplay(GU_TRUE);
}

void
endGu()
{
    sceGuDisplay(GU_FALSE);
    sceGuTerm();
}

int
exit_callback(int arg1, int arg2, void *common)
{
    endGu();
    sceKernelExitGame();
    return 0;
}

int
callback_thread(SceSize args, void *argp)
{
    int callback_id =
        sceKernelCreateCallback("Exit Callback", exit_callback, NULL);
    sceKernelRegisterExitCallback(callback_id);
    sceKernelSleepThreadCB();
    return 0;
}

int
setup_callbacks(void)
{
    int thread_id =
        sceKernelCreateThread("update_thread", callback_thread, 0x11, 0xFA0, 0, 0);
    if(thread_id >= 0) sceKernelStartThread(thread_id, 0, 0);
    return thread_id;
}

void
drawRect(float x, float y, float w, float h)
{
    // This is not permanent memory allocation. The memory will be invalid
    // as soon as the same display list starts being filled again, so no
    // need to deallocate stuff here
    vec2 *vertices = (vec2*)sceGuGetMemory(2 * sizeof(vec2));

    vertices[0].x = x;
    vertices[0].y = y;
    vertices[1].x = x + w;
    vertices[1].y = y + h;

    sceGuColor(0xff0000ff); // red (ABGR)
    sceGuDrawArray(GU_SPRITES, GU_TEXTURE_16BIT | GU_VERTEX_16BIT | GU_TRANSFORM_2D, 2, 0, vertices);
}

int
main(void)
{
    setup_callbacks();
    pspDebugScreenInit();
    initGu();

    static float xpos = 32.0f;
    static float ypos = 32.0f;
    static float speed = 5.0f;

    SceCtrlData pad;
    sceCtrlSetSamplingCycle(0);
    sceCtrlSetSamplingMode(PSP_CTRL_MODE_ANALOG);
    
    while(1) {
        sceGuStart(GU_DIRECT, list);
        sceGuClearColor(0xff000000); // black background (ABGR)
        sceGuClear(GU_COLOR_BUFFER_BIT);

        sceCtrlReadBufferPositive(&pad, 1);

        if(pad.Buttons != 0) {
            if(pad.Buttons & PSP_CTRL_UP) ypos -= speed;
            if(pad.Buttons & PSP_CTRL_DOWN) ypos += speed;
            if(pad.Buttons & PSP_CTRL_LEFT) xpos -= speed;
            if(pad.Buttons & PSP_CTRL_RIGHT) xpos += speed;
        }

        drawRect(xpos, ypos, 64, 64);

        pspDebugScreenSetXY(0, 0);
        //pspDebugScreenPrintf("X = %0.2f\nY = %0.2f", xpos, ypos);
        pspDebugScreenPrintf("X = %0.2f", xpos);
        pspDebugScreenSetXY(0, 1);
        pspDebugScreenPrintf("Y = %0.2f", ypos);
        
        sceGuFinish();
        sceGuSync(0, 0);
        
        sceDisplayWaitVblankStart();
        sceGuSwapBuffers();
    }

    return 0;
}
