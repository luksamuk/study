#ifndef CAMERA_H
#define CAMERA_H

#include <libgpu.h>

typedef struct {
    VECTOR  position;
    SVECTOR rotation;
    MATRIX  lookat;
} Camera;

void look_at(Camera *camera, VECTOR *eye, VECTOR *target, VECTOR *up);

#endif