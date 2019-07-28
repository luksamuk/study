/*
 *  problem-20180929.c
 *  Copyright © 2018 Lucas Vieira <lucasvieira@lisp.com.br>
 *  
 *  Licensed under the MIT License.
 *  Permission is hereby granted, free of charge, to any person obtaining a
 *  copy of this software and associated documentation files (the "Software"),
 *  to deal in the Software without restriction, including without limitation
 *  the rights to use, copy, modify, merge, publish, distribute, sublicense,
 *  and/or sell copies of the Software, and to permit persons to whom the
 *  Software is furnished to do so, subject to the following conditions:
 *   
 *  The above copyright notice and this permission notice shall be included in
 *  all copies or substantial portions of the Software.
 *   
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 *  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 *  DEALINGS IN THE SOFTWARE.
**/

// This solves a programming problem described in r/C_Programming.
//
// User inputs a starting value A, step H and the lower limit of function
// value YM. The following conditions have to be true: H > 0.
// The function value y will be calculated in the following points:
// A
// A + H
// A + 2H
// A + 3H
// ...
// while the condition y > YM holds true, however not more than 15 times.

#include <stdio.h>
#include <assert.h>

float
calc_y(float x)
{
    return ((6.0f * x * x) + (29.0f * x) + 14.0f)
        / (32.0f - (x * x * x * x * x));
}

int
main(void)
{
    float a, h, ym;
    scanf("%f %f %f", &a, &h, &ym);
    assert(h > 0);

    float y;
    int iteration = 0;
    do {
        float x = a + (h * iteration);
        y = calc_y(x);
        printf("x: %f -> y: %f\n", x, y);
        iteration++;
    } while((y > ym) && (iteration < 15));
    return 0;
}

