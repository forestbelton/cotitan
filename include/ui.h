/* Copyright (c) 2012 Forest Belton (apples)
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in
* all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
* THE SOFTWARE.
*/
#ifndef COTITAN_UI_H_
#define COTITAN_UI_H_

#include<stdbool.h>

#define GAMEWIN_W 0.7
#define GAMEWIN_H 0.75
#define SIDEWIN_W (1-GAMEWIN_W)
#define SIDEWIN_H GAMEWIN_H
#define MSGWIN_W 1
#define MSGWIN_H (1-GAMEWIN_H)


/* Initializes the user interface. */
void ui_init     (void);

/* Give control of the thread to the UI. For mtime milliseconds. Setting this to 0 means keep going 'till user exits. */
void ui_interact(int mtime);

/* Places the character c at the coordinate (x,y) on the world map. */
void ui_place_obj(int c, int x, int y);

/* Prints a formatted message to the event log. */
void ui_printf   (int color, const char *msg, ...);

/* Prompts the user to enter a line. */
/* Note: For the sake of flexibility, this is placed on the heap. Don't forget to free() */
char *ui_prompt  (int echo, const char *msg);

/* Cleans up user interface */
void ui_destroy(void);

#endif

