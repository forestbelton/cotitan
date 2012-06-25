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
#include "ui.h"
#include "misc.h"

#include <stdlib.h>
#include <ncurses.h>

#define WGAME 0x0
#define WMSG 0x1
#define WSIDE 0x2

static bool ct_winit(WINDOW* w);
static bool ct_wrefresh(int i);
static bool ct_wmvresize(int i, int h, int w, int y, int x);
static bool ct_layout(int,int,int,int);
static bool ct_process(int);
static bool ct_border();
static WINDOW *wnd[5], *wndbox[5];
static int curw, curh, curx, cury;

void ui_init() {
  int i;

  /* Set some global properties */
  initscr();
  if(has_colors()) {
    start_color();
  }
  else {
    DEBUG("Crappy terminal has no colors. Not using any.");
  }
  
  /* No line buffering, take function keys  */
  cbreak();
  noecho();

  /* Make some windows */
  for(i=0;i<3;i++) {
    wnd[i] = newwin(1,1,1,1);
    wndbox[i] = newwin(1,1,1,1);
    ct_winit(wnd[i]);
  }

  /* Set properties of specific windows */  
  scrollok(wnd[WMSG],true);

  /* Default terminal size */
  /* @TODO Find portable way of detecting this */
  ct_layout(25,80,0,0);
  for(i=0;i<3;i++) {
    ct_wrefresh(i);
  }

}

void ui_destroy() {
  int i;
  bool fail;

  fail = false;
  for(i=0;i<3;i++) {
    fail = delwin(wnd[i]) == ERR || delwin(wndbox[i]) == ERR;
    if(fail) break;
  }

  if(fail) DEBUG("Failed to delete ncurses window(s)");
  endwin();
}

void ui_interact(int msec) {
  int total, c, errlvl;
  /* Set mode */
  keypad(wndbox[WGAME],true);
  /* Set delay to n milliseconds */
  if(msec > 0) {
    total = msec;
    wtimeout(wndbox[WGAME],50);
  }
  else{
    total = 0;
  }
  
  errlvl = 0;
  while(1) {
    c = wgetch(wndbox[WGAME]);
    total -= 50;
    if(c == ERR) {
      if(msec > 0) {
	/* Timed out */
	if(total <= 0) break;
      }
      else {
	/* Probably a real error */
	errlvl++;
	if(errlvl >= 5) break;
	break;
      }
    }
    else {
      ct_process(c);
    }
  }
  if(msec > 0) wtimeout(wndbox[WGAME],-1);
}

void ui_place_obj(int c, int x, int y) {
  mvwaddch(wnd[WGAME],y,x,c);
  ct_wrefresh(WGAME);
}

void ui_printf(int color, const char *fmt, ...) {
  va_list vargs;
  attr_t oldattr;
  short oldclr;
  bool clr;
  clr = has_colors();
  
  if(clr) {
    wattr_get(wnd[WMSG], &oldattr, &oldclr, 0);
    init_pair(1, color, COLOR_BLACK);
    wattron(wnd[WMSG],COLOR_PAIR(1));
  }

  va_start(vargs,fmt);

  vwprintw(wnd[WMSG],fmt,vargs);
  waddch(wnd[WMSG],'\n');

  va_end(vargs);

  ct_wrefresh(WMSG);
  if(clr) {
    wattr_set(wnd[WMSG],oldattr,oldclr,0);
  }
}

char *ui_prompt(int doecho, const char *msg) {
  char *store;

  /* Set up modes */
  if(doecho)
    echo();
  else
    noecho();
  nocbreak();
  store = (char*)malloc(sizeof(char)*1024);

  /* First, we need to send the cursor to bottom screen */
  mvwprintw(wnd[WGAME], (int)(GAMEWIN_H*curh)-3, 0, msg);
  ct_wrefresh(WGAME);
  wgetnstr(wnd[WGAME],store,1024);

  /* Clear line */
  wmove(wnd[WGAME], (int)(GAMEWIN_H*curh)-3, 0);
  wclrtobot(wnd[WGAME]);
  ct_wrefresh(WGAME);

  /* Reset mode */
  noecho(); cbreak();

  return store;
}

/* Some bundled window operations (Change both window and border window) */

bool ct_winit(WINDOW* w) {
  if(w == 0)
    return false;

  /* Setup common properties */
  wtimeout(w,-1);
  keypad(w, true);

  return true;
}

bool ct_wrefresh(int i) {
  if(wrefresh(wnd[i]) == ERR || wrefresh(wndbox[i]) == ERR)
    return false;
  return true;
}

bool ct_wmvresize(int i, int h, int w, int y, int x) {
  if(mvwin( wndbox[i], y, x ) == ERR)
    return false;
  if(wresize( wndbox[i], h, w ) == ERR)
    return false;
  if(mvwin( wnd[i], y+1, x+1 ) == ERR)
    return false;
  if(wresize( wnd[i], h-2, w-2 ) == ERR)
    return false;
  return true;
}

bool ct_layout(int h, int w, int y, int x) {
  /* Store the layout paramters */
  curw = w;
  curh = h;
  curx = x;
  cury = y;
  
  /* Calculate actual dimensions based on constant ratios */
  if(ct_wmvresize( WGAME, (int)(GAMEWIN_H*h), (int)(GAMEWIN_W*w), y, x ) == false)
    return false;
  if(ct_wmvresize( WSIDE, (int)(SIDEWIN_H*h), (int)(SIDEWIN_W*w) , y, x+(int)(GAMEWIN_W*w) ) == false)
    return false;
  if(ct_wmvresize( WMSG, (int)(MSGWIN_H*h), (int)(MSGWIN_W*w), y+(int)(GAMEWIN_H*h), x ) == false)
    return false;
  if(ct_border() == false)
    return false;
  return true;
}

bool ct_border() {
  if(box( wndbox[WGAME], 0, 0 ) == ERR)
    return false;
  if(box( wndbox[WSIDE], 0, 0 ) == ERR)
    return false;
  if(box( wndbox[WMSG], 0, 0 ) == ERR)
    return false;
  return true;
}

bool ct_process(int k) {
  /* Handle key press */
  /* @TODO Send event to server etc. */
  return true;
}

