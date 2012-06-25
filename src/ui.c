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
#include<stdlib.h>
#include<stdbool.h>
#include<ncurses.h>

static bool ct_layout(int,int,int,int);
static bool ct_process(int);
static bool ct_border();
static WINDOW *game, *msg, *side;
static int curw, curh;

void ui_init() {
  /* Set some basic properties */
  initscr();
  if( has_colors() ){
    start_color();
  }else{
    DEBUG("Crappy terminal has no colors. Not using any.");
  }
  /* No line buffering, take function keys  */
  cbreak();
  keypad(stdscr, true);
  noecho();
  /* Make some windows */
  game = newwin(1,1,1,1);
  msg = newwin(1,1,1,1);
  side = newwin(1,1,1,1);
  /* Default terminal size */
  /* @TODO Find portable way of detecting this */
  ct_layout(25,80,0,0);
  wrefresh(game);
  wrefresh(msg);
  wrefresh(side);
}

void ui_destroy(){
  if(
     delwin( game ) == ERR ||
     delwin( msg ) == ERR ||
     delwin( side ) == ERR
   ) DEBUG("Failed to destroy ncurses window(s)");
  endwin();
}

void ui_interact(int msec){
  int total, c, errlvl;
  /* Set delay to n milliseconds */
  if( msec > 0 ){
    total = msec;
    wtimeout(game,50);
  }
  errlvl = 0;
  while(1){
    c = wgetch(game);
    total -= 50;
    if( c == ERR ){
      if( msec > 0 ){
	/* Timed out */
	if( total <= 0 ) break;
      }else{
	/* Probably a real error */
	errlvl++;
	if( errlvl >= 5 ) break;
	break;
      }
    }else{
      ct_process(c);
    }
  }
  if( msec > 0 ) wtimeout(game,-1);
}

void ui_place_obj(int c, int x, int y){
  char b[2];
  b[0] = c; b[1] = 0;
  mvwprintw(game,y,x,b);
  wrefresh(game);
}

void ui_printf(int color, const char *fmt, ...){
  va_list vargs;
  bool clr;
  clr = has_colors();
  if( clr ) attron(COLOR_PAIR(color));
  va_start(vargs,fmt);
  vwprintw(msg,fmt,vargs);
  va_end(vargs);
  wrefresh(msg);
  if( clr ) attroff(COLOR_PAIR(color));
}

char *ui_prompt  (int doecho, const char *msg){
  char *store;
  /* Set up modes */
  if( doecho ) echo();
  else noecho();
  nocbreak();
  store = (char*)malloc(sizeof(char)*1024);
  /* First, we need to send the cursor to bottom screen */
  mvwprintw(game, (int)(GAMEWIN_H*curh)-1, 0, msg);
  wrefresh(game);
  wgetnstr(game,store,1024);
  /* Reset mode */
  noecho(); cbreak();
  return store;
}

bool ct_layout(int h, int w, int y, int x){
  /* Calculate actual dimensions based on constant ratios */
  curw = w; curh = h;
  return
    mvwin( game, y, x ) != ERR &&
    wresize( game, (int)(GAMEWIN_H*h), (int)(GAMEWIN_W*w) ) != ERR &&
    mvwin( side, 0, (int)(GAMEWIN_W*w) ) != ERR &&
    wresize( side, (int)(SIDEWIN_H*h), (int)(SIDEWIN_W*w) ) != ERR &&
    mvwin( msg, (int)(GAMEWIN_H*h), 0 ) != ERR &&
    wresize( msg, (int)(MSGWIN_H*h), (int)(MSGWIN_W*w) ) != ERR &&
    ct_border()
    ? true : false;
}

bool ct_border(){
  return
    box( game, 0, 0 ) != ERR &&
    box( side, 0, 0 ) != ERR &&
    box( msg, 0, 0 ) != ERR
    ? true : false;
}

bool ct_stylize(WINDOW* w){
  if( w == 0 ) return false;
  
  return true;
}

bool ct_process(int k){
  /* Handle key press */
  /* @TODO Send event to server etc. */
  return true;
}


int ui_test(){
  ui_init();
  while(1){
    ui_interact(500);
    ui_printf(0,ui_prompt(1,"ENTER:"));
  }
  ui_destroy();
}
