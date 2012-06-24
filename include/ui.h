#ifndef COTITAN_UI_H_
#define COTITAN_UI_H_

/* Initializes the user interface. */
void ui_init     (void);

/* Places the character c at the coordinate (x,y) on the world map. */
void ui_place_obj(int c, int x, int y);

/* Prints a formatted message to the event log. */
void ui_printf   (int color, const char *msg, ...);

/* Prompts the user to enter a line. */
char *ui_prompt  (int echo, const char *msg);

#endif

