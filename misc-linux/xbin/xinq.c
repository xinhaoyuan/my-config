/* This code is dedicated to public domain. */
#include <stdio.h>

#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include <X11/extensions/Xinerama.h>

int wa_sx, wa_sy, wa_ex, wa_ey;
int win_sx, win_sy, win_ex, win_ey;
int max_its = 0;
int _sx, _sy, _ex, _ey;

#define MIN(a,b) ((a) < (b) ? (a) : (b))
#define MAX(a,b) ((a) > (b) ? (a) : (b))

void process_screen(int sx, int sy, int ex, int ey) {
    if (sx < wa_sx) sx = wa_sx;
    if (ex > wa_ex) ex = wa_ex;
    if (sy < wa_sy) sy = wa_sy;
    if (ey > wa_ey) ey = wa_ey;

    int its = (MIN(ex, win_ex) - MAX(sx, win_sx)) * (MIN(ey, win_ey) - MAX(sy, win_sy));
    if (its > max_its) {
        max_its = its;
        _sx = sx; _ex = ex;
        _sy = sy; _ey = ey;
    }
}

int main(int argc, char **argv)
{
    if (argc != 9) {
        fprintf(stderr, "Usage %s [WA X] [WA Y] [WA W] [WA H] [WIN X] [WIN Y] [WIN W] [WIN H]\n",
                argv[0]);
        return 1;
    }

    wa_sx = atoi(argv[1]);
    wa_sy = atoi(argv[2]);
    wa_ex = wa_sx + atoi(argv[3]);
    wa_ey = wa_sy + atoi(argv[4]);
    win_sx = atoi(argv[5]);
    win_sy = atoi(argv[6]);
    win_ex = win_sx + atoi(argv[7]);
    win_ey = win_sy + atoi(argv[8]);

    Display *dpy = XOpenDisplay(NULL);
    if (dpy == NULL) {
        fprintf(stderr, "error: cannot open the display\n");
        return 1;
    }

    int n;
    XineramaScreenInfo *info;

    int screen = DefaultScreen(dpy);

    if (info = XineramaQueryScreens(dpy, &n)) {
        int i;
        for (i = 0; i < n; ++ i) {
            process_screen(info[i].x_org, info[i].y_org,
                           info[i].x_org + info[i].width, info[i].y_org + info[i].height);
        }
    } else {
        process_screen(0, 0, DisplayWidth(dpy, screen), DisplayHeight(dpy, screen));
    }
    
    XFree(info);
    XCloseDisplay(dpy);

    printf("%d %d %d %d", _sx, _sy, _ex - _sx, _ey - _sy);
    
    return 0;
}
