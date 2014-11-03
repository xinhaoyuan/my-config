#include <X11/Xlib.h>
#include <X11/extensions/shape.h>
#include <stdlib.h>
#include <string.h>

#include <sstream>
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

using namespace std;

Display *dpy;
Screen  *screen;
int      screen_id;
int      screen_width;
int      screen_height;

typedef struct osd_box_s {
    string *id;
    int x_covered;
    int y_covered;
    int x, y;
    int width, height;
    int lsx, lsy, lex, ley;  // logical axis
    int frame_size;
    Window window;
    int mapped;
} osd_box_s;

typedef struct interval_point_s {
    osd_box_s *box;
    int dir;
    int entry;
} interval_point_s;

int show_uncovered = 0;
int major_direction = 0;

void
create_osd_box(osd_box_s *box,
               int x, int y, int width, int height,
               int frame_size) {
    
    XSetWindowAttributes attrs;
    XRectangle xrect;
    GC gc;
    attrs.override_redirect = True;
    attrs.background_pixel = 0x000000;
    Window win = XCreateWindow(dpy, RootWindow(dpy, screen_id),
                               x, y, width, height, 0,
                               CopyFromParent, InputOutput,
                               CopyFromParent,
                               CWOverrideRedirect | CWBackPixel,
                               &attrs);

    /* create rectangle what will be 'transparent' in the window */
    xrect.x = frame_size;
    xrect.y = frame_size;
    xrect.width = width - frame_size * 2;
    xrect.height = height - frame_size * 2;

    /* substruct rectangle from the window */
    XShapeCombineRectangles(dpy, win, ShapeBounding,
                            0, 0, &xrect, 1, ShapeSubtract, Unsorted);

    box->x          = x;
    box->y          = y;
    box->width      = width;
    box->height     = height;
    box->frame_size = frame_size;
    box->window     = win;
    box->mapped     = 0;
    box->x_covered  = 0;
    box->y_covered  = 0;
}

void
show_osd_box(osd_box_s *box, int bg, int fg) {
    XSetWindowBackground(dpy, box->window, bg);
    if (box->mapped)
        XClearWindow(dpy, box->window);
    else {
        box->mapped = 1;
        XMapWindow(dpy, box->window);
    }

    GC gc;
    /* create a white gc */
    gc = XCreateGC(dpy, box->window, 0, NULL);
    XSetForeground(dpy, gc, fg);

    /* draw the outer white rectangle */
    XDrawRectangle(dpy, box->window, gc,
                   0, 0, box->width - 1, box->height - 1);

    /* draw the inner white rectangle */
    XDrawRectangle(dpy, box->window, gc,
                   box->frame_size - 1, box->frame_size - 1,
                   box->width - 2 * (box->frame_size - 1) - 1,
                   box->height - 2 * (box->frame_size - 1) - 1);

    XFreeGC(dpy, gc);
}

void
hide_osd_box(osd_box_s *box) {
    if (box->mapped) {
        box->mapped = 0;
        XUnmapWindow(dpy, box->window);
    }
}

Window wpointer;
Window wline;
vector<osd_box_s *> boxes;
vector<interval_point_s *> x_points;
vector<interval_point_s *> y_points;
int cur_x, cur_y, cur_lx, cur_ly;
osd_box_s *cur_box = NULL;

int
getv(interval_point_s *a) {
    if (a->dir == 0)
        return (a->entry ? a->box->x : (a->box->x + a->box->width));
    else return (a->entry ? a->box->y : (a->box->y + a->box->height));
}

bool
point_cmp(interval_point_s *a, interval_point_s *b) {
    int va = getv(a);
    int vb = getv(b);
    if (va == vb) return a->entry < b->entry;
    else return va < vb;
}

void
redraw_osd_boxes() {
    osd_box_s *box_cd = NULL;
    int box_cd_dis;
    int dis;
    for (int i = 0; i < boxes.size(); ++ i) {
        osd_box_s *box = boxes[i];
        box->x_covered = (box->lsx <= cur_lx &&
                          cur_lx < box->lex);
        box->y_covered = (box->lsy <= cur_ly &&
                          cur_ly < box->ley);
        
        if (major_direction == 0 &&
            box->y_covered) {
            show_osd_box(box, 0x000000, 0xffffff);
            dis = box->lsx - cur_lx + box->lex - cur_lx;
            if (dis < 0) dis = -dis;
            if (box_cd == NULL || dis < box_cd_dis) {
                box_cd_dis = dis;
                box_cd = box;
            }
        } else if (major_direction == 1 &&
                   box->x_covered) {
            show_osd_box(box, 0x000000, 0xffffff);
            if (dis < 0) dis = -dis;
            if (box_cd == NULL || dis < box_cd_dis)
                box_cd = box;
        } else if (show_uncovered)
            show_osd_box(box, 0x7f7f7f, 0xffffff);
        else hide_osd_box(box);
    }

    if (cur_box == NULL)
        cur_box = box_cd;

    if (cur_box) {
        show_osd_box(cur_box, 0xffffff, 0x7f7f7f);
        XRaiseWindow(dpy, cur_box->window);
    }

    cur_x = getv(x_points[cur_lx]);
    cur_y = getv(y_points[cur_ly]);

    if (major_direction == 0)
        XMoveWindow(dpy, wline, 0, cur_y);
    else XMoveWindow(dpy, wline, cur_x, 0);
    XRaiseWindow(dpy, wline);
    
    XMoveWindow(dpy, wpointer, cur_x - 2, cur_y - 2);
    XRaiseWindow(dpy, wpointer);
}

int
move_up(int same_column, int skip_row) {
    int next_ly = cur_ly;
    while (true) {
        osd_box_s *box = y_points[next_ly]->box;
        if (y_points[next_ly]->entry &&
            (!same_column ||  box->x_covered) &&
            (!skip_row    || !box->y_covered) &&
            box != cur_box) {
            cur_ly = next_ly;
            cur_box = box;
            break;
        }
        
        if (next_ly > 0)
            -- next_ly;
        else break;
    }
}

int
move_down(int same_column, int skip_row) {
    int next_ly = cur_ly;
    while (true) {
        if (next_ly + 1 >= y_points.size()) break;
        osd_box_s *box = y_points[next_ly + 1]->box;
        if (!y_points[next_ly + 1]->entry &&
            (!same_column ||  box->x_covered) &&
            (!skip_row    || !box->y_covered) &&
            box != cur_box) {
            cur_ly = next_ly;
            cur_box = box;
            break;
        }
        ++ next_ly;
    }
}

int
move_left(int same_row, int skip_column) {
    int next_lx = cur_lx;
    while (true) {
        osd_box_s *box = x_points[next_lx]->box;
        if (x_points[next_lx]->entry &&
            (!same_row    ||  box->y_covered) &&
            (!skip_column || !box->x_covered) &&
            box != cur_box) {
            cur_lx = next_lx;
            cur_box = box;
            break;
        }
        
        if (next_lx > 0)
            -- next_lx;
        else break;
    }
}

int
move_right(int same_row, int skip_column) {
    int next_lx = cur_lx;
    while (true) {
        if (next_lx + 1 >= x_points.size()) break;
        osd_box_s *box = x_points[next_lx + 1]->box;
        if (!x_points[next_lx + 1]->entry &&
            (!same_row    ||  box->y_covered) &&
            (!skip_column || !box->x_covered) &&
            box != cur_box) {
            cur_lx = next_lx;
            cur_box = box;
            break;
        }
        ++ next_lx;
    }
}

void
help() {
    cout << "-s\t:\tshow box that is not covered\n"
         << "-h\t:\tset direction to horizontal\n"
         << "-v\t:\tset direction to vertical\n"
         << "--help\t:\tprint this help\n";
}

int
main(int argc, char **argv) {
    int i;

    for (i = 0; i < argc; ++ i) {
        if (strcmp(argv[i], "-s") == 0) {
            show_uncovered = 1;
        } else if (strcmp(argv[i], "-h") == 0) {
            major_direction = 0;
        } else if (strcmp(argv[i], "-v") == 0) {
            major_direction = 1;
        } else if (strcmp(argv[i], "--help") == 0) {
            help();
            return 0;
        }
    }
    
    dpy                    = XOpenDisplay(NULL);
    screen_id              = DefaultScreen(dpy);
    screen                 = ScreenOfDisplay(dpy, screen_id);
    screen_width           = WidthOfScreen(screen);
    screen_height          = HeightOfScreen(screen);
    
    int ret                = 0;    
    int keycode_up         = XKeysymToKeycode(dpy, XK_Up);
    int keycode_down       = XKeysymToKeycode(dpy, XK_Down);
    int keycode_left       = XKeysymToKeycode(dpy, XK_Left);
    int keycode_right      = XKeysymToKeycode(dpy, XK_Right);
    int keycode_exit       = XKeysymToKeycode(dpy, XK_Escape);
    int keycode_enter      = XKeysymToKeycode(dpy, XK_Return);
    unsigned int modifiers = AnyModifier;
    Window root            = DefaultRootWindow(dpy);
    
    bool b;
    string line;
    while (b = getline(cin, line)) {
        istringstream is(line);
        int x, y;
        if (is >> x >> y) {
            cur_x = x;
            cur_y = y;
            break;
        }
    }

    if (!b) {
        cerr << "cannot read start axis" << endl;
        ret = -1;
        goto end;
    }

    while (getline(cin, line)) {
        istringstream is(line);
        string id;
        int x, y, w, h;
        if (is >> id >> x >> y >> w >> h) {
            osd_box_s *box = new osd_box_s();
            interval_point_s *x_entry = new interval_point_s();
            interval_point_s *x_exit = new interval_point_s();
            interval_point_s *y_entry = new interval_point_s();
            interval_point_s *y_exit = new interval_point_s();
            
            x_entry->box = box;
            x_entry->dir = 0;
            x_entry->entry = 1;
            x_exit->box = box;
            x_exit->dir = 0;
            x_exit->entry = 0;
            y_entry->box = box;
            y_entry->dir = 1;
            y_entry->entry = 1;
            y_exit->box = box;
            y_exit->dir = 1;
            y_exit->entry = 0;

            create_osd_box(box, x, y, w, h, 4);
            box->id = new string(id);

            boxes.push_back(box);
            x_points.push_back(x_entry); 
            x_points.push_back(x_exit);
            y_points.push_back(y_entry);
            y_points.push_back(y_exit);
        }
    }

    if (boxes.size() == 0) {
        cerr << "no box to select" << endl;
        ret = -1;
        goto end;
    }

    sort(x_points.begin(), x_points.end(), point_cmp);
    sort(y_points.begin(), y_points.end(), point_cmp);

    for (int i = 0; i < x_points.size(); ++ i) {
        if (x_points[i]->entry)
            x_points[i]->box->lsx = i;
        else x_points[i]->box->lex = i;
    }

    for (int i = 0; i < y_points.size(); ++ i) {
        if (y_points[i]->entry)
            y_points[i]->box->lsy = i;
        else y_points[i]->box->ley = i;
    }

    XGrabKey(dpy, keycode_up, 0, root, false, GrabModeAsync, GrabModeAsync);
    XGrabKey(dpy, keycode_down, 0, root, false, GrabModeAsync, GrabModeAsync);
    XGrabKey(dpy, keycode_left, 0, root, false, GrabModeAsync, GrabModeAsync);
    XGrabKey(dpy, keycode_right, 0, root, false, GrabModeAsync, GrabModeAsync);
    XGrabKey(dpy, keycode_up, ShiftMask, root, false, GrabModeAsync, GrabModeAsync);
    XGrabKey(dpy, keycode_down, ShiftMask, root, false, GrabModeAsync, GrabModeAsync);
    XGrabKey(dpy, keycode_left, ShiftMask, root, false, GrabModeAsync, GrabModeAsync);
    XGrabKey(dpy, keycode_right, ShiftMask, root, false, GrabModeAsync, GrabModeAsync);
    XGrabKey(dpy, keycode_up, ControlMask, root, false, GrabModeAsync, GrabModeAsync);
    XGrabKey(dpy, keycode_down, ControlMask, root, false, GrabModeAsync, GrabModeAsync);
    XGrabKey(dpy, keycode_left, ControlMask, root, false, GrabModeAsync, GrabModeAsync);
    XGrabKey(dpy, keycode_right, ControlMask, root, false, GrabModeAsync, GrabModeAsync);
    XGrabKey(dpy, keycode_up, ControlMask | ShiftMask, root, false, GrabModeAsync, GrabModeAsync);
    XGrabKey(dpy, keycode_down, ControlMask | ShiftMask, root, false, GrabModeAsync, GrabModeAsync);
    XGrabKey(dpy, keycode_left, ControlMask | ShiftMask, root, false, GrabModeAsync, GrabModeAsync);
    XGrabKey(dpy, keycode_right, ControlMask | ShiftMask, root, false, GrabModeAsync, GrabModeAsync);
    XGrabKey(dpy, keycode_exit, 0, root, false, GrabModeAsync, GrabModeAsync);
    XGrabKey(dpy, keycode_enter, 0, root, false, GrabModeAsync, GrabModeAsync);
    XSelectInput(dpy, root, KeyPressMask);

    for (i = 0; i < x_points.size(); ++ i) {
        int x = getv(x_points[i]);
        if (x > cur_x) {
            cur_lx = i == 0 ? 0 : i - 1;
            break;
        }
    } if (i == x_points.size()) { cur_lx = x_points.size() - 1; }

    for (i = 0; i < y_points.size(); ++ i) {
        int y = getv(y_points[i]);
        if (y > cur_y) {
            cur_ly = i == 0 ? 0 : i - 1;
            break;
        }
    } if (i == y_points.size()) { cur_ly = y_points.size() - 1; }


    XSetWindowAttributes attrs;
    attrs.override_redirect = True;
    attrs.background_pixel = 0x000000;
    attrs.border_pixel = 0xee6600;
    wpointer = XCreateWindow(dpy, RootWindow(dpy, screen_id),
                            0, 0, 3, 3, 2,
                            CopyFromParent, InputOutput,
                            CopyFromParent,
                            CWOverrideRedirect | CWBackPixel | CWBorderPixel,
                            &attrs);

    if (major_direction == 0) {
        wline = XCreateWindow(dpy, RootWindow(dpy, screen_id),
                              0, 0, screen_width - 2, 1, 1,
                              CopyFromParent, InputOutput,
                              CopyFromParent,
                              CWOverrideRedirect | CWBackPixel | CWBorderPixel,
                              &attrs);
    } else {
        wline = XCreateWindow(dpy, RootWindow(dpy, screen_id),
                              0, 0, 1, screen_height - 2, 1,
                              CopyFromParent, InputOutput,
                              CopyFromParent,
                              CWOverrideRedirect | CWBackPixel | CWBorderPixel,
                              &attrs);
    }
    
    redraw_osd_boxes();

    XMapWindow(dpy, wpointer);
    XMapWindow(dpy, wline);
    
    XEvent e; 
    while (1) {
        XNextEvent(dpy, &e);
        switch(e.type)
        {
        case KeyPress: {
            int ctrl  = (e.xkey.state & ControlMask) != 0;
            int shift = (e.xkey.state & ShiftMask)   != 0;
            int v     = major_direction != 0;
            if (e.xkey.keycode == keycode_up) {
                move_up(v ^ ctrl ^ shift, v ^ !ctrl);
            } else if (e.xkey.keycode == keycode_down) {
                move_down(v ^ ctrl ^ shift, v ^ !ctrl);
            } else if (e.xkey.keycode == keycode_left) {
                move_left(v ^ ctrl ^ !shift, v ^ ctrl);
            } else if (e.xkey.keycode == keycode_right) {
                move_right(v ^ ctrl ^ !shift, v ^ ctrl);
            } else if (e.xkey.keycode == keycode_exit ||
                       e.xkey.keycode == keycode_enter) {
                if (cur_box != NULL) {
                    cout << *cur_box->id << ' ' << cur_x << ' ' << cur_y << endl;
                }
                goto cleanup_and_end;
            }
            redraw_osd_boxes();
        } 
        default:
            break;
        }
    }

  cleanup_and_end:
  end:
    XCloseDisplay(dpy);
    return ret;
}
