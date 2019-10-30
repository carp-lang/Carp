#include <SDL.h>

// Event
SDL_Event SDL_Event_init() {
    SDL_Event e;
    memset(&e, 0, sizeof(e));
    return e;
}

int SDL_Event_type(SDL_Event *e) {
    return e->type;
}

SDL_Keycode SDL_Event_keycode(SDL_Event *e) {
    return e->key.keysym.sym;
}

bool SDL_Event__EQ_(SDL_EventType a, SDL_EventType b) {
    return a == b;
}

SDL_Event SDL_Event_copy(SDL_Event *e) {
    return *e;
}

// Keycode

SDL_Keycode SDL_Keycode_copy(SDL_Keycode *a) {
    return *a;
}

String SDL_Keycode_str(SDL_Keycode a) {
    char *buffer = CARP_MALLOC(32);
    snprintf(buffer, 32, "%d", a);
    return buffer;
}

// Helpers
SDL_Rect SDL_rect(int x, int y, int w, int h) {
    SDL_Rect r;
    r.x = x;
    r.y = y;
    r.w = w;
    r.h = h;
    return r;
}

SDL_Point SDL_point(int x, int y) {
    SDL_Point p;
    p.x = x;
    p.y = y;
    return p;
}

SDL_Color SDL_rgb(int r, int g, int b) {
    SDL_Color p;
    p.r = r;
    p.g = g;
    p.b = b;
    p.a = 255;
    return p;
}

SDL_Color SDL_rgba(int r, int g, int b, int a) {
    SDL_Color p;
    p.r = r;
    p.g = g;
    p.b = b;
    p.a = a;
    return p;
}

int SDL_Color_r(SDL_Color *col) {
    return (int)(col->r);
}

int SDL_Color_g(SDL_Color *col) {
    return (int)(col->g);
}

int SDL_Color_b(SDL_Color *col) {
    return (int)(col->b);
}

int SDL_Color_a(SDL_Color *col) {
    return (int)(col->a);
}

void *SDL_SurfacePixels(SDL_Surface *s) {
    return s->pixels;
}

int SDL_SurfacePitch(SDL_Surface *s) {
    return s->pitch;
}
