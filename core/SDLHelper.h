#include <SDL.h>

#include <carp_memory.h>
#include <core.h>

SDL_Event SDL_Event_init() {
    SDL_Event e;
    return e;
}

int SDL_Event_type(SDL_Event *e) {
    return e->type;
}

SDL_Keycode SDL_Event_keycode(SDL_Event *e) {
    return e->key.keysym.sym;
}

int SDL_Keycode__EQ_(SDL_Keycode a, SDL_Keycode b) {
    return a == b;
}

SDL_Keycode SDL_Keycode_copy(SDL_Keycode* a) {
    return *a;
}

String SDL_Keycode_str(SDL_Keycode a) {
    char *buffer = CARP_MALLOC(32);
    snprintf(buffer, 32, "%d", a);
    return buffer;
}

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

bool SDL_Event__EQ_(SDL_EventType a, SDL_EventType b) {
    return a == b;
}
