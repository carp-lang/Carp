#include <SDL.h>

#include <carp_memory.h>
#include <core.h>

typedef struct {
    SDL_Window *window;
    SDL_Renderer *renderer;
} App;

App app_MINUS_init(const char *title, int width, int height) {
    SDL_Init(SDL_INIT_EVERYTHING);
    SDL_Window *window;
    SDL_Renderer *renderer;
    SDL_CreateWindowAndRenderer(width, height, 0, &window, &renderer);
    SDL_SetWindowTitle(window, title);
    App app;
    app.window = window;
    app.renderer = renderer;
    return app;
}

SDL_Window *app_MINUS_window(App app) {
    return app.window;
}

SDL_Renderer *app_MINUS_renderer(App app) {
    return app.renderer;
}

void app_MINUS_stop(App *app) {
    SDL_DestroyWindow(app->window);
    SDL_Quit();
}

SDL_Event SDL_event_MINUS_init() {
    SDL_Event e;
    return e;
}

int SDL_event_MINUS_type(SDL_Event *e) {
    return e->type;
}

SDL_Keycode SDL_event_MINUS_keycode(SDL_Event *e) {
    return e->key.keysym.sym;
}

int SDL_Keycode__EQ_(SDL_Keycode a, SDL_Keycode b) {
    return a == b;
}

SDL_Keycode SDL_Keycode_copy(SDL_Keycode* a) {
    return *a;
}

String Keycode_str(SDL_Keycode a) {
    char *buffer = CARP_MALLOC(32);
    snprintf(buffer, 32, "%d", a);
    return buffer;
}

SDL_Rect SDL_make_MINUS_rect(int x, int y, int w, int h) {
    SDL_Rect r;
    r.x = x;
    r.y = y;
    r.w = w;
    r.h = h;
    return r;
}

SDL_Point SDL_make_MINUS_point(int x, int y) {
    SDL_Point p;
    p.x = x;
    p.y = y;
    return p;
}

bool SDL_EventType__EQ_(SDL_EventType a, SDL_EventType b) {
    return a == b;
}
