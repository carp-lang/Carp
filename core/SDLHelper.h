#include <core.h>
#include <SDL.h>

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

SDL_Event SDL_Event_init() {
    SDL_Event e;
    return e;
}

int event_MINUS_type(SDL_Event *e) {
    return e->type;
}

SDL_Keycode event_MINUS_keycode(SDL_Event *e) {
    return e->key.keysym.sym;
}

int Keycode__EQ_(SDL_Keycode a, SDL_Keycode b) {
    return a == b;
}

SDL_Keycode Keycode_copy(SDL_Keycode* a) {
    return *a;
}

string Keycode_str(SDL_Keycode a) {
    char *buffer = CARP_MALLOC(32);
    snprintf(buffer, 32, "%d", a);
    return buffer;
}

SDL_Rect make_MINUS_rect(int x, int y, int w, int h) {
    SDL_Rect r;
    r.x = x;
    r.y = y;
    r.w = w;
    r.h = h;
    return r;
}

SDL_Point make_MINUS_point(int x, int y) {
    SDL_Point p;
    p.x = x;
    p.y = y;
    return p;
}
