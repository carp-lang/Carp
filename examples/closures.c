#include <stdio.h>
#include <stdlib.h>

// (defn main []
//   (let [x 123
//         f (fn [] (+ 1 x))]
//      (f)))

// Environment for this particular lambda
typedef struct Env_main_0 {
    int x;
} Env_main_0;

// Deleter
void delete_Env_main_0(Env_main_0 env) {
    // nothing to free in this case
}

typedef struct Closure {
    int (*callback)(void *closure);
    void *env;
    void (*delete)(void*);
} Closure;

// The body of the lambda, but lifted to its own function. Takes the environment as a first argument.
int lifted_lambda_main_0(Env_main_0 *env) {
    return 1 + env->x; // simplified for readability
}

int main() {
    // let x
    int x = 123;
    // let f
    Env_main_0 *env_0 = malloc(sizeof(Env_main_0));
    env_0->x = x;
    Closure f = {
        .callback = (void*)lifted_lambda_main_0,
        .env = env_0,
        .delete = (void*)delete_Env_main_0
    };
    // call f
    int _1 = f.callback(f.env);
    // delete f
    f.delete(f.env);
    return _1;
}
