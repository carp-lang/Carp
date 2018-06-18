#include <stdio.h>
#include <stdlib.h>

// (defn main []
//   (let [x 123
//         f (fn [y] (+ x y))]
//      (f 1)))

// Environment for this particular lambda
typedef struct Env_main_0 {
    int x;
} Env_main_0;

// Deleter
void delete_Env_main_0(Env_main_0 *env) {
    // no managed members to free in this case
    free(env);
    printf("Deleted Env_main_0\n");
}

typedef struct Closure {
    void *callback;
    void *env;
    void (*delete)(void*);
} Closure;

typedef int (*Fn_CallClosure__Int)(void*, int);

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
    Fn_CallClosure__Int casted_f = f.callback;
    int _1 = casted_f(f.env, 1);
    // delete f
    f.delete(f.env);
    return _1;
}
