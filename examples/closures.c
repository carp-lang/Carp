#include <stdio.h>
#include <stdlib.h>

// (defn main []
//   (let-do [x 123
//           f (fn [y] (+ x y))
//           g square]
//      (f 210)
//      (g 3))

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

typedef int (*Fn_CallClosure__Int_Int)(void*, int);
typedef int (*Fn_Int_Int)(int);

// The body of the lambda, but lifted to its own function. Takes the environment as a first argument.
int lifted_lambda_main_0(Env_main_0 *env, int y) {
    return env->x + y; // simplified for readability
}

int square(int x) {
    return x * x;
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
    //let g
    Closure g = {
        .callback = square,
        .env = NULL,
        .delete = NULL
    };
    // call f
    int _1 = f.env ? ((Fn_CallClosure__Int_Int)f.callback)(f.env, 210) : ((Fn_Int_Int)f.callback)(210);
    // delete f
    if(f.delete) { f.delete(f.env); }
    // call g
    int _2 = g.env ? ((Fn_CallClosure__Int_Int)g.callback)(f.env, 3) : ((Fn_Int_Int)g.callback)(3);
    // delete g
    if(g.delete) { f.delete(f.env); }
    printf("_1 = %d\n_2 = %d\n", _1, _2);
}
