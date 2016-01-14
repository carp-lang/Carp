#include "repl.h"
#include "eval.h"
#include "../out/shared.h"

int main() {
  shadow_stack_pos = 0;
  env_new_global();
  //eval_text(global_env, "(load-lisp (str (getenv \"CARP_DIR\") \"lisp/boot.carp\"))", false);
  eval_text(global_env, "(def id (fn (x) x))", false);
  eval_text(global_env, "(def f (fn (x) (+ x 10)))", false);  
  repl(global_env);  
  assert(obj_total == 0);
}
