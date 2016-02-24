#include "repl.h"
#include "eval.h"
#include "../shared/shared.h"

int main() {
  //printf("%ld %ld %ld", sizeof(float), sizeof(int), sizeof(void*));
  carp_platform_init();
  obj_total_max = 100000;
  stack_pos = 0;
  shadow_stack_pos = 0;
  env_new_global();
  eval_text(global_env, "(load-lisp (str (getenv \"CARP_DIR\") \"lisp/boot.carp\"))", false, obj_new_string("main.c"));
  pop_stacks_to_zero();
  repl(global_env);
  carp_platform_shutdown();
  assert(obj_total == 0);
}
