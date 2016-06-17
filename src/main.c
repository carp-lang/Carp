#include "../shared/platform.h"
#include "../shared/shared.h"
#include "repl.h"
#include "eval.h"
#include "gc.h"
#include <stdio.h>
#include "bytecode.h"

#define HANDLE_SIGNALS 0

void signal_handler(int sig) {
  printf("\e[31m");
  printf("Got signal: ");
  switch(sig) {
  case SIGABRT:
    printf("SIGABRT\n");
    break;
  case SIGFPE:
    printf("SIGFPE\n");
    break;
  case SIGILL:
    printf("SIGILL\n");
    break;
  case SIGINT:
    printf("SIGINT\n");
    break;
  case SIGSEGV:
    printf("SIGSEGV\n");
    printf("\e[0m");
    printf("Will try to resume in 1 second...\n");
    sleep(1);
    longjmp(jumpbuffer, 0);
    break;
  case SIGTERM:
    printf("SIGTERM\n");
    break;
  default:
    printf("Unhandled %d\n", sig);
  }
  exit(-1);
}

int main(int argc, char **argv) {

  if(HANDLE_SIGNALS) {
    signal(SIGABRT, signal_handler);
    signal(SIGFPE, signal_handler);
    signal(SIGILL, signal_handler);
    signal(SIGSEGV, signal_handler);
    signal(SIGTERM, signal_handler);
    //signal(SIGINT, signal_handler);
  }
  
  /* printf("%ld %ld %ld \n", sizeof(float), sizeof(int), sizeof(void*)); */
  carp_platform_init();
  obj_total_max = 100000;
  parallell = NULL;

  Process *process = process_new();

  if(BYTECODE_EVAL) {
    eval_text(process, process->global_env, "(def BYTECODE_EVAL true)", false, obj_new_string("main.c"));
  }
  else {
    eval_text(process, process->global_env, "(def BYTECODE_EVAL false)", false, obj_new_string("main.c"));
  }
  
  eval_text(process,
            process->global_env,
            "(load-lisp (str (getenv \"CARP_DIR\") \"lisp/boot.carp\"))",
            false,
            obj_new_string("main.c"));
  
  if(argc == 2) {
    char load_file[512];
    snprintf(load_file, 512, "(load-lisp (str \"%s\"))", argv[1]);
    eval_text(process, process->global_env, load_file, false, obj_new_string("main.c"));
  }

  repl(process);

  carp_platform_shutdown();
  gc_all();
  assert(obj_total == 0);
}
