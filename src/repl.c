#include "repl.h"
#include "eval.h"
#include "gc.h"
#include "obj_string.h"
#include "reader.h"
#include "eval.h"
#include "env.h"
#include "primops.h"
#include "process.h"

jmp_buf jumpbuffer;

#define MAX_INPUT_BUFFER_SIZE (2048 * 32)
char input[MAX_INPUT_BUFFER_SIZE];

#define GC_COLLECT_BEFORE_REPL_INPUT 0

int paren_balance(char *s) {
  char ignore = '\0';
  //printf("s = %s\n", s);
  int balance = 0;
  for(int i = 0; s[i] != '\0'; i++) {
    char c = s[i];
    if(ignore == '\0') {
      if(c == '(') balance++;
      if(c == ')') balance--;
      if(c == '[') balance++;
      if(c == ']') balance--;
      if(c == '{') balance++;
      if(c == '}') balance--;
      if(c == '"') {
        ignore = '"';
      }
      if(c == ';') {
        //printf("Start ignoring comment.\n");
        ignore = ';';
      }
    }
    else {
      //printf("ignoring '%c' %d, ignore = '%c'\n", c, c, ignore);
      
      if(c == '"' && c == ignore) {
        //printf("back from ignoring string\n");
        ignore = '\0';
      }
      else if(c == '\n' && ignore == ';') {
        //printf("back from ignoring comment\n");
        ignore = '\0';
      }
    }
  }
  return balance;
}

void repl(Process *process) {
  while(1) {

    /* int r = */ setjmp(jumpbuffer);
    //printf("r = %d\n", r);
    
    if(GC_COLLECT_BEFORE_REPL_INPUT) {
      if(LOG_GC_POINTS) {
	printf("Running GC before taking REPL input:\n");
      }
      gc(process);
    }
    if(prompt) {
      printf("%s", prompt->cdr->s);
    }
    int read_offset = 0;
    
  read_more:;
    void *eof = fgets(input + read_offset, MAX_INPUT_BUFFER_SIZE - read_offset, stdin);
    if(eof == NULL) {
      break;
    }
    if(paren_balance(input) <= 0) {
      process_reset(process);
      eval_text(process, process->global_env, input, true, obj_new_string("repl"));
      pop_stacks_to_zero(process);
      printf("\n");
      if(process->dead) {
        break;
      }
    }
    else {
      //printf("Unbalanced, waiting for ending parenthesis.\n");
      if(prompt_unfinished_form) {
        printf("%s", prompt_unfinished_form->cdr->s);
      }
      read_offset = strlen(input);
      goto read_more;
    }
    //assert(stack_pos == 0);
    //stack_print();

    if(parallell) {
      process_tick(parallell);
      printf("Ticked parallell process with result: %s\n", parallell->final_result ? obj_to_string(process, parallell->final_result)->s : "NULL");
      if(parallell->final_result) {
        parallell = NULL;
      }
    }
  }
  gc(process);
}

void pop_stacks_to_zero(Process *process) {
  process->stack_pos = 0;
  process->shadow_stack_pos = 0;
  process->function_trace_pos = 0;
}
