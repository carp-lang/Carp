#include "bytecode.h"
#include "obj.h"
#include "obj_array.h"
#include "process.h"
#include "env.h"
#include "obj_string.h"
#include "assertions.h"

#define HEAD_EQ(str) (form->car->tag == 'Y' && strcmp(form->car->s, (str)) == 0)

// 'q' stop
// 'l' push literal
// 'c' call
// 'y' lookup

void visit_form(Obj *env, Obj *bytecodeObj, int *position, Obj *form);

void add_literal(Obj *bytecodeObj, int *position, Obj *form) {
  Obj *literals = bytecodeObj->bytecode_literals;
  char new_literal_index = literals->count;
  obj_array_mut_append(literals, form);
  bytecodeObj->bytecode[*position] = 'l';
  bytecodeObj->bytecode[*position + 1] = new_literal_index + 65;
  *position += 2;
}

void add_call(Obj *env, Obj *bytecodeObj, int *position, Obj *form) {
  Obj *argp = form->cdr;
  int arg_count = 0;
  while(argp && argp->car) {
    visit_form(env, bytecodeObj, position, argp->car);
    argp = argp->cdr;
    arg_count++;
  }
  visit_form(env, bytecodeObj, position, form->car); // the function position
  bytecodeObj->bytecode[*position] = 'c';
  bytecodeObj->bytecode[*position + 1] = arg_count + 65;
  (*position) += 2;
}

void add_lookup(Obj *bytecodeObj, int *position, Obj *form) {
  Obj *literals = bytecodeObj->bytecode_literals;
  char new_literal_index = literals->count;
  obj_array_mut_append(literals, form);
  bytecodeObj->bytecode[*position] = 'y';
  bytecodeObj->bytecode[*position + 1] = new_literal_index + 65;
  *position += 2;
}  

void visit_form(Obj *env, Obj *bytecodeObj, int *position, Obj *form) {
  if(form->tag == 'C') {
    if(form->car->car == NULL) {
      add_literal(bytecodeObj, position, nil);
    }
    else if(HEAD_EQ("quote")) {
      add_literal(bytecodeObj, position, form->car);
    }
    else if(HEAD_EQ("fn")) {
      Obj *lambda = obj_new_lambda(form->cdr->car, form_to_bytecode(env, form->cdr->cdr->car), env, form);
      add_literal(bytecodeObj, position, lambda);
    }
    else {
      add_call(env, bytecodeObj, position, form);
    }
  }
  else if(form->tag == 'Y') {
    add_lookup(bytecodeObj, position, form);
  }
  else {
    add_literal(bytecodeObj, position, form);
  }
  /* else { */
  /*   printf("Bytecode can't handle form: "); */
  /*   obj_print_cout(form); */
  /*   exit(1); */
  /* } */
}

Obj *form_to_bytecode(Obj *env, Obj *form) {
  char *code = malloc(2048);
  Obj *bytecodeObj = obj_new_bytecode(code);
  int position = 0;
  visit_form(env, bytecodeObj, &position, form);
  bytecodeObj->bytecode[position++] = 'q';
  bytecodeObj->bytecode[position++] = '\0';  
  return bytecodeObj;
}

/* void bytecode_apply(Process *process, Obj *function, Obj **args, int arg_count) { */
  
/* } */

typedef struct {
  int p;
  Obj *bytecodeObj;
  Obj *env;
} BytecodeFrame;

Obj *bytecode_eval(Process *process, Obj *bytecodeObj) {
  BytecodeFrame frames[256] = {};
  int frame = 0;
  frames[frame].p = 0;
  frames[frame].bytecodeObj = bytecodeObj;
  frames[frame].env = process->global_env;
 
  Obj *literal, *function, *lookup;
  int arg_count, i;
  
  while(true) {
    //printf("frame = %d\n", frame);
    Obj **literals_array = frames[frame].bytecodeObj->bytecode_literals->array;
    char *bytecode = frames[frame].bytecodeObj->bytecode;
    int p = frames[frame].p;
    char c = bytecode[p];
    
    switch(c) {
    case 'l':
      i = bytecode[p + 1] - 65;
      literal = literals_array[i];
      //printf("Pushing literal "); obj_print_cout(literal); printf("\n");
      stack_push(process, literal);
      frames[frame].p += 2;
      break;
    case 'y':
      i = bytecode[p + 1] - 65;
      literal = literals_array[i];
      //printf("Looking up literal "); obj_print_cout(literal); printf("\n");
      lookup = env_lookup(process, frames[frame].env, literal);
      if(!lookup) {
        set_error_return_nil("Failed to lookup ", literal);
      }
      stack_push(process, lookup);
      frames[frame].p += 2;
      break;
    case 'c':
      function = stack_pop(process);
      arg_count = bytecode[p + 1] - 65;
      Obj **args = NULL;
      if(arg_count > 0) {
        args = malloc(sizeof(Obj*) * arg_count);
      }
      for(int i = 0; i < arg_count; i++) {
        Obj *arg = stack_pop(process);
        args[arg_count - i - 1] = arg;
        //shadow_stack_push(process, arg);
      }
      frames[frame].p += 2;

      if(function->tag == 'P') {
        stack_push(process, function->primop((struct Process*)process, args, arg_count));
      }
      else if(function->tag == 'L') {
        Obj *calling_env = obj_new_environment(function->env);
        //printf("arg_count = %d\n", arg_count);
        env_extend_with_args(process, calling_env, function, arg_count, args, true);
        frame++;
        frames[frame].p = 0;
        frames[frame].bytecodeObj = function->body;
        frames[frame].env = calling_env;
        //printf("Pushing new stack frame with bytecode '%s'\n", frames[frame].bytecode); // and env %s\n", frames[frame].bytecode, obj_to_string(process, calling_env)->s);
      }
      else {
        printf("Can't handle other calling methods yet %c\n", function->tag);
        obj_print_cout(function);
        return nil;
      }      
      break;
    case 'q':
      frame--;
      if(frame < 0) {
        goto done;
      }
      break;
    default:
      printf("Unhandled instruction: %c\n", c);
      exit(-1);
    }
  }

 done:;
  return stack_pop(process);
}

