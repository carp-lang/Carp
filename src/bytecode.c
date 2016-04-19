#include "bytecode.h"
#include "obj.h"
#include "obj_array.h"
#include "process.h"
#include "env.h"
#include "obj_string.h"
#include "assertions.h"
#include "eval.h"

#define HEAD_EQ(str) (form->car->tag == 'Y' && strcmp(form->car->s, (str)) == 0)

// 'q' stop
// 'l' push literal
// 'c' call
// 'y' lookup
// 'i' if branch

// 'd' def
// 'e' local env def
// 'r' reset!
// 't' let
// 's' pop one frame
// 'o' do

void visit_form(Process *process, Obj *env, Obj *bytecodeObj, int *position, Obj *form);

void add_literal(Obj *bytecodeObj, int *position, Obj *form) {
  Obj *literals = bytecodeObj->bytecode_literals;
  char new_literal_index = literals->count;
  obj_array_mut_append(literals, form);
  bytecodeObj->bytecode[*position] = 'l';
  bytecodeObj->bytecode[*position + 1] = new_literal_index + 65;
  *position += 2;
}

void add_call(Process *process, Obj *env, Obj *bytecodeObj, int *position, Obj *form) {
  Obj *argp = form->cdr;
  int arg_count = 0;
  while(argp && argp->car) {
    visit_form(process, env, bytecodeObj, position, argp->car);
    argp = argp->cdr;
    arg_count++;
  }
  visit_form(process, env, bytecodeObj, position, form->car); // the function position
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

void add_if(Process *process, Obj *env, Obj *bytecodeObj, int *position, Obj *form) {
  assert_or_set_error(form->cdr->car, "Too few body forms in 'if' form: ", form);
  assert_or_set_error(form->cdr->cdr->car, "Too few body forms in 'if' form: ", form);
  assert_or_set_error(form->cdr->cdr->cdr->car, "Too few body forms in 'if' form: ", form);
  assert_or_set_error(form->cdr->cdr->cdr->cdr->car == NULL, "Too many body forms in 'if' form (use explicit 'do').", form);
  Obj *true_branch = form_to_bytecode(process, env, form->cdr->cdr->car);
  Obj *false_branch = form_to_bytecode(process, env, form->cdr->cdr->cdr->car);
  Obj *literals = bytecodeObj->bytecode_literals;
  char new_literal_index = literals->count;
  obj_array_mut_append(literals, true_branch);
  obj_array_mut_append(literals, false_branch);
  visit_form(process, env, bytecodeObj, position, form->cdr->car);
  bytecodeObj->bytecode[*position] = 'i';
  bytecodeObj->bytecode[*position + 1] = new_literal_index + 65;
  *position += 2;
}

void add_do(Process *process, Obj *env, Obj *bytecodeObj, int *position, Obj *form) {
  Obj *p = form->cdr;
  while(p && p->car) {
    visit_form(process, env, bytecodeObj, position, p->car);
    p = p->cdr;
  }
}

void add_let(Process *process, Obj *env, Obj *bytecodeObj, int *position, Obj *form) {
  
  Obj *bindings = form->cdr->car;
  Obj *body = form->cdr->cdr->car;
  shadow_stack_push(process, bindings);
  shadow_stack_push(process, body);

  //printf("bindings: %s\n", obj_to_string(process, bindings)->s);

  Obj *bindings_only_symbols = obj_new_array(bindings->count / 2);
  shadow_stack_push(process, bindings_only_symbols);
  
  for(int i = 0; i < bindings_only_symbols->count; i++) {
    bindings_only_symbols->array[i] = bindings->array[i * 2];
    visit_form(process, env, bytecodeObj, position, bindings->array[i * 2 + 1]);
  }
  //printf("bindings_only_symbols: %s\n", obj_to_string(process, bindings_only_symbols)->s);

  Obj *literals = bytecodeObj->bytecode_literals;
  char new_literal_index = literals->count;
  Obj *let_body_code = form_to_bytecode(process, env, body);

  obj_array_mut_append(literals, bindings_only_symbols);
  obj_array_mut_append(literals, let_body_code);
  
  bytecodeObj->bytecode[*position] = 't';
  bytecodeObj->bytecode[*position + 1] = new_literal_index + 65;
  bytecodeObj->bytecode[*position + 2] = new_literal_index + 1 + 65;
  //bytecodeObj->bytecode[*position + 3] = 's';

  *position += 3;

  shadow_stack_pop(process);
  shadow_stack_pop(process);
  shadow_stack_pop(process);
}

void add_def(Process *process, Obj *env, Obj *bytecodeObj, int *position, Obj *form) {
  visit_form(process, env, bytecodeObj, position, form->cdr->cdr->car);
  Obj *literals = bytecodeObj->bytecode_literals;
  char new_literal_index = literals->count;
  obj_array_mut_append(literals, form->cdr->car);
  bytecodeObj->bytecode[*position] = 'd';
  bytecodeObj->bytecode[*position + 1] = new_literal_index + 65;
  *position += 2;
}

void visit_form(Process *process, Obj *env, Obj *bytecodeObj, int *position, Obj *form) {
  if(eval_error) {
    return;
  }
  else if(form->tag == 'C') {
    if(form->car->car == NULL) {
      add_literal(bytecodeObj, position, nil);
    }
    else if(HEAD_EQ("quote")) {
      add_literal(bytecodeObj, position, form->car);
    }
    else if(HEAD_EQ("if")) {
      add_if(process, env, bytecodeObj, position, form);
    }
    else if(HEAD_EQ("do")) {
      add_do(process, env, bytecodeObj, position, form);
    }
    else if(HEAD_EQ("let")) {
      add_let(process, env, bytecodeObj, position, form);
    }
    else if(HEAD_EQ("def")) {
      add_def(process, env, bytecodeObj, position, form);
    }
    else if(HEAD_EQ("fn")) {
      Obj *lambda = obj_new_lambda(form->cdr->car, form_to_bytecode(process, env, form->cdr->cdr->car), env, form);
      add_literal(bytecodeObj, position, lambda);
    }
    else {
      add_call(process, env, bytecodeObj, position, form);
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

Obj *form_to_bytecode(Process *process, Obj *env, Obj *form) {
  char *code = malloc(2048);
  Obj *bytecodeObj = obj_new_bytecode(code);
  int position = 0;
  visit_form(process, env, bytecodeObj, &position, form);
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
 
  Obj *literal, *function, *lookup, *result, *bindings, *let_env;
  int arg_count, i, bindings_index, body_index;
  
  while(true) {
    
    Obj **literals_array = frames[frame].bytecodeObj->bytecode_literals->array;
    char *bytecode = frames[frame].bytecodeObj->bytecode;
    int p = frames[frame].p;
    char c = bytecode[p];
    
    //printf("frame = %d, c = %c\n", frame, c);
    
    switch(c) {
    case 'l':
      i = bytecode[p + 1] - 65;
      literal = literals_array[i];
      //printf("Pushing literal "); obj_print_cout(literal); printf("\n");
      stack_push(process, literal);
      frames[frame].p += 2;
      break;
    case 'd':
      i = bytecode[p + 1] - 65;
      literal = literals_array[i];
      result = env_extend(process->global_env, literal, stack_pop(process));
      stack_push(process, result->cdr);
      frames[frame].p += 2;
      break;
    case 't':
      //printf("entering let\n");
      //shadow_stack_push(process, let_env);

      bindings_index = bytecode[p + 1] - 65;
      body_index = bytecode[p + 2] - 65;
      
      bindings = literals_array[bindings_index];
      //printf("bindings: %s\n", obj_to_string(process, bindings)->s);

      let_env = obj_new_environment(frames[frame].env);
      for(int i = 0; i < bindings->count; i++) {
        env_extend(let_env, bindings->array[i], stack_pop(process));
      }

      frames[frame].p += 3;
    
      frames[frame + 1].p = 0;
      frames[frame + 1].bytecodeObj = literals_array[body_index];
      frames[frame + 1].env = let_env;
      frame++;

      //printf("will now execute: %s\n", obj_to_string(process, frames[frame].bytecodeObj)->s);

      break;
    case 's':
      frame--;
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
    case 'i':
      i = bytecode[p + 1] - 65;
      if(is_true(stack_pop(process))) {
        frames[frame].p = 0;
        frames[frame].bytecodeObj = literals_array[i];
        frames[frame].env = frames[frame - 1].env;
      }
      else {
        frames[frame].p = 0;
        frames[frame].bytecodeObj = literals_array[i + 1];
        frames[frame].env = frames[frame - 1].env;
      }
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
      else if(function->tag == 'F') {
        call_foreign_function(process, function, args, arg_count);
      }
      else if(function->tag == 'K') {
        if(arg_count != 1) {
          eval_error = obj_new_string("Args to keyword lookup must be a single arg.");
        }
        else if(args[0]->tag != 'E') {
          eval_error = obj_new_string("Arg 0 to keyword lookup must be a dictionary: ");
          obj_string_mut_append(eval_error, obj_to_string(process, args[0])->s);
        }
        else {
          Obj *value = env_lookup(process, args[0], function);
          if(value) {
            stack_push(process, value);
          } else {
            eval_error = obj_new_string("Failed to lookup keyword '");
            obj_string_mut_append(eval_error, obj_to_string(process, function)->s);
            obj_string_mut_append(eval_error, "'");
            obj_string_mut_append(eval_error, " in \n");
            obj_string_mut_append(eval_error, obj_to_string(process, args[0])->s);
            obj_string_mut_append(eval_error, "\n");
          }
        }
      }
      else if(function->tag == 'L') {
        Obj *calling_env = obj_new_environment(function->env);
        //printf("arg_count = %d\n", arg_count);
        env_extend_with_args(process, calling_env, function, arg_count, args, true);
        frame++;
        frames[frame].p = 0;
        if(function->body->tag != 'X') {
          set_error_return_nil("The body of the lambda must be bytecode, ", function);
        }
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

