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
// 't' let
// 'o' do
// 'r' reset!
// 'n' not
// 'w' or

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

void add_not(Process *process, Obj *env, Obj *bytecodeObj, int *position, Obj *form) {
  visit_form(process, env, bytecodeObj, position, form->cdr->car);
  bytecodeObj->bytecode[*position] = 'n';
  *position += 1;
}

void add_or(Process *process, Obj *env, Obj *bytecodeObj, int *position, Obj *form) {
  assert(false);
  /* char *code = malloc(512); */
  /* Obj *or_branch = obj_new_bytecode(code); */

  /* Obj *p = form->cdr; */
  /* int pos2 = 0; */
  /* while(p && p->car) { */
  /*   visit_form(process, env, or_branch, &pos2, p->car); */
  /*   p = p->cdr; */
  /* } */

  /* Obj *literals = bytecodeObj->bytecode_literals; */
  /* char new_literal_index = literals->count; */
  /* obj_array_mut_append(literals, or_branch); */
  /* bytecodeObj->bytecode[*position] = 'w'; */
  /* bytecodeObj->bytecode[*position + 1] = new_literal_index + 65; */
  /* *position += 2; */
}

void add_ref(Process *process, Obj *env, Obj *bytecodeObj, int *position, Obj *form) {
  visit_form(process, env, bytecodeObj, position, form->cdr->car);
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

void add_reset(Process *process, Obj *env, Obj *bytecodeObj, int *position, Obj *form) {
  visit_form(process, env, bytecodeObj, position, form->cdr->cdr->car);
  Obj *literals = bytecodeObj->bytecode_literals;
  char new_literal_index = literals->count;
  obj_array_mut_append(literals, form->cdr->car);
  bytecodeObj->bytecode[*position] = 'r';
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
    else if(HEAD_EQ("reset!")) {
      add_reset(process, env, bytecodeObj, position, form);
    }
    else if(HEAD_EQ("ref")) {
      add_ref(process, env, bytecodeObj, position, form);
    }
    else if(HEAD_EQ("or")) {
      add_or(process, env, bytecodeObj, position, form);
    }
    else if(HEAD_EQ("not")) {
      add_not(process, env, bytecodeObj, position, form);
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

// returns NULL if not done yet
Obj *bytecode_eval_internal(Process *process, Obj *bytecodeObj, int steps) {
  Obj *literal, *function, *lookup, *result, *bindings, *let_env, *binding;
  int arg_count, i, bindings_index, body_index;
  
  for(int step = 0; step < steps; step++) {

    if(eval_error) {
      return nil;
    }
    
    Obj **literals_array = process->frames[process->frame].bytecodeObj->bytecode_literals->array;
    char *bytecode = process->frames[process->frame].bytecodeObj->bytecode;
    int p = process->frames[process->frame].p;
    char c = bytecode[p];
    
    //printf("frame = %d, c = %c\n", frame, c);
    
    switch(c) {
    case 'l':
      i = bytecode[p + 1] - 65;
      literal = literals_array[i];
      //printf("Pushing literal "); obj_print_cout(literal); printf("\n");
      stack_push(process, literal);
      process->frames[process->frame].p += 2;
      break;
    case 'd':
      i = bytecode[p + 1] - 65;
      literal = literals_array[i];
      result = env_extend(process->global_env, literal, stack_pop(process));
      stack_push(process, result->cdr);
      process->frames[process->frame].p += 2;
      break;
    case 'n':
      if(is_true(stack_pop(process))) {
        stack_push(process, lisp_false);
      } else {
        stack_push(process, lisp_true);
      }
      process->frames[process->frame].p += 1;
      break;
    case 'r':
      i = bytecode[p + 1] - 65;
      literal = literals_array[i];
      binding = env_lookup_binding(process, process->frames[process->frame].env, literal);
      if(binding->car) {
        //printf("binding: %s\n", obj_to_string(process, binding)->s);
        binding->cdr = stack_pop(process);
        stack_push(process, binding->cdr);
      } else {
        eval_error = obj_new_string("reset! can't find variable to reset: ");
        obj_string_mut_append(eval_error, obj_to_string(process, literal)->s);
        return nil;
      }      
      process->frames[process->frame].p += 2;
      break;
    case 't':
      //printf("entering let\n");
      //shadow_stack_push(process, let_env);

      bindings_index = bytecode[p + 1] - 65;
      body_index = bytecode[p + 2] - 65;
      
      bindings = literals_array[bindings_index];
      //printf("bindings: %s\n", obj_to_string(process, bindings)->s);

      let_env = obj_new_environment(process->frames[process->frame].env);
      for(int i = 0; i < bindings->count; i++) {
        env_extend(let_env, bindings->array[i], stack_pop(process));
      }

      process->frames[process->frame].p += 3;
    
      process->frames[process->frame + 1].p = 0;
      process->frames[process->frame + 1].bytecodeObj = literals_array[body_index];
      process->frames[process->frame + 1].env = let_env;
      process->frame++;

      //printf("will now execute: %s\n", obj_to_string(process, process->frames[process->frame].bytecodeObj)->s);

      break;
    case 'y':
      i = bytecode[p + 1] - 65;
      literal = literals_array[i];
      //printf("Looking up literal "); obj_print_cout(literal); printf("\n");
      lookup = env_lookup(process, process->frames[process->frame].env, literal);
      if(!lookup) {
        set_error_return_nil("Failed to lookup ", literal);
      }
      stack_push(process, lookup);
      process->frames[process->frame].p += 2;
      break;
    case 'i':
      i = bytecode[p + 1] - 65;
      if(is_true(stack_pop(process))) {
        process->frames[process->frame].p = 0;
        process->frames[process->frame].bytecodeObj = literals_array[i];
        process->frames[process->frame].env = process->frames[process->frame - 1].env;
      }
      else {
        process->frames[process->frame].p = 0;
        process->frames[process->frame].bytecodeObj = literals_array[i + 1];
        process->frames[process->frame].env = process->frames[process->frame - 1].env;
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
      process->frames[process->frame].p += 2;

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
        process->frame++;
        process->frames[process->frame].p = 0;
        if(function->body->tag != 'X') {
          set_error_return_nil("The body of the lambda must be bytecode, ", function);
        }
        process->frames[process->frame].bytecodeObj = function->body;
        process->frames[process->frame].env = calling_env;
        //printf("Pushing new stack frame with bytecode '%s'\n", process->frames[process->frame].bytecode); // and env %s\n", process->frames[process->frame].bytecode, obj_to_string(process, calling_env)->s);
      }
      else {
        printf("Can't handle other calling methods yet %c\n", function->tag);
        obj_print_cout(function);
        return nil;
      }      
      break;
    case 'q':
      process->frame--;
      if(process->frame < 0) {
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

Obj *bytecode_eval(Process *process, Obj *bytecodeObj) {
  shadow_stack_push(process, bytecodeObj);
  
  process->frames[process->frame].p = 0;
  process->frames[process->frame].bytecodeObj = bytecodeObj;
  process->frames[process->frame].env = process->global_env;

  Obj *final_result = NULL;
  while(!final_result) {
    final_result = bytecode_eval_internal(process, bytecodeObj, 100);
  }

  shadow_stack_pop(process);
  return final_result;
}

