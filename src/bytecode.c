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
// 'l' push <literal>
// 'p' push nil
// 'c' call <argcount>
// 'y' lookup <literal>
// 'd' def <literal>
// 't' let
// 'o' do
// 'r' reset!
// 'n' not
// 'i' jump if false
// 'j' jump (no matter what)

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

  visit_form(process, env, bytecodeObj, position, form->cdr->car); // expression
  bytecodeObj->bytecode[*position] = 'i'; // if
  *position += 1;

  int jump_to_false_pos = *position;
  bytecodeObj->bytecode[*position] = '?'; // amount to jump when expression is false
  *position += 1;
  
  visit_form(process, env, bytecodeObj, position, form->cdr->cdr->car); // true branch

  int jump_from_true_pos = *position + 1;
  bytecodeObj->bytecode[*position + 0] = 'j';
  bytecodeObj->bytecode[*position + 1] = '?'; // amount to jump when true is done
  *position += 2;

  // Now we know where the false branch begins, jump here if the expression is true:
  bytecodeObj->bytecode[jump_to_false_pos] = *position + 65;
  
  visit_form(process, env, bytecodeObj, position, form->cdr->cdr->cdr->car); // false branch

  // Now we know where the whole block ends, jump here when true branch is done:
  bytecodeObj->bytecode[jump_from_true_pos] = *position + 65;
}

void add_while(Process *process, Obj *env, Obj *bytecodeObj, int *position, Obj *form) {
  int start = *position;

  visit_form(process, env, bytecodeObj, position, form->cdr->car);
  bytecodeObj->bytecode[*position] = 'i'; // if
  *position += 1;
  int jump_pos = *position;
  bytecodeObj->bytecode[*position] = '?'; // amount to jump
  *position += 1;
  
  visit_form(process, env, bytecodeObj, position, form->cdr->cdr->car);

  bytecodeObj->bytecode[*position + 0] = 'j'; // go back to start
  bytecodeObj->bytecode[*position + 1] = start + 65;
  *position += 2;

  // Now we know where to jump to if the while expression is false:
  bytecodeObj->bytecode[jump_pos] = *position + 65;

  bytecodeObj->bytecode[*position + 0] = 'p'; // the while loop produces nil as a value
  *position += 1;
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
    if(form->car == NULL) {
      add_literal(bytecodeObj, position, nil);
    }
    else if(form->car->car == NULL) {
      add_literal(bytecodeObj, position, nil); // is this case needed?
    }
    else if(HEAD_EQ("quote")) {
      add_literal(bytecodeObj, position, form->cdr->car);
    }
    else if(HEAD_EQ("if")) {
      add_if(process, env, bytecodeObj, position, form);
    }
    else if(HEAD_EQ("while")) {
      add_while(process, env, bytecodeObj, position, form);
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
    else if(HEAD_EQ("not")) {
      add_not(process, env, bytecodeObj, position, form);
    }
    else if(HEAD_EQ("fn")) {
      Obj *lambda = obj_new_lambda(form->cdr->car, form_to_bytecode(process, env, form->cdr->cdr->car), env, form);
      add_literal(bytecodeObj, position, lambda);
    }
    else if(HEAD_EQ("macro")) {
      Obj *macro = obj_new_macro(form->cdr->car, form_to_bytecode(process, env, form->cdr->cdr->car), env, form);
      add_literal(bytecodeObj, position, macro);
    }
    else {
      Obj *lookup = env_lookup(process, env, form->car);
      if(lookup && lookup->tag == 'M') {
        Obj *macro = lookup;

        Obj *calling_env = obj_new_environment(macro->env);

        Obj *argp = form->cdr;
        int arg_count = 0;
        while(argp && argp->car) {
          argp = argp->cdr;
          arg_count++;
        }

        //printf("Arg count: %d\n", arg_count);

        argp = form->cdr;
        Obj *args = obj_new_array(arg_count);
        for(int i = 0; i < arg_count; i++) {
          args->array[i] = argp->car;
          argp = argp->cdr;
        }

        //printf("Args: %s\n", obj_to_string(process, args)->s);
        
        env_extend_with_args(process, calling_env, macro, arg_count, args->array, true);
        if(eval_error) {
          return;
        }

        process->frame++;
        process->frames[process->frame].p = 0;
        if(macro->body->tag != 'X') {
          set_error("The body of the macro must be bytecode: ", macro);
          return;
        }
        process->frames[process->frame].bytecodeObj = macro->body;
        process->frames[process->frame].env = calling_env;

        shadow_stack_push(process, bytecodeObj);
        
        Obj *expanded = NULL;
        while(!expanded) {
          expanded = bytecode_eval_internal(process, bytecodeObj, 1000);
          if(eval_error) {
            return;
          }
        }

        shadow_stack_pop(process);
        
        //printf("Expanded '%s' to %s\n", obj_to_string(process, form->car)->s, obj_to_string(process, expanded)->s);
        visit_form(process, env, bytecodeObj, position, expanded);
      }
      else {
        add_call(process, env, bytecodeObj, position, form);
      }
    }
  }
  else if(form->tag == 'Y') {
    add_lookup(bytecodeObj, position, form);
  }
  else {
    add_literal(bytecodeObj, position, form);
  }
}

Obj *form_to_bytecode(Process *process, Obj *env, Obj *form) {
  char *code = malloc(2048);
  Obj *bytecodeObj = obj_new_bytecode(code);
  int position = 0;
  visit_form(process, env, bytecodeObj, &position, form);
  bytecodeObj->bytecode[position++] = 'q';
  bytecodeObj->bytecode[position++] = '\0';
  //printf("Converted '%s' to bytecode: %s\n", obj_to_string(process, form)->s, obj_to_string(process, bytecodeObj)->s);
  return bytecodeObj;
}

// returns NULL if not done yet
Obj *bytecode_eval_internal(Process *process, Obj *bytecodeObj, int steps) {
  Obj *literal, *function, *lookup, *result, *bindings, *let_env, *binding;
  int arg_count, i, bindings_index, body_index;
  
  for(int step = 0; step < steps; step++) {
    
    if(eval_error) {
      return NULL;
    }
    
    Obj **literals_array = process->frames[process->frame].bytecodeObj->bytecode_literals->array;
    char *bytecode = process->frames[process->frame].bytecodeObj->bytecode;
    int p = process->frames[process->frame].p;
    char c = bytecode[p];
    
    //printf("frame = %d, p = %d,  c = %c\n", process->frame, p, c);
    
    switch(c) {
    case 'p':
      stack_push(process, nil);
      process->frames[process->frame].p += 1;
      break;
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
        return NULL;
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
        set_error_return_null("Failed to lookup ", literal);
      }
      stack_push(process, lookup);
      process->frames[process->frame].p += 2;
      break;
    case 'i':
      if(is_true(stack_pop(process))) {
        // don't jump, just skip over the next instruction (which is the length of the jump)
        process->frames[process->frame].p += 2;
      } else {
        // jump if false!
        int i = bytecode[p + 1] - 65;
        process->frames[process->frame].p = i;
      }
      break;
    case 'j':
      process->frames[process->frame].p = bytecode[p + 1] - 65;
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
        if(process->frame >= BYTECODE_FRAME_SIZE - 1) {
          set_error_return_null("Bytecode stack overflow. ", nil);
        }
        
        Obj *calling_env = obj_new_environment(function->env);
        //printf("arg_count = %d\n", arg_count);
        env_extend_with_args(process, calling_env, function, arg_count, args, true);
        process->frame++;
        process->frames[process->frame].p = 0;
        if(function->body->tag != 'X') {
          set_error_return_null("The body of the lambda must be bytecode, ", function);
        }
        process->frames[process->frame].bytecodeObj = function->body;
        process->frames[process->frame].env = calling_env;
        //printf("Pushing new stack frame with bytecode '%s'\n", process->frames[process->frame].bytecode); // and env %s\n", process->frames[process->frame].bytecode, obj_to_string(process, calling_env)->s);
      }
      else {
        set_error_return_null("Can't call \n", function);
      }      
      break;
    case 'q':
      process->frame--;
      if(process->frame < 0) {
        return stack_pop(process);
      }
      break;
    default:
      printf("Unhandled instruction: %c\n", c);
      exit(-1);
    }
  }

  return NULL;
}

Obj *bytecode_eval(Process *process, Obj *bytecodeObj, bool restart) {

  if(bytecodeObj->tag != 'X') {
    set_error_return_nil("The code to eval must be bytecode, ", bytecodeObj);
  }
  
  shadow_stack_push(process, bytecodeObj);

  if(restart) {
    process->frame = 0;
  }
  
  process->frames[process->frame].p = 0;
  process->frames[process->frame].bytecodeObj = bytecodeObj;
  process->frames[process->frame].env = process->global_env;

  Obj *final_result = NULL;
  while(!final_result) {
    final_result = bytecode_eval_internal(process, bytecodeObj, 20);
    if(eval_error) {
      final_result = nil;
      break;
    }
  }
  //printf("Final result = %s\n", obj_to_string(process, final_result)->s);

  shadow_stack_pop(process);
  return final_result;
}

