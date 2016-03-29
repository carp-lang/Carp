#include "gc.h"

#define LOG_GC_KILL_COUNT 0
#define LOG_FREE 0

void obj_mark_alive(Obj *o) {
  if(!o || o->alive) {
    return;
  }

  //printf("marking %p alive: ", o); obj_print_cout(o); printf("\n");
  
  o->alive = true;
  obj_mark_alive(o->meta);
  
  if(o->tag == 'C') {
    obj_mark_alive(o->car);
    obj_mark_alive(o->cdr);
  }
  else if(o->tag == 'A') {
    for(int i = 0; i < o->count; i++) {
      obj_mark_alive(o->array[i]);
    }
  }
  else if(o->tag == 'L' || o->tag == 'M') {
    obj_mark_alive(o->params);
    obj_mark_alive(o->body);
    obj_mark_alive(o->env);
    obj_mark_alive(o->code);
  }
  else if(o->tag == 'E') {
    obj_mark_alive(o->parent);
    obj_mark_alive(o->bindings);
  }
  else if(o->tag == 'F') {
    obj_mark_alive(o->arg_types);
    obj_mark_alive(o->return_type);
  }

  // TODO: remove data pointed to by void_ptr:s? (tag 'Q')
}

void free_internal_data(Obj *dead) {
  if(dead->given_to_ffi) {
    // ignore this object
  }
  else if(dead->tag == 'F') {
    free(dead->cif);
    free(dead->name);
  }
  else if(dead->tag == 'S' || dead->tag == 'Y' || dead->tag == 'K') {
    /* if(dead->tag == 'S') { */
    /*   printf("freeing '%s'\n", dead->s); */
    /*   dead->deathwish = dead->s; */
    /*   return; */
    /* } */
    free(dead->s);
  }
  else if(dead->tag == 'A') {
    free(dead->array);
  }
}

void gc_sweep() {
  int kill_count = 0;
  Obj **p = &obj_latest;
  while(*p) {
    if(!(*p)->alive) {
      Obj *dead = *p;

      if(LOG_FREE) {
        printf("free ");
        printf("%p %c ", dead, dead->tag);
        //obj_print_cout(dead);
        printf("\n");
      }

      *p = dead->prev;
      free_internal_data(dead);

      //memset(dead, 0, sizeof(Obj));
      free(dead);
      
      /* if(dead->tag == 'A') free(dead); */
      /* else if(dead->tag == 'B') free(dead); */
      /* else if(dead->tag == 'C') free(dead); */
      /* else if(dead->tag == 'D') free(dead); */
      /* else if(dead->tag == 'E') free(dead); */
      /* else if(dead->tag == 'F') free(dead); */
      /* else if(dead->tag == 'G') free(dead); */
      /* else if(dead->tag == 'H') free(dead); */
      /* else if(dead->tag == 'I') free(dead); */
      /* else if(dead->tag == 'K') free(dead); */
      /* else if(dead->tag == 'L') free(dead); */
      /* else if(dead->tag == 'M') free(dead); */
      /* else if(dead->tag == 'N') free(dead); */
      /* else if(dead->tag == 'O') free(dead); */
      /* else if(dead->tag == 'P') free(dead); */
      /* else if(dead->tag == 'Q') free(dead); */
      /* else if(dead->tag == 'R') free(dead); */
      /* else if(dead->tag == 'S') free(dead); */
      /* else if(dead->tag == 'T') free(dead); */
      /* else if(dead->tag == 'U') free(dead); */
      /* else if(dead->tag == 'V') free(dead); */
      /* else if(dead->tag == 'X') free(dead); */
      /* else if(dead->tag == 'Y') free(dead); */
      /* else if(dead->tag == 'Z') free(dead); */
      /* else { */
      /*   printf("Can't free object with invalid tag: %c\n", dead->tag); */
      /* } */
      
      obj_total--;
      kill_count++;
    }
    else {
      (*p)->alive = false; // for next gc collect
      p = &(*p)->prev;
    }
  }
  if(LOG_GC_KILL_COUNT) {
    printf("\e[33mGC:d %d Obj:s, %d left.\e[0m\n", kill_count, obj_total);
  }
}

void gc(Obj *env) {
  obj_mark_alive(env);
  for(int i = 0; i < stack_pos; i++) {
    obj_mark_alive(stack[i]);
  }
  for(int i = 0; i < shadow_stack_pos; i++) {
    obj_mark_alive(shadow_stack[i]);
  }
  for(int i = 0; i < function_trace_pos; i++) {
    obj_mark_alive(function_trace[i].caller);
    obj_mark_alive(function_trace[i].callee);
  }
  gc_sweep();
}

void gc_all() {
  gc_sweep();
}

