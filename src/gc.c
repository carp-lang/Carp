#include "gc.h"

#define LOG_GC_KILLS 0

void obj_mark_alive(Obj *o) {
  if(!o || o->alive) {
    return;
  }
  
  o->alive = true;
  
  if(o->tag == 'C') {
    obj_mark_alive(o->car);
    obj_mark_alive(o->cdr);
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
}

void free_internal_data(Obj *dead) {
  if(dead->tag == 'F') {
    free(dead->cif);
  }
  else if(dead->tag == 'S' || dead->tag == 'Y' || dead->tag == 'K') {
    free(dead->s);
  }
}

void gc_sweep() {
  int kill_count = 0;
  Obj **p = &obj_latest;
  while(*p) {
    if(!(*p)->alive) {
      Obj *dead = *p;
      *p = dead->prev;
      free_internal_data(dead);
      //printf("free %p %c\n", dead, dead->tag);
      free(dead);
      obj_total--;
      kill_count++;
    }
    else {
      (*p)->alive = false; // for next gc collect
      p = &(*p)->prev;
    }
  }
  if(LOG_GC_KILLS) {
    printf("\e[33mDeleted %d Obj:s.\e[0m\n", kill_count);
  }
}

void gc(Obj *env, Obj *forms) {
  if(forms) {
    obj_mark_alive(forms);
  }
  obj_mark_alive(env);
  for(int i = 0; i < stack_pos - 1; i++) {
    obj_mark_alive(stack[i]);
  }
  gc_sweep();
}

void gc_all() {
  gc_sweep();
}

