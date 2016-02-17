/* typedef struct { */
/*   Obj *lambda; */
/*   Obj *caller_arg_types; */
/*   Obj  */
/* } LambdaCallInfo; */

void call_lambda_from_ffi(ffi_cif *cif, unsigned int *ret, void* args[], Obj *o) {
  printf("Calling lambda %s from ffi function!\n", obj_to_string(o)->s);
  //*ret = fputs(*(char **)args[0], user_data);

  int arg_count = cif->nargs;
  printf("arg count: %d\n", arg_count);

  Obj *obj_args[arg_count];
  
  for(int i = 0; i < arg_count; i++) {
    ///void *c_arg = args[i];
    if(cif->arg_types[i] == &ffi_type_sint) {
      int *x = args[i];
      obj_args[i] = obj_new_int(*x);
    }
    else if(cif->arg_types[i] == &ffi_type_float) {
      float *x = args[i];
      obj_args[i] = obj_new_float(*x);
    }
    else if(cif->arg_types[i] == &ffi_type_schar) {
      char *x = args[i];
      obj_args[i] = obj_new_char(*x);
    }
    else {
      //printf("Can't handle arg type %p when calling ffi function.\n", cif->arg_types[i]);
      obj_args[i] = obj_new_ptr(args[i]);
    }
    //printf("arg %d: %s\n", i, obj_to_string(obj_args[i])->s);
  }
  
  apply(o, obj_args, cif->nargs);
  *ret = stack_pop()->i;
}


// The following code can be used when sending functions as arguments to ffi functions:

 else if(args[i]->tag == 'L') {

   //ffi_cif *create_cif(Obj *args, int arg_count, Obj *return_type_obj, char *func_name)

   printf("Will call unbaked lambda from ffi function. Lambda should have types: %s\n", obj_to_string(type_obj)->s);
	    
   ffi_type *closure_args[0];
   ffi_closure *closure;
     
   void (*closure_fun_ptr)();
   int rc;
     
   /* Allocate closure and closure_fun_ptr */
   closure = ffi_closure_alloc(sizeof(ffi_closure), (void**)&closure_fun_ptr);
     
   if (closure) {
     /* Initialize the argument info vectors */
     closure_args[0] = &ffi_type_pointer;

     /* ffi_cif cif_static; */
     /* ffi_cif *cif = &cif_static; */
     /* ffi_prep_cif(cif, FFI_DEFAULT_ABI, 0, &ffi_type_void, closure_args); */

     Obj *lambda_arg_types = type_obj->cdr->car;
     Obj *lambda_return_type = type_obj->cdr->cdr->car;
     int lambda_arg_count = 0;
     Obj *p = lambda_arg_types;
     while(p && p->car) {
       p = p->cdr;
       lambda_arg_count++;
     }
		
     ffi_cif *cif = create_cif(lambda_arg_types, lambda_arg_count, lambda_return_type, "TODO:proper-name");

     /* Initialize the closure, setting stream to stdout */
     Obj *lambda_arg = args[i];

     typedef void (*LambdaCallback)(ffi_cif *, void *, void **, void *);
	      
     if (ffi_prep_closure_loc(closure, cif, (LambdaCallback)call_lambda_from_ffi, lambda_arg, closure_fun_ptr) == FFI_OK) {
       printf("Prep done\n");
       values[i] = &closure_fun_ptr;
     }
     else {
       set_error("Closure prep failed ", nil);
     }
   }
   else {
     set_error("Failed to allocate closure. ", nil);
   }
 }
