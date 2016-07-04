#pragma once

#define assert_or_return(assertion, ...) if(!(assertion)) { printf(_VA_ARGS_); printf("\n"); return; }
#define assert_or_return_nil(assertion, ...) if(!(assertion)) { printf(_VA_ARGS_); printf("\n"); return; }

#define set_error(message, obj) \
  eval_error = concat_c_strings((message), obj_to_string(process, (obj) ? (obj) : nil)->s); \
  stack_push(process, nil);                                              \
  return;

#define set_error_return_nil(message, obj) \
  eval_error = concat_c_strings((message), obj_to_string(process, (obj) ? (obj) : nil)->s); \
  return nil;

#define set_error_return_null(message, obj) \
  eval_error = concat_c_strings((message), obj_to_string(process, (obj) ? (obj) : nil)->s); \
  return NULL;

#define set_error_and_return(message, obj) \
  eval_error = concat_c_strings((message), obj_to_string(process, (obj) ? (obj) : nil)->s); \
  return nil;

#define assert_or_set_error(assertion, message, obj)	\
  if(!(assertion)) {					\
    set_error(message, obj);				\
  }

#define assert_or_set_error_return_nil(assertion, message, obj)	\
  if(!(assertion)) {					\
    set_error_return_nil(message, obj);				\
  }

#define assert_or_set_error_return_null(assertion, message, obj)	\
  if(!(assertion)) {					\
    set_error_return_null(message, obj);				\
  }

#define assert_or_fatal_error(assertion, message) \
  if(!(assertion)){                               \
    puts(message);                                \
    exit(1);                                      \
  }

