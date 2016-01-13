#pragma once

#define assert_or_return(assertion, ...) if(!(assertion)) { printf(_VA_ARGS_); printf("\n"); return; }
#define assert_or_return_nil(assertion, ...) if(!(assertion)) { printf(_VA_ARGS_); printf("\n"); return; }

#define set_error(message, obj) \
  error = concat_c_strings((message), obj_to_string((obj) ? (obj) : nil)->s); \
  stack_push(nil); \
  return;

#define set_error_and_return(message, obj) \
  error = concat_c_strings((message), obj_to_string((obj) ? (obj) : nil)->s); \
  return nil;

#define assert_or_set_error(assertion, message, obj)	\
  if(!(assertion)) {					\
    set_error(message, obj);				\
  }

