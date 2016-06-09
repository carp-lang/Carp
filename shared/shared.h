#pragma once

#ifndef CARP_SHARED_H
#define CARP_SHARED_H

#include "types.h"
#include "platform.h"

#define and &&

#ifdef WIN32
#define EXPORT __declspec(dllexport)
#else
#define EXPORT
#endif

typedef int unknown;
typedef void* typevar;
typedef void* any;

typedef char* string;

EXPORT int intsqrt(int x);
EXPORT float itof(int x);
EXPORT float dtof(double x);
EXPORT double ftod(float x);

#ifdef max
#undef max
#endif
EXPORT int max(int x, int y);

EXPORT string itos(int x);

EXPORT bool nullQMARK(void *p);

EXPORT bool not(bool x);

EXPORT void print(string msg);

EXPORT void println(string msg);

// This function is used for testing of the ownership system
EXPORT void eat_string(char *s);

EXPORT char *string_copy(char *s);

EXPORT char *string_append(char *s1, char *s2);

EXPORT bool file_existsQMARK(char *filename);

EXPORT int inc(int x);
EXPORT int dec(int x);

EXPORT void async(void *f);

EXPORT int last_index_of(string s, char c);

EXPORT string substring(string s, int index);

EXPORT string file_path_component(string s);

EXPORT string get_input();

EXPORT int mod(int x, int y);

#ifdef WIN32
EXPORT void sleep(int millis);
#endif

EXPORT CARP_PLATFORM platform();

EXPORT string get_normal_console_color();

EXPORT string get_console_color(int x);

EXPORT Array *chars(string s);

EXPORT string string_join(string separator, Array *array_of_strings);

#endif

