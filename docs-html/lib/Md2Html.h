#ifndef MD2HTML_H
#define MD2HTML_H

// forward definitions of required Carp functionality
typedef char *String;                   // copied from core.h
String String_from_MINUS_cstr(char *s); // implementation (!) is in carp_string.h


// this function does the actual work
String Md2Html_convert(String *in);


#endif
