#pragma once
#include <string.h>

#include <carp_memory.h>
#include <core.h>

void String_delete(string s) {
    CARP_FREE(s);
}

string String_copy(string *s) {
    size_t len = strlen(*s) + 1;
    string ptr = CARP_MALLOC(len);

    if (ptr == NULL) {
      return NULL;
    }

    return (string) memcpy(ptr, *s, len);
}

bool String__EQ_(string *a, string *b) {
    return strcmp(*a, *b) == 0;
}

string String_append(string a, string b) {
    int la = strlen(a);
    int lb = strlen(b);
    int total = la + lb + 1;
    string buffer = CARP_MALLOC(total);
    snprintf(buffer, total, "%s%s", a, b);
    CARP_FREE(a);
    CARP_FREE(b);
    return buffer;
}

int String_count(string *s) {
    return strlen(*s);
}

// Replace with 'copy' later:
string String_duplicate(string *s) {
    return String_copy(s);
}

char* String_cstr(string *s) {
    return *s;
}

string String_str(string *s) {
    int n = strlen(*s) + 1;
    string buffer = CARP_MALLOC(n);
    snprintf(buffer, n, "%s", *s);
    return buffer;
}


string String_prn(string *s) {
    int n = strlen(*s) + 4;
    string buffer = CARP_MALLOC(n);
    snprintf(buffer, n, "@\"%s\"", *s);
    return buffer;
}

char String_char_MINUS_at(string* s, int i) {
  return (*s)[i];
}

string String_format(string *str, string *s) {
    int size = snprintf(NULL, 0, *str, *s)+1;
    string buffer = CARP_MALLOC(size);
    snprintf(buffer, size, *str, *s);
    return buffer;
}

Array String_chars(string *s) {
    Array chars;
    chars.len = strlen(*s);
    chars.data = String_copy(s);
    return chars;
}

string String_from_MINUS_chars(Array a) {
    string s = CARP_MALLOC(a.len+1);
    memmove(s, a.data, a.len);
    s[a.len] = '\0';
    return s;
}

string String_tail(string* s) {
  int len = strlen(*s);
  string news = CARP_MALLOC(len);
  memcpy(news, (*s)+1, len-1);
  news[len-1] = '\0';
  return news;
}

string String_empty() {
    string s = CARP_MALLOC(1);
    s[0] = '\0';
    return s;
}

string Bool_str(bool b) {
    string true_str = "true";
    string false_str = "false";
    if(b) {
        return String_copy(&true_str);
    } else {
        return String_copy(&false_str);
    }
}

string Bool_format(string* str, bool b) {
    int size = snprintf(NULL, 0, *str, b)+1;
    string buffer = CARP_MALLOC(size);
    snprintf(buffer, size, *str, b);
    return buffer;
}

string Char_str(char c) {
    string buffer = CARP_MALLOC(3);
    snprintf(buffer, 3, "\\%c", c);
    return buffer;
}

string Char_format(string* str, char b) {
    int size = snprintf(NULL, 0, *str, b)+1;
    string buffer = CARP_MALLOC(size);
    snprintf(buffer, size, *str, b);
    return buffer;
}

string Double_str(double x) {
    int size = snprintf(NULL, 0, "%g", x)+1;
    string buffer = CARP_MALLOC(size);
    snprintf(buffer, size, "%g", x);
    return buffer;
}

string Double_format(string* s, double x) {
    int size = snprintf(NULL, 0, *s, x)+1;
    string buffer = CARP_MALLOC(size);
    snprintf(buffer, size, *s, x);
    return buffer;
}

string Float_str(float x) {
    int size = snprintf(NULL, 0, "%gf", x)+1;
    string buffer = CARP_MALLOC(size);
    snprintf(buffer, size, "%gf", x);
    return buffer;
}

string Float_format(string* str, float x) {
    int size = snprintf(NULL, 0, *str, x)+1;
    string buffer = CARP_MALLOC(size);
    snprintf(buffer, size, *str, x);
    return buffer;
}

string Int_str(int x) {
    int size = snprintf(NULL, 0, "%d", x)+1;
    string buffer = CARP_MALLOC(size);
    snprintf(buffer, size, "%d", x);
    return buffer;
}

string Int_format(string* str, int x) {
    int size = snprintf(NULL, 0, *str, x)+1;
    string buffer = CARP_MALLOC(size);
    snprintf(buffer, size, *str, x);
    return buffer;
}

int Int_from_MINUS_string(string *s) {
    return atoi(*s);
}

string Long_str(long x) {
    int size = snprintf(NULL, 0, "%ldl", x)+1;
    string buffer = CARP_MALLOC(size);
    snprintf(buffer, size, "%ldl", x);
    return buffer;
}

string Long_format(string* str, long x) {
    int size = snprintf(NULL, 0, *str, x)+1;
    string buffer = CARP_MALLOC(size);
    snprintf(buffer, size, *str, x);
    return buffer;
}

long Long_from_MINUS_string(string *s) {
    return atol(*s);
}
