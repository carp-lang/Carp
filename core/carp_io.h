#pragma once
#include <stdio.h>

#include <carp_string.h>

void IO_println(String *s) { puts(*s); }
void IO_print(String *s) { printf("%s", *s); }

char IO_EOF = (char) EOF;

#ifndef _WIN32
String IO_get_MINUS_line() {
    size_t size = 1024;
    String buffer = CARP_MALLOC(size);
    getline(&buffer, &size, stdin);
    return buffer;
}
#endif

String IO_read_MINUS_file(String *filename) {
    String buffer = 0;
    long length;
    FILE *f = fopen(*filename, "rb");

    if(f) {
        fseek (f, 0, SEEK_END);
        length = ftell (f);
        fseek (f, 0, SEEK_SET);
        buffer = CARP_MALLOC (length + 1);
        if (buffer)	{
            fread (buffer, 1, length, f);
            buffer[length] = '\0';
        } else {
            printf("Failed to open buffer from file: %s\n", *filename);
            buffer = String_empty();
        }
        fclose (f);
    } else {
        printf("Failed to open file: %s\n", *filename);
        buffer = String_empty();
    }


    return buffer;
}

char IO_fgetc(FILE *f) {
    return (char) fgetc(f);
}

void IO_fclose(FILE *f) {
    fclose(f);
}

FILE *IO_fopen(String *filename, String *mode) {
    return fopen(*filename, *mode);
}
