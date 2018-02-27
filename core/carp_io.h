#pragma once
#include <stdio.h>

#include <carp_string.h>

void IO_println(string *s) { puts(*s); }
void IO_print(string *s) { printf("%s", *s); }

string IO_get_MINUS_line() {
    size_t size = 1024;
    string buffer = CARP_MALLOC(size);
    getline(&buffer, &size, stdin);
    return buffer;
}

string IO_read_MINUS_file(string *filename) {
    string buffer = 0;
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

