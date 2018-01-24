// Bool
bool Bool_copy(bool* b) {
  return *b;
}

bool Bool__EQ_(bool a, bool b) {
  return a == b;
}

bool Bool__DIV__EQ_(bool a, bool b) {
  return a != b;
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

