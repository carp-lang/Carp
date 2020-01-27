typedef uint8_t byte;

uint8_t Byte__PLUS_(uint8_t x, uint8_t y) {
    return x + y;
}
uint8_t Byte__MINUS_(uint8_t x, uint8_t y) {
    return x - y;
}
uint8_t Byte__MUL_(uint8_t x, uint8_t y) {
    return x * y;
}
uint8_t Byte__DIV_(uint8_t x, uint8_t y) {
    return x / y;
}
bool Byte__EQ_(uint8_t x, uint8_t y) {
    return x == y;
}
bool Byte__LT_(uint8_t x, uint8_t y) {
    return x < y;
}
bool Byte__GT_(uint8_t x, uint8_t y) {
    return x > y;
}

uint8_t Byte_inc(uint8_t x) {
    return x + 1;
}
uint8_t Byte_dec(uint8_t x) {
    return x - 1;
}
uint8_t Byte_bit_MINUS_shift_MINUS_left(uint8_t x, uint8_t y) {
    return x << y;
}
uint8_t Byte_bit_MINUS_shift_MINUS_right(uint8_t x, uint8_t y) {
    return x >> y;
}
uint8_t Byte_bit_MINUS_and(uint8_t x, uint8_t y) {
    return x & y;
}
uint8_t Byte_bit_MINUS_or(uint8_t x, uint8_t y) {
    return x | y;
}
uint8_t Byte_bit_MINUS_xor(uint8_t x, uint8_t y) {
    return x ^ y;
}
uint8_t Byte_bit_MINUS_not(uint8_t x) {
    return ~x;
}

uint8_t Byte_copy(const uint8_t *x) {
    return *x;
}

uint8_t Byte_mod(uint8_t x, uint8_t divider) {
    return x % divider;
}

bool Byte_mask(uint8_t a, uint8_t b) {
    return a & b;
}

int Byte_to_MINUS_int(uint8_t a) {
    return a;
}

uint8_t Byte_from_MINUS_int(int a) {
    return a;
}
