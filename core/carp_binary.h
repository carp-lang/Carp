#include <stdint.h>

uint16_t Binary_to_MINUS_int16(uint8_t b1, uint8_t b2) {
  return (uint16_t) (b2 << 8) | b1;
}

uint32_t Binary_to_MINUS_int32(uint8_t b1, uint8_t b2, uint8_t b3, uint8_t b4) {
  return (uint32_t) b1 | (b2 << 8) | (b3 << 16) | (b4 << 24);
}

uint64_t Binary_to_MINUS_int64(uint8_t b1, uint8_t b2, uint8_t b3, uint8_t b4, uint8_t b5, uint8_t b6, uint8_t b7, uint8_t b8) {
  return (uint64_t) b1 | (b2 << 8) | (b3 << 16) | (b4 << 24) | ((uint64_t)b5 << 32) | 
    ((uint64_t)b6 << 40) | ((uint64_t)b7 << 48) | ((uint64_t)b8 << 56);
}

int Binary_system_MINUS_endianness_MINUS_internal() {
  // The int type is always >= 16 bits, two bytes, according to The C
  // Programming Language, Second Edition. Contrarily, char is always a single
  // byte. 
  //
  // Allocating 1 and converting to char will leave us with the first byte used
  // to represent the int. On a little endian machine, we're left with 1 on a
  // big endian machine, we're left with 0.
  unsigned int i = 1;
  // Conversion to char lets us access bytes individually.
  // We return the first byte.
  return (int) ((char*)&i)[0];
}
