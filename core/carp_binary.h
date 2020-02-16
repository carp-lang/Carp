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
