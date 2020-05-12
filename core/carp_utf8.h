static size_t utf8len(const char *s) {
    size_t l = 0;
    for (size_t i = 0; s[i]; i++) l += (s[i] & 0xC0) != 0x80;
    return l;
}

static size_t wutf8clen(uint32_t c) {
    if (c < 0x80) {
        return 1;
    } else if (c < 0x800) {
        return 2;
    } else if (c < 0xd800 || c - 0xe000 < 0x2000) {
        return 3;
    } else if (c - 0x10000 < 0x100000) {
        return 4;
    }
    assert(0);
    return 0;
}

static size_t wutf8len(uint32_t *s, size_t sz) {
    size_t r = 0;
    for (size_t i = 0; i < sz; i++) r += wutf8clen(s[i]);
    return r;
}

static size_t utf8encode(char *s, uint32_t c) {
    if (c < 0x80) {
        *s = c;
        return 1;
    } else if (c < 0x800) {
        *s++ = 0xc0 | (c >> 6);
        *s = 0x80 | (c & 0x3f);
        return 2;
    } else if (c < 0xd800 || c - 0xe000 < 0x2000) {
        *s++ = 0xe0 | (c >> 12);
        *s++ = 0x80 | ((c >> 6) & 0x3f);
        *s = 0x80 | (c & 0x3f);
        return 3;
    } else if (c - 0x10000 < 0x100000) {
        *s++ = 0xf0 | (c >> 18);
        *s++ = 0x80 | ((c >> 12) & 0x3f);
        *s++ = 0x80 | ((c >> 6) & 0x3f);
        *s = 0x80 | (c & 0x3f);
        return 4;
    }
    assert(0);
    return 0;
}

// Adapted from: http://bjoern.hoehrmann.de/utf-8/decoder/dfa/
static uint32_t inline utf8decode(uint32_t *state, uint32_t *codep,
                                  uint32_t byte) {
    const uint32_t UTF8_ACCEPT = 0;
    const uint32_t UTF8_REJECT = 12;
    static const uint8_t utf8d[] = {
        // clang-format off
		// The first part of the table maps bytes to character classes that
		// to reduce the size of the transition table and create bitmasks.
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
		7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
		8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
		10,3,3,3,3,3,3,3,3,3,3,3,3,4,3,3, 11,6,6,6,5,8,8,8,8,8,8,8,8,8,8,8,

		// The second part is a transition table that maps a combination
		// of a state of the automaton and a character class to a state.
		0,12,24,36,60,96,84,12,12,12,48,72, 12,12,12,12,12,12,12,12,12,12,12,12,
		12, 0,12,12,12,12,12, 0,12, 0,12,12, 12,24,12,12,12,12,12,24,12,24,12,12,
		12,12,12,12,12,12,12,24,12,12,12,12, 12,24,12,12,12,12,12,12,12,24,12,12,
		12,12,12,12,12,12,12,36,12,36,12,12, 12,36,12,12,12,12,12,36,12,36,12,12,
		12,36,12,12,12,12,12,12,12,12,12,12,
        // clang-format on
    };
    uint32_t type = utf8d[byte];
    *codep = (*state != UTF8_ACCEPT) ? (byte & 0x3fu) | (*codep << 6)
                                     : (0xff >> type) & (byte);
    *state = utf8d[256 + *state + type];
    return *state;
}
