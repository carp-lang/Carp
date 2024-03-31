/*
** {======================================================
** PATTERN MATCHING adapted from lstrlib.c of Lua 5.3.4
** Copyright (C) 1994-2017 Lua.org, PUC-Rio; see LUA_LICENSE
** =======================================================
*/
#if !defined(CARP_MAXCAPTURES)
#define CARP_MAXCAPTURES 16
#endif

/* macro to 'unsign' a character */
#define uchar(c) ((Char)(c))

/*
** Some sizes are better limited to fit in 'int', but must also fit in
** 'size_t'. (We assume that Carp’s integer cannot be smaller than 'int'.)
*/
#define MAX_SIZET ((size_t)(~(size_t)0))

#define MAXSIZE (sizeof(size_t) < sizeof(int) ? MAX_SIZET : (size_t)(INT_MAX))

#define CAP_UNFINISHED (-1)
#define CAP_NONE (-2)

typedef struct PatternMatchState {
    String src_init; /* init of source String */
    String src_end;  /* end ('\0') of source String */
    String p_end;    /* end ('\0') of Pattern */
    int matchdepth;  /* control for recursive depth (to avoid C stack overflow)
                      */
    uint8_t level;   /* total number of captures (finished or unfinished) */
    struct {
        String init;
        ptrdiff_t len;
    } capture[CARP_MAXCAPTURES];
} PatternMatchState;

/* recursive function */
String Pattern_internal_match(PatternMatchState *ms, String s, String p);

/* maximum recursion depth for 'match' */
#if !defined(MAXCCALLS)
#define MAXCCALLS 200
#endif

#define C_ESC '\\'
#define SPECIALS "^$*+?.([\\-"

int carp_regerror(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vprintf(fmt, ap);
    va_end(ap);
    return -1;
}

int Pattern_internal_check_capture(PatternMatchState *ms, int l) {
    l -= '1';
    if (l < 0 || l >= ms->level || ms->capture[l].len == CAP_UNFINISHED) {
        return carp_regerror("invalid capture index %c%d", C_ESC, l + 1);
    }
    return l;
}

int Pattern_internal_capture_to_close(PatternMatchState *ms) {
    int level = ms->level;
    for (level--; level >= 0; level--) {
        if (ms->capture[level].len == CAP_UNFINISHED) return level;
    }
    return carp_regerror("invalid Pattern capture");
}

String Pattern_internal_classend(PatternMatchState *ms, String p) {
    switch (*p++) {
        case C_ESC: {
            if (p == ms->p_end)
                carp_regerror("malformed Pattern (ends with '%c')", C_ESC);
            return p + 1;
        }
        case '[': {
            if (*p == '^') p++;
            do { /* look for a ']' */
                if (p == ms->p_end)
                    carp_regerror("malformed Pattern (missing ']')");
                if (*(p++) == C_ESC && p < ms->p_end)
                    p++; /* skip escapes (e.g. '%]') */
            } while (*p != ']');
            return p + 1;
        }
        default:
            return p;
    }
}

int Pattern_internal_match_class(int c, int cl) {
    int res;
    switch (tolower(cl)) {
        case 'a':
            res = isalpha(c);
            break;
        case 'c':
            res = iscntrl(c);
            break;
        case 'd':
            res = isdigit(c);
            break;
        case 'g':
            res = isgraph(c);
            break;
        case 'l':
            res = islower(c);
            break;
        case 'p':
            res = ispunct(c);
            break;
        case 's':
            res = isspace(c);
            break;
        case 'u':
            res = isupper(c);
            break;
        case 'w':
            res = isalnum(c);
            break;
        case 'x':
            res = isxdigit(c);
            break;
        default:
            return (cl == c);
    }
    return (islower(cl) ? res : !res);
}

int Pattern_internal_matchbracketclass(int c, String p, String ec) {
    int sig = 1;
    if (*(p + 1) == '^') {
        sig = 0;
        p++; /* skip the '^' */
    }
    while (++p < ec) {
        if (*p == C_ESC) {
            p++;
            if (Pattern_internal_match_class(c, uchar(*p))) return sig;
        } else if ((*(p + 1) == '-') && (p + 2 < ec)) {
            p += 2;
            if (uchar(*(p - 2)) <= c && c <= uchar(*p)) return sig;
        } else if (uchar(*p) == c)
            return sig;
    }
    return !sig;
}

int Pattern_internal_singlematch(PatternMatchState *ms, String s, String p,
                                 String ep) {
    if (s >= ms->src_end) {
        return 0;
    } else {
        Char c = uchar(*s);
        switch (*p) {
            case '.':
                return 1; /* matches any char */
            case C_ESC:
                return Pattern_internal_match_class(c, uchar(*(p + 1)));
            case '[':
                return Pattern_internal_matchbracketclass(c, p, ep - 1);
            default:
                return (uchar(*p) == c);
        }
    }
}

String Pattern_internal_matchbalance(PatternMatchState *ms, String s,
                                     String p) {
    if (p >= ms->p_end - 1)
        carp_regerror("malformed Pattern (missing arguments to '%cb')", C_ESC);
    if (*s != *p) {
        return NULL;
    } else {
        int b = *p;
        int e = *(p + 1);
        int cont = 1;
        while (++s < ms->src_end) {
            if (*s == e) {
                if (--cont == 0) return s + 1;
            } else if (*s == b) {
                cont++;
            }
        }
    }
    return NULL; /* String ends out of balance */
}

String Pattern_internal_max_expand(PatternMatchState *ms, String s, String p,
                                   String ep) {
    ptrdiff_t i = 0; /* counts maximum expand for item */
    while (Pattern_internal_singlematch(ms, s + i, p, ep)) i++;
    /* keeps trying to match with the maximum repetitions */
    while (i >= 0) {
        String res = Pattern_internal_match(ms, (s + i), ep + 1);
        if (res) return res;
        i--; /* else didn't match; reduce 1 repetition to try again */
    }
    return NULL;
}

String Pattern_internal_min_expand(PatternMatchState *ms, String s, String p,
                                   String ep) {
    for (;;) {
        String res = Pattern_internal_match(ms, s, ep + 1);
        if (res)
            return res;
        else if (Pattern_internal_singlematch(ms, s, p, ep))
            s++; /* try with one more repetition */
        else
            return NULL;
    }
}

String Pattern_internal_start_capture(PatternMatchState *ms, String s, String p,
                                      int what) {
    String res;
    int level = ms->level;
    if (level >= CARP_MAXCAPTURES) carp_regerror("too many captures");
    ms->capture[level].init = s;
    ms->capture[level].len = what;
    ms->level = level + 1;
    if (!(res = Pattern_internal_match(ms, s, p)))
        ms->level--; /* undo capture on failed match */
    return res;
}

String Pattern_internal_end_capture(PatternMatchState *ms, String s, String p) {
    int l = Pattern_internal_capture_to_close(ms);
    String res;
    ms->capture[l].len = s - ms->capture[l].init; /* close capture */
    if (!(res = Pattern_internal_match(ms, s, p))) {
        ms->capture[l].len = CAP_UNFINISHED; /* undo capture */
    }
    return res;
}

String Pattern_internal_match_capture(PatternMatchState *ms, String s, int l) {
    size_t len;
    l = Pattern_internal_check_capture(ms, l);
    len = ms->capture[l].len;
    if ((size_t)(ms->src_end - s) >= len &&
        !memcmp(ms->capture[l].init, s, len)) {
        return s + len;
    }
    return NULL;
}

String Pattern_internal_match(PatternMatchState *ms, String s, String p) {
    if (ms->matchdepth-- == 0) carp_regerror("Pattern too complex");
init:                     /* using goto's to optimize tail recursion */
    if (p != ms->p_end) { /* end of Pattern? */
        switch (*p) {
            case '(': { /* start capture */
                if (*(p + 1) == ')')
                    s = Pattern_internal_start_capture(ms, s, p + 2, CAP_NONE);
                else
                    s = Pattern_internal_start_capture(ms, s, p + 1,
                                                       CAP_UNFINISHED);
                break;
            }
            case ')': { /* end capture */
                s = Pattern_internal_end_capture(ms, s, p + 1);
                break;
            }
            case '$': {
                if ((p + 1) != ms->p_end)
                    goto dflt; /* are we done as we should */
                s = (s == ms->src_end) ? s : NULL; /* check end of String */
                break;
            }
            case C_ESC: { /* escaped sequences not in the format class[*+?-]? */
                switch (*(p + 1)) {
                    case 'b': { /* balanced String? */
                        s = Pattern_internal_matchbalance(ms, s, p + 2);
                        if (s) {
                            p += 4;
                            goto init; /* return match(ms, s, p + 4); */
                        } /* else fail (s == NULL) */
                        break;
                    }
                    case 'f': { /* frontier? */
                        String ep;
                        Char previous;
                        p += 2;
                        if (*p != '[')
                            carp_regerror("missing '[' after '%cf' in Pattern",
                                          C_ESC);
                        ep = Pattern_internal_classend(
                            ms, p); /* points to what is next */
                        previous = (s == ms->src_init) ? '\0' : *(s - 1);
                        if (!Pattern_internal_matchbracketclass(uchar(previous),
                                                                p, ep - 1) &&
                            Pattern_internal_matchbracketclass(uchar(*s), p,
                                                               ep - 1)) {
                            p = ep;
                            goto init; /* return match(ms, s, ep); */
                        }
                        s = NULL; /* match failed */
                        break;
                    }
                    case 'r':   /* carriage return? */
                    case 'n':   /* newline? */
                    case 't': { /* tab? */
                        Char h = *(p + 1);
                        p += 2;
                        if ((*s == '\r' && h == 'r') ||
                            (*s == '\n' && h == 'n') ||
                            (*s == '\t' && h == 't')) {
                            s++;
                            goto init;
                        } else
                            s = NULL;
                        break;
                    }
                    case '0':
                    case '1':
                    case '2':
                    case '3':
                    case '4':
                    case '5':
                    case '6':
                    case '7':
                    case '8':
                    case '9': { /* capture results (\0-\9)? */
                        s = Pattern_internal_match_capture(ms, s,
                                                           uchar(*(p + 1)));
                        if (s) {
                            p += 2;
                            goto init; /* return match(ms, s, p + 2) */
                        }
                        break;
                    }
                    default:
                        goto dflt;
                }
                break;
            }
            default:
            dflt: { /* Pattern class plus optional suffix */
                String ep = Pattern_internal_classend(
                    ms, p); /* points to optional suffix */
                /* does not match at least once? */
                if (!Pattern_internal_singlematch(ms, s, p, ep)) {
                    if (*ep == '*' || *ep == '?' ||
                        *ep == '-') { /* accept empty? */
                        p = ep + 1;
                        goto init; /* return match(ms, s, ep + 1); */
                    } else {       /* '+' or no suffix */
                        s = NULL;  /* fail */
                    }
                } else {            /* matched once */
                    switch (*ep) {  /* handle optional suffix */
                        case '?': { /* optional */
                            String res;
                            if ((res = Pattern_internal_match(ms, s + 1,
                                                              ep + 1))) {
                                s = res;
                            } else {
                                p = ep + 1;
                                goto init; /* else return match(ms, s, ep + 1);
                                            */
                            }
                            break;
                        }
                        case '+': /* 1 or more repetitions */
                            s++;  /* 1 match already done */
                                  /* FALLTHROUGH */
                        case '*': /* 0 or more repetitions */
                            s = Pattern_internal_max_expand(ms, s, p, ep);
                            break;
                        case '-': /* 0 or more repetitions (minimum) */
                            s = Pattern_internal_min_expand(ms, s, p, ep);
                            break;
                        default: /* no suffix */
                            s++;
                            p = ep;
                            goto init; /* return match(ms, s + 1, ep); */
                    }
                }
                break;
            }
        }
    }
    ms->matchdepth++;
    return s;
}

String String_copy_len(String s, int len) {
    String ptr = CARP_MALLOC(len + 1);
    memcpy(ptr, s, len);
    ptr[len] = '\0';
    return ptr;
}

Array Array_push_String(Array a, String s, int i, int len) {
    ((String *)a.data)[i] = String_copy_len(s, len);
    return a;
}

Array Pattern_internal_push_onecapture(PatternMatchState *ms, int i, String s,
                                       String e, Array captures) {
    if (i >= ms->level) {
        if (!i)
            return Array_push_String(captures, s, i,
                                     e - s); /* add whole match */
        else
            carp_regerror("invalid capture index %cd", C_ESC, i + 1);
    } else {
        ptrdiff_t l = ms->capture[i].len;
        if (l == CAP_UNFINISHED)
            carp_regerror("unfinished capture");
        else if (l != CAP_NONE)
            return Array_push_String(captures, ms->capture[i].init, i,
                                     ms->capture[i].len);
    }
    return captures;
}

Array Pattern_internal_push_captures(PatternMatchState *ms, String s,
                                     String e) {
    int i;
    int nlevels = (ms->level == 0 && s) ? 1 : ms->level;
    Array res;
    res.len = nlevels;
    res.capacity = nlevels;
    res.data = CARP_MALLOC(nlevels * sizeof(String));
    for (i = 0; i < nlevels; i++)
        Pattern_internal_push_onecapture(ms, i, s, e, res);
    return res;
}

void Pattern_internal_prepstate(PatternMatchState *ms, String s, size_t ls,
                                String p, size_t lp) {
    ms->matchdepth = MAXCCALLS;
    ms->src_init = s;
    ms->src_end = s + ls;
    ms->p_end = p + lp;
}

void Pattern_internal_reprepstate(PatternMatchState *ms) {
    ms->level = 0;
    assert(ms->matchdepth == MAXCCALLS);
}

Array Pattern_match_MINUS_groups(Pattern *p, String *s) {
    String str = *s;
    Pattern pat = *p;
    int lstr = strlen(str);
    int lpat = strlen(pat);
    PatternMatchState ms;
    String s1 = str;
    int anchor = (*pat == '^');
    if (anchor) {
        pat++;
        lpat--; /* skip anchor character */
    }
    Pattern_internal_prepstate(&ms, str, lstr, pat, lpat);
    do {
        String res;
        Pattern_internal_reprepstate(&ms);
        if ((res = Pattern_internal_match(&ms, s1, pat))) {
            return Pattern_internal_push_captures(&ms, s1, res);
        }
    } while (s1++ < ms.src_end && !anchor);
    Array a;
    a.len = 0;
    a.capacity = 0;
    a.data = NULL;
    return a;
}

typedef struct PatternMatchResult {
    int start;  // negative start or end indicates a non-match
    int end;
} PatternMatchResult;

PatternMatchResult Pattern_match_MINUS_from(Pattern *p, String *s,
                                            int startpos) {
    PatternMatchResult result = {.start = -1, .end = -1};
    String str = *s + startpos;
    Pattern pat = *p;
    int lstr = strlen(str);
    int lpat = strlen(pat);
    PatternMatchState ms;
    String s1 = str;
    int anchor = (*pat == '^');
    if (anchor) {
        pat++;
        lpat--; /* skip anchor character */
    }
    Pattern_internal_prepstate(&ms, str, lstr, pat, lpat);
    do {
        String res;
        Pattern_internal_reprepstate(&ms);
        if ((res = Pattern_internal_match(&ms, s1, pat))) {
            result.start = startpos + (s1 - str);
            result.end = startpos + res - str;
            break;
        }
    } while (s1++ < ms.src_end && !anchor);
    return result;
}

/* state for 'gmatch' */
typedef struct PatternGMatchState {
    String src;           /* current position */
    String pat;           /* Pattern */
    String lastmatch;     /* end of last match */
    PatternMatchState ms; /* match state */
} PatternGMatchState;

typedef struct PatternGMatchRes {
    bool valid;
    Array data;
} PatternGMatchRes;

PatternGMatchRes Pattern_internal_gmatch_aux(PatternGMatchState *gm) {
    String src;
    Array a;
    for (src = gm->src; src <= gm->ms.src_end; src++) {
        String e;
        Pattern_internal_reprepstate(&gm->ms);
        if ((e = Pattern_internal_match(&gm->ms, src, gm->pat)) &&
            e != gm->lastmatch) {
            gm->src = gm->lastmatch = e;
            a = Pattern_internal_push_captures(&gm->ms, src, e);
            return (PatternGMatchRes){.valid = true, .data = a};
        }
    }
    memset(&a, 0, sizeof(a));
    return (PatternGMatchRes){.valid = false, .data = a}; /* not found */
}

Array Array_push_back(Array res, Array tmp) {
    res.len++;
    res.data = CARP_REALLOC(res.data, res.len * sizeof(Array));
    ((Array *)res.data)[res.len - 1] = tmp;
    return res;
}

Array Pattern_match_MINUS_all_MINUS_groups(Pattern *p, String *s) {
    String str = *s;
    Pattern pat = *p;
    int lstr = strlen(str);
    int lpat = strlen(pat);
    PatternGMatchState gm;
    Pattern_internal_prepstate(&gm.ms, str, lstr, pat, lpat);
    gm.src = str;
    gm.pat = pat;
    gm.lastmatch = NULL;
    Array res;
    res.len = 0;
    res.capacity = 0;
    res.data = NULL;
    PatternGMatchRes tmp = Pattern_internal_gmatch_aux(&gm);
    while (tmp.valid) {
        res = Array_push_back(res, tmp.data);
        tmp = Pattern_internal_gmatch_aux(&gm);
    }
    return res;
}

String Pattern_internal_add_char(String a, Char b) {
    if (!a) {
        String buffer = CARP_MALLOC(2);
        snprintf(buffer, 1, "%c", b);
        return buffer;
    }

    int len = strlen(a) + 2;
    String buffer = CARP_MALLOC(len);
    snprintf(buffer, len - 1, "%s%c", a, b);
    CARP_FREE(a);
    return buffer;
}

String Pattern_internal_add_value(PatternMatchState *ms, String res, String src,
                                  String e, String tr) {
    size_t l, i;
    l = strlen(tr);
    for (i = 0; i < l; i++) {
        if (tr[i] != C_ESC)
            res = Pattern_internal_add_char(res, tr[i]);
        else {
            i++; /* skip ESC */
            if (!isdigit(uchar(tr[i]))) {
                if (tr[i] != C_ESC) {
                    carp_regerror("invalid use of '%c' in replacement String",
                                  C_ESC);
                }
                res = Pattern_internal_add_char(res, tr[i]);
            } else if (tr[i] == '0')
                res = String_append(&res, &src);
            else {
                Array a = {.len = 0, .capacity = 0, .data = NULL};
                Pattern_internal_push_onecapture(ms, tr[i] - '1', src, e, a);
                res = String_append(
                    &res,
                    &((String *)
                          a.data)[0]); /* add capture to accumulated result */
            }
        }
    }
    return res;
}

String Pattern_substitute(Pattern *p, String *s, String *t, int ns) {
    String str = *s;
    Pattern pat = *p;
    String tr = *t;
    int lstr = strlen(str);
    int lpat = strlen(pat);
    String lastmatch = NULL; /* end of last match */
    int anchor = (*pat == '^');
    String res = NULL;
    PatternMatchState ms;
    int n = 0;
    if (anchor) {
        pat++;
        lpat--; /* skip anchor character */
    }
    Pattern_internal_prepstate(&ms, str, lstr, pat, lpat);
    while (n < ns || ns == -1) {
        String e;
        Pattern_internal_reprepstate(&ms); /* (re)prepare state for new match */
        if ((e = Pattern_internal_match(&ms, str, pat)) &&
            e != lastmatch) { /* match? */
            n++;
            res = Pattern_internal_add_value(
                &ms, res, str, e, tr); /* add replacement to buffer */
            str = lastmatch = e;
        } else if (str < ms.src_end)
            res = Pattern_internal_add_char(res, *str++);
        else
            break; /* end of subject */
        if (anchor) break;
    }

    if (!res) return String_copy(&str);

    int l = strlen(res) + strlen(str) + 1;
    String buffer = CARP_MALLOC(l);
    snprintf(buffer, l - 1, "%s%s", res, str);
    CARP_FREE(res);
    return buffer;
}

Pattern Pattern_copy(Pattern *p) {
    size_t len = strlen(*p) + 1;
    Pattern ptr = CARP_MALLOC(len);
    return (Pattern)memcpy(ptr, *p, len);
}

void Pattern_delete(Pattern p) {
    CARP_FREE(p);
}

Pattern Pattern_init(String *p) {
    return Pattern_copy(p);
}

String Pattern_str(Pattern *p) {
    return Pattern_copy(p);
}

String Pattern_prn(Pattern *p) {
    int n = strlen(*p) + 4;
    String buffer = CARP_MALLOC(n);
    snprintf(buffer, n - 1, "#\"%s\"", *p);
    return buffer;
}

bool Pattern__EQ_(Pattern *a, Pattern *b) {
    return strcmp(*a, *b) == 0;
}
