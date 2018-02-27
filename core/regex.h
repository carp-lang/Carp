#include <ctype.h>
#include <stdarg.h>
#include <stddef.h>

#include <core.h>

#if !defined(CARP_MAXCAPTURES)
#define CARP_MAXCAPTURES 16
#endif


/* macro to 'unsign' a character */
#define uchar(c) ((unsigned char)(c))


/*
** Some sizes are better limited to fit in 'int', but must also fit in
** 'size_t'. (We assume that Carp’s integer cannot be smaller than 'int'.)
*/
#define MAX_SIZET ((size_t)(~(size_t)0))

#define MAXSIZE (sizeof(size_t) < sizeof(int) ? MAX_SIZET : (size_t)(INT_MAX))


/*
** {======================================================
** PATTERN MATCHING
** =======================================================
*/


#define CAP_UNFINISHED (-1)
#define CAP_NONE (-2)

typedef struct MatchState {
  string src_init;  /* init of source string */
  string src_end;  /* end ('\0') of source string */
  string p_end;  /* end ('\0') of pattern */
  int matchdepth;  /* control for recursive depth (to avoid C stack overflow) */
  unsigned char level;  /* total number of captures (finished or unfinished) */
  struct {
    string init;
    ptrdiff_t len;
  } capture[CARP_MAXCAPTURES];
} MatchState;

/* recursive function */
static string match (MatchState *ms, string s, string p);

/* maximum recursion depth for 'match' */
#if !defined(MAXCCALLS)
#define MAXCCALLS 200
#endif

#define C_ESC '\\'
#define SPECIALS "^$*+?.([\\-"

int carp_regerror(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vprintf(fmt, ap);
  va_end(ap);
  return -1;
}

static int check_capture(MatchState *ms, int l) {
  l -= '1';
  if (l < 0 || l >= ms->level || ms->capture[l].len == CAP_UNFINISHED) {
    return carp_regerror("invalid capture index %c%d", C_ESC, l + 1);
  }
  return l;
}

static int capture_to_close(MatchState *ms) {
  int level = ms->level;
  for (level--; level>=0; level--) {
    if (ms->capture[level].len == CAP_UNFINISHED) return level;
  }
  return carp_regerror("invalid pattern capture");
}

static string classend(MatchState *ms, string p) {
  switch (*p++) {
    case C_ESC: {
      if (p == ms->p_end) carp_regerror("malformed pattern (ends with '%c')", C_ESC);
      return p+1;
    }
    case '[': {
      if (*p == '^') p++;
      do {  /* look for a ']' */
        if (p == ms->p_end) carp_regerror("malformed pattern (missing ']')");
        if (*(p++) == C_ESC && p < ms->p_end) p++;  /* skip escapes (e.g. '%]') */
      } while (*p != ']');
      return p+1;
    }
    default: {
      return p;
    }
  }
}

static int match_class(int c, int cl) {
  int res;
  switch (tolower(cl)) {
    case 'a' : res = isalpha(c); break;
    case 'c' : res = iscntrl(c); break;
    case 'd' : res = isdigit(c); break;
    case 'g' : res = isgraph(c); break;
    case 'l' : res = islower(c); break;
    case 'p' : res = ispunct(c); break;
    case 's' : res = isspace(c); break;
    case 'u' : res = isupper(c); break;
    case 'w' : res = isalnum(c); break;
    case 'x' : res = isxdigit(c); break;
    default: return (cl == c);
  }
  return (islower(cl) ? res : !res);
}

static int matchbracketclass(int c, string p, string ec) {
  int sig = 1;
  if (*(p+1) == '^') {
    sig = 0;
    p++;  /* skip the '^' */
  }
  while (++p < ec) {
    if (*p == C_ESC) {
      p++;
      if (match_class(c, uchar(*p))) return sig;
    }
    else if ((*(p+1) == '-') && (p+2 < ec)) {
      p+=2;
      if (uchar(*(p-2)) <= c && c <= uchar(*p)) return sig;
    }
    else if (uchar(*p) == c) return sig;
  }
  return !sig;
}

static int singlematch(MatchState *ms, string s, string p,
                       string ep) {
  if (s >= ms->src_end) {
    return 0;
  } else {
    int c = uchar(*s);
    switch (*p) {
      case '.': return 1;  /* matches any char */
      case C_ESC: return match_class(c, uchar(*(p+1)));
      case '[': return matchbracketclass(c, p, ep-1);
      default:  return (uchar(*p) == c);
    }
  }
}

static string matchbalance(MatchState *ms, string s, string p) {
  if (p >= ms->p_end - 1) carp_regerror("malformed pattern (missing arguments to '%cb')", C_ESC);
  if (*s != *p) {
    return NULL;
  } else {
    int b = *p;
    int e = *(p+1);
    int cont = 1;
    while (++s < ms->src_end) {
      if (*s == e) {
        if (--cont == 0) return s+1;
      }
      else if (*s == b) {
        cont++;
      }
    }
  }
  return NULL;  /* string ends out of balance */
}

static string max_expand(MatchState *ms, string s, string p,
                              string ep) {
  ptrdiff_t i = 0;  /* counts maximum expand for item */
  while (singlematch(ms, s + i, p, ep)) i++;
  /* keeps trying to match with the maximum repetitions */
  while (i>=0) {
    string res = match(ms, (s+i), ep+1);
    if (res) return res;
    i--;  /* else didn't match; reduce 1 repetition to try again */
  }
  return NULL;
}

static string min_expand(MatchState *ms, string s, string p,
                              string ep) {
  for (;;) {
    string res = match(ms, s, ep+1);
    if (res) return res;
    else if (singlematch(ms, s, p, ep)) s++;  /* try with one more repetition */
    else return NULL;
  }
}


static string start_capture(MatchState *ms, string s, string p,
                                 int what) {
  string res;
  int level = ms->level;
  if (level >= CARP_MAXCAPTURES) carp_regerror("too many captures");
  ms->capture[level].init = s;
  ms->capture[level].len = what;
  ms->level = level+1;
  if (!(res=match(ms, s, p))) ms->level--;  /* undo capture on failed match */
  return res;
}


static string end_capture(MatchState *ms, string s, string p) {
  int l = capture_to_close(ms);
  string res;
  ms->capture[l].len = s - ms->capture[l].init;  /* close capture */
  if (!(res = match(ms, s, p))) ms->capture[l].len = CAP_UNFINISHED; /* undo capture */
  return res;
}


static string match_capture (MatchState *ms, string s, int l) {
  size_t len;
  l = check_capture(ms, l);
  len = ms->capture[l].len;
  if ((size_t)(ms->src_end-s) >= len && !memcmp(ms->capture[l].init, s, len)) {
    return s+len;
  }
  return NULL;
}


static string match(MatchState *ms, string s, string p) {
  if (ms->matchdepth-- == 0) carp_regerror("pattern too complex");
  init: /* using goto's to optimize tail recursion */
  if (p != ms->p_end) {  /* end of pattern? */
    switch (*p) {
      case '(': {  /* start capture */
        if (*(p + 1) == ')') s = start_capture(ms, s, p + 2, CAP_NONE);
        else s = start_capture(ms, s, p + 1, CAP_UNFINISHED);
        break;
      }
      case ')': {  /* end capture */
        s = end_capture(ms, s, p + 1);
        break;
      }
      case '$': {
        if ((p + 1) != ms->p_end) goto dflt;  /* are we done as we should */
        s = (s == ms->src_end) ? s : NULL;  /* check end of string */
        break;
      }
      case C_ESC: {  /* escaped sequences not in the format class[*+?-]? */
        switch (*(p + 1)) {
          case 'b': {  /* balanced string? */
            s = matchbalance(ms, s, p + 2);
            if (s) {
              p += 4; goto init;  /* return match(ms, s, p + 4); */
            }  /* else fail (s == NULL) */
            break;
          }
          case 'f': {  /* frontier? */
            string ep; char previous;
            p += 2;
            if (*p != '[') carp_regerror("missing '[' after '%cf' in pattern", C_ESC);
            ep = classend(ms, p);  /* points to what is next */
            previous = (s == ms->src_init) ? '\0' : *(s - 1);
            if (!matchbracketclass(uchar(previous), p, ep - 1) &&
               matchbracketclass(uchar(*s), p, ep - 1)) {
              p = ep; goto init;  /* return match(ms, s, ep); */
            }
            s = NULL;  /* match failed */
            break;
          }
          case '0': case '1': case '2': case '3':
          case '4': case '5': case '6': case '7':
          case '8': case '9': {  /* capture results (\0-\9)? */
            s = match_capture(ms, s, uchar(*(p + 1)));
            if (s) {
              p += 2; goto init;  /* return match(ms, s, p + 2) */
            }
            break;
          }
          default: goto dflt;
        }
        break;
      }
      default: dflt: {  /* pattern class plus optional suffix */
        string ep = classend(ms, p);  /* points to optional suffix */
        /* does not match at least once? */
        if (!singlematch(ms, s, p, ep)) {
          if (*ep == '*' || *ep == '?' || *ep == '-') {  /* accept empty? */
            p = ep + 1; goto init;  /* return match(ms, s, ep + 1); */
          }
          else { /* '+' or no suffix */
            s = NULL;  /* fail */
          }
        }
        else {  /* matched once */
          switch (*ep) {  /* handle optional suffix */
            case '?': {  /* optional */
              string res;
              if ((res = match(ms, s + 1, ep + 1))) {
                s = res;
              } else {
                p = ep + 1; goto init;  /* else return match(ms, s, ep + 1); */
              }
              break;
            }
            case '+':  /* 1 or more repetitions */
              s++;  /* 1 match already done */
              /* FALLTHROUGH */
            case '*':  /* 0 or more repetitions */
              s = max_expand(ms, s, p, ep);
              break;
            case '-':  /* 0 or more repetitions (minimum) */
              s = min_expand(ms, s, p, ep);
              break;
            default:  /* no suffix */
              s++; p = ep; goto init;  /* return match(ms, s + 1, ep); */
          }
        }
        break;
      }
    }
  }
  ms->matchdepth++;
  return s;
}

static string lmemfind(string s1, size_t l1, string s2,
                            size_t l2) {
  if (l2 == 0) return s1;  /* empty strings are everywhere */
  if (l2 > l1) return NULL;  /* avoids a negative 'l1' */
  string init;  /* to search for a '*s2' inside 's1' */
  l2--;  /* 1st char will be checked by 'memchr' */
  l1 = l1-l2;  /* 's2' cannot be found after that */
  while (l1 > 0 && (init = (string )memchr(s1, *s2, l1))) {
    init++;   /* 1st char is already checked */
    if (!memcmp(init, s2+1, l2)) {
      return init-1;
    } else {  /* correct 'l1' and 's1' to try again */
      l1 -= init-s1;
      s1 = init;
    }
  }
  return NULL;  /* not found */
}

string String_copy_len(string s, int len) {
    string ptr = CARP_MALLOC(len);

    if (ptr == NULL) {
      return NULL;
    }

    return (string) memcpy(ptr, s, len);
}

Array push_string(Array a, string s, int i, int len) {
  ((string*)a.data)[i] = String_copy_len(s, len);
  return a;
}

Array push_onecapture(MatchState *ms, int i, string s, string e,
                      Array captures) {
  if (i >= ms->level) {
    if (!i) return push_string(captures, s, i, ms->capture[i].len);  /* add whole match */
    else carp_regerror("invalid capture index %cd", C_ESC, i + 1);
  }
  else {
    ptrdiff_t l = ms->capture[i].len;
    if (l == CAP_UNFINISHED) carp_regerror("unfinished capture");
    else if (l != CAP_NONE) return push_string(captures, ms->capture[i].init, i, ms->capture[i].len);
  }
  return captures;
}

Array push_captures(MatchState *ms, string s, string e) {
  int i;
  int nlevels = (ms->level == 0 && s) ? 1 : ms->level;
  Array res;
  res.len = nlevels;
  res.data = CARP_MALLOC(nlevels*sizeof(string));
  for (i = 0; i < nlevels; i++) push_onecapture(ms, i, s, e, res);
  return res;
}

/* check whether pattern has no special characters */
static int nospecials(string p, size_t l) {
  size_t upto = 0;
  do {
    if (strpbrk(p + upto, SPECIALS)) return 0; /* pattern has a special character */
    upto += strlen(p + upto) + 1; /* may have more after \0 */
  } while (upto <= l);
  return 1; /* no special chars found */
}

static void prepstate(MatchState *ms, string s, size_t ls, string p,
                      size_t lp) {
  ms->matchdepth = MAXCCALLS;
  ms->src_init = s;
  ms->src_end = s + ls;
  ms->p_end = p + lp;
}

static void reprepstate(MatchState *ms) {
  ms->level = 0;
  assert(ms->matchdepth == MAXCCALLS);
}

static int String_find(string str, string pat) {
  int lstr = strlen(str);
  int lpat = strlen(pat);
  /* explicit request or no special characters? */
  if (nospecials(pat, lpat)) {
    /* do a plain search */
    string s2 = lmemfind(str, lstr, pat, lpat);
    return s2-str;
  }
  MatchState ms;
  string s1 = str;
  int anchor = (*pat == '^');
  if (anchor) {
    pat++; lpat--;  /* skip anchor character */
  }
  prepstate(&ms, str, lstr, pat, lpat);
  do {
    string res;
    reprepstate(&ms);
    if ((res=match(&ms, s1, pat))) return s1 - str + 1;
  } while (s1++ < ms.src_end && !anchor);
  return -1;
}


Array String_match(string str, string pat) {
  int lstr = strlen(str);
  int lpat = strlen(pat);
  MatchState ms;
  string s1 = str;
  int anchor = (*pat == '^');
  if (anchor) {
    pat++; lpat--;  /* skip anchor character */
  }
  prepstate(&ms, str, lstr, pat, lpat);
  do {
    string res;
    reprepstate(&ms);
    if ((res=match(&ms, s1, pat))) return push_captures(&ms, s1, res);
  } while (s1++ < ms.src_end && !anchor);
  Array a;
  a.len = 0;
  a.data = NULL;
  return a;
}
