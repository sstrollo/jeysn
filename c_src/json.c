/*
 * Copyright 2016 Sebastian Strollo
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
 * A small implementation of a JSON scanner

  The JavaScript Object Notation (JSON) Data Interchange Format

  https://tools.ietf.org/html/rfc7159

 */

#include <stdio.h>
#include <string.h>
#include <stdint.h>

#include <stdlib.h>
#include <errno.h>

#include "json.h"

#define json_state_at_eob(JSP) (((JSP)->buf.buf == NULL) || \
                                ((JSP)->buf.ptr == \
                                 ((JSP)->buf.buf + (JSP)->buf.size)))
#define json_state_at_eof(JSP) ((JSP)->eof && json_state_at_eob(JSP))


static void update_position(json_state_t *jsp, uchar *newpos)
{
    size_t count = newpos - jsp->buf.ptr;
    jsp->position.offset += count;
    jsp->position.column += count;
    jsp->buf.ptr     = newpos;
}

static void nl(json_state_t *jsp)
{
    jsp->position.offset += 1;
    jsp->position.line   += 1;
    jsp->position.column  = 0;
}

static json_result_t json_result_error_on_eof(json_state_t *jsp)
{
    if (jsp->eof) {
        jsp->token.type = json_token_error;
        jsp->token.value.error.code = json_error_eof;
        jsp->token.value.error.string = "premature end while scanning token";
        return json_result_error;
    } else {
        return json_result_more;
    }
}

void json_state_init(json_state_t *jsp, void *(*alloc)(size_t size),
                     void (*free)(void *ptr))
{
    memset(jsp, 0, sizeof(*jsp));
    jsp->alloc = alloc;
    jsp->free = free;
    jsp->position.line = 1;
    return;
}

void json_state_destroy(json_state_t *jsp)
{
    if (jsp->buf.buf) {
        jsp->free((void *)jsp->buf.buf);
    }
    memset(jsp, 0, sizeof(*jsp));
    return;
}

void json_state_set_eof(json_state_t *jsp)
{
    jsp->eof = 1;
    return;
}

void json_state_add_buffer(json_state_t *jsp, void *p, size_t sz)
{
    if (jsp->buf.buf == NULL) {
        jsp->buf.buf = jsp->buf.ptr = jsp->alloc(sz);
        jsp->buf.size = sz;
        jsp->buf.stop = jsp->buf.buf + sz;
        memcpy(jsp->buf.buf, p, sz);
        return;
    }
    size_t keep = jsp->buf.stop - jsp->buf.ptr;
    if ((keep + sz) <= jsp->buf.size)
    {
        /* re-use current buffer */
#if 0
        fprintf(stderr,
                "re-using buf: currsz=%ld keep=%ld need=%ld ptrpos=%ld\n",
                jsp->buf.size, keep, sz, jsp->buf.ptr - jsp->buf.buf);
#endif
        memmove(jsp->buf.buf, jsp->buf.ptr, keep);
        jsp->buf.ptr = jsp->buf.buf;
        memcpy(jsp->buf.buf + keep, p, sz);
        jsp->buf.stop = jsp->buf.buf + keep + sz;
    } else {
        uchar *tmp = jsp->buf.buf;
        jsp->buf.size = keep + sz;
        jsp->buf.buf = jsp->alloc(jsp->buf.size); /* realloc() */
        if (keep > 0) memcpy(jsp->buf.buf, jsp->buf.ptr, keep);
        jsp->buf.ptr = jsp->buf.buf;
        memcpy(jsp->buf.buf + keep, p, sz);
        jsp->buf.stop = jsp->buf.buf + jsp->buf.size;
        jsp->free(tmp);
    }
#if 0
    fprintf(stderr, "buf: keep=%ld sz=%ld size=%ld stop=%ld |%.*s|\n",
            keep, sz, jsp->buf.size, (jsp->buf.stop - jsp->buf.buf),
            (int)(jsp->buf.stop - jsp->buf.buf), jsp->buf.buf);
#endif
    return;
}

/* ------------------------------------------------------------------------

  7.  Strings

      char = unescaped /
          escape (
              %x22 /          ; "    quotation mark  U+0022
              %x5C /          ; \    reverse solidus U+005C
              %x2F /          ; /    solidus         U+002F
              %x62 /          ; b    backspace       U+0008
              %x66 /          ; f    form feed       U+000C
              %x6E /          ; n    line feed       U+000A
              %x72 /          ; r    carriage return U+000D
              %x74 /          ; t    tab             U+0009
              %x75 4HEXDIG )  ; uXXXX                U+XXXX

      escape = %x5C              ; \

      quotation-mark = %x22      ; "

      unescaped = %x20-21 / %x23-5B / %x5D-10FFFF

* ------------------------------------------------------------------------ */

#define json_char_begin_array     (0x5b)  /* [ */
#define json_char_end_array       (0x5d)  /* ] */
#define json_char_begin_object    (0x7b)  /* { */
#define json_char_end_object      (0x7d)  /* } */
#define json_char_name_separator  (0x3a)  /* : */
#define json_char_value_separator (0x2c)  /* , */

#define json_char_quotation_mark  (0x22)
#define json_char_escape          (0x5c)
#define json_char_solidus         (0x2f)
#define json_char_reverse_solidus (0x5c)

#if 0
static inline uchar get_hex_byte(uchar *s)
{
    uchar *ptr = s;
    uchar res = 0;
#define HEXCHAR(C) ( \
        (((C) >= '0') && ((C) <= '9')) ? (C) - '0' :        \
        (((C) >= 'a') && ((C) <= 'f')) ? ((C) - 'a') + 10 :     \
        (((C) >= 'A') && ((C) <= 'F')) ? ((C) - 'A') + 10 : 0)
    res = HEXCHAR(*ptr);
    ptr++;
    res *= 16;
    res += HEXCHAR(*ptr);
    return res;
#undef HEXCHAR
}
#endif

static inline uint32_t get_hex_2byte(uchar *s)
{
    uchar *ptr = s;
    uint32_t res = 0;
#define HEXCHAR(C) ( \
        (((C) >= '0') && ((C) <= '9')) ? (C) - '0' :        \
        (((C) >= 'a') && ((C) <= 'f')) ? ((C) - 'a') + 10 :     \
        (((C) >= 'A') && ((C) <= 'F')) ? ((C) - 'A') + 10 : 0)
    res = HEXCHAR(*ptr);  ptr++;
    res *= 16;
    res += HEXCHAR(*ptr); ptr++;
    res *= 16;
    res += HEXCHAR(*ptr); ptr++;
    res *= 16;
    res += HEXCHAR(*ptr);
    return res;
#undef HEXCHAR
}

#define is_hex_char(C) ((((C) >= '0') && ((C) <= '9')) || \
                        (((C) >= 'a') && ((C) <= 'f')) || \
                        (((C) >= 'A') && ((C) <= 'F')))


/*
   Any character may be escaped.  If the character is in the Basic
   Multilingual Plane (U+0000 through U+FFFF), then it may be
   represented as a six-character sequence: a reverse solidus, followed
   by the lowercase letter u, followed by four hexadecimal digits that
   encode the character's code point.  The hexadecimal letters A though
   F can be upper or lower case.  So, for example, a string containing
   only a single reverse solidus character may be represented as
   "\u005C".

   Alternatively, there are two-character sequence escape
   representations of some popular characters.  So, for example, a
   string containing only a single reverse solidus character may be
   represented more compactly as "\\".

   To escape an extended character that is not in the Basic Multilingual
   Plane, the character is represented as a 12-character sequence,
   encoding the UTF-16 surrogate pair.  So, for example, a string
   containing only the G clef character (U+1D11E) may be represented as
   "\uD834\uDD1E".
*/

#define utf16_high_surrogate_start (0xD800)
#define utf16_low_surrogate_start  (0xDC00)
#define is_utf16_high_surrogate_range(N) (((N) >= 0xD800) && ((N) <= 0xDBFF))
#define is_utf16_low_surrogate_range(N)  (((N) >= 0xDC00) && ((N) <= 0xDFFF))
#define is_unicode_reserved_range(N)     (((N) >= 0xD800) && ((N) <= 0xDFFF))

/* pointing to 'u' followed by  */
/* return 0 : need more, -1 invalid,  */
static inline int json_utf8_escaped_width(uchar *p, uchar *stop, uint32_t *cp)
{
    if ((p+5) >= stop) {
        return 0;
    }
    if (is_hex_char(*(p+1)) && is_hex_char(*(p+2)) &&
        is_hex_char(*(p+3)) && is_hex_char(*(p+4)))
    {
        uint32_t i = get_hex_2byte(p+1);
        if (is_utf16_high_surrogate_range(i)) {
            if ((p+11) >= stop ) {
                return 0;
            }
            if ((*(p+5) == json_char_reverse_solidus) && (*(p+6) == 'u') &&
                is_hex_char(*(p+7)) && is_hex_char(*(p+8)) &&
                is_hex_char(*(p+9)) && is_hex_char(*(p+10)))
            {
                /* CP 0x10000..0x10FFFF encoded in utf-16 surrogate pair */
                uint32_t low = get_hex_2byte(p+7);
                if (is_utf16_low_surrogate_range(low)) {
                    if (cp) {
                        i   -= utf16_high_surrogate_start;
                        low -= utf16_low_surrogate_start;
                        i = 0x010000 + (i << 10) + low;
                        *cp = i;
                    }
                    return 4;
                }
            }
        }
        if (is_unicode_reserved_range(i)) {
            return -1;
        }
        if (i > 0xffff)  return -1;
        /* CodePoint 0..0xD7FF , 0xE000..0xFFFF */
        if (cp) { *cp = i; }
        if (i <= 0x7F)   return 1;
        if (i <= 0x7FF)  return 2;
        if (i <= 0xFFFF) return 3;
    }
    return -1;
}

/* srcp is looking at 'u' */
static inline int json_utf8_escaped_expand(uchar **srcp, uchar **dstp,
                                           uchar *stop)
{
    uchar *src = *srcp;
    uchar *dst = *dstp;
    uint32_t cp;
    int slen = 5;
    int dlen = json_utf8_escaped_width(src, stop, &cp);
    switch (dlen) {
    case 4:
        *(dst+3) = (uchar)((cp & 0x3f) | 0x80); cp >>= 6;
        *(dst+2) = (uchar)((cp & 0x3f) | 0x80); cp >>= 6;
        *(dst+1) = (uchar)((cp & 0x3f) | 0x80); cp >>= 6;
        *(dst+0) = (uchar)((cp & 0x07) | 0xf0);
        slen = 11;
        break;
    case 3:
        *(dst+2) = (uchar)((cp & 0x3f) | 0x80); cp >>= 6;
        *(dst+1) = (uchar)((cp & 0x3f) | 0x80); cp >>= 6;
        *(dst+0) = (uchar)((cp & 0x0f) | 0xe0);
        break;
    case 2:
        *(dst+1) = (uchar)((cp & 0x3f) | 0x80); cp >>= 6;
        *(dst+0) = (uchar)((cp & 0x1f) | 0xc0);
        break;
    case 1:
        *(dst+0) = (uchar)cp;
        break;
    default:
        /* invalid input ignored */
        return 0;
    }
    /* update pointers */
    *srcp = src + slen;
    *dstp = dst + dlen;
    return 1;
}

/* Assumption: ptr is pointing to first quote of string */
static json_result_t json_string(json_state_t *jsp)
{
    json_token_t *jtok = &jsp->token;
    int need_escape = 0;
    size_t sz = 0;
    uchar *start, *p = jsp->buf.ptr;
    uchar *stop = jsp->buf.stop;
    if (++p == stop) { return json_result_error_on_eof(jsp); }
    start = p;

    /* Now p is looking at the first character of the string, first
     * scan to find end and figure out if we need to quote
     */
    for (;*p != json_char_quotation_mark;) {
//        printf("parse_string: <%p> %c |%.*s|\n", p, *p, (int)(stop - p), p);
        if (*p == json_char_escape) {
            if ((p+1) == stop) { return json_result_error_on_eof(jsp); }
            p++;
            switch (*p) {
            case json_char_quotation_mark:
            case json_char_reverse_solidus:
            case json_char_solidus:
            case 'b':
            case 'f':
            case 'n':
            case 'r':
            case 't':
                need_escape = 1;
                p++;
                sz++;
                break;
            case 'u':
            {
                int dlen = json_utf8_escaped_width(p, stop, NULL);
                if ((dlen == 0) &&
                    (memchr(p, json_char_quotation_mark, stop-p) == NULL))
                {
                    return json_result_error_on_eof(jsp);
                }
                if (dlen > 0) {
                    need_escape = 1;
                    sz += dlen;
                    p += (dlen == 4) ? 11 : 5;
                } else {
                    sz++;  /* ignore (copy) invalid escape sequence */
                }
                break;
            }
            default:
                sz++;
            }
        } else {
            p++;
            sz++;
        }
        if (p == stop) {
            return json_result_error_on_eof(jsp);
        }
    }
//    printf("need_escape=%d %d %d |%.*s|\n",
//           need_escape, (int)(p-start), (int)sz, (int)(p-start), start);
    if (!need_escape) {
        jtok->type = json_token_string;
        jtok->value.string.string = start;
        jtok->value.string.size = p - start;
        jtok->value.string.need_free = 0;
        update_position(jsp, p + 1);
        return json_result_token;
    } else {
        uchar *src = start, *dst, *tmp = jsp->alloc(sz);
        for (dst = tmp; src < p;) {
            if (*src == json_char_reverse_solidus) {
                src++;
                switch (*src) {
                case json_char_quotation_mark:
                case json_char_reverse_solidus:
                case json_char_solidus:
                    *dst++ = *src++;
                    break;
                case 'b': { *dst++ = 0x08; src++; break; }
                case 'f': { *dst++ = 0x0c; src++; break; }
                case 'n': { *dst++ = 0x0a; src++; break; }
                case 'r': { *dst++ = 0x0d; src++; break; }
                case 't': { *dst++ = 0x09; src++; break; }
                case 'u':
                    if (json_utf8_escaped_expand(&src, &dst, stop)) {
                        break;
                    }
                    /* fall-through */
                default:
                    *dst++ = json_char_reverse_solidus;
                }
            } else {
                *dst++ = *src++;
            }
        }
        jtok->type = json_token_string;
        jtok->value.string.string = tmp;
        jtok->value.string.size = sz;
        jtok->value.string.need_free = 1;
        update_position(jsp, p + 1);
        return json_result_token;
    }
}

/* ------------------------------------------------------------------------

    6.  Numbers

      number = [ minus ] int [ frac ] [ exp ]

      decimal-point = %x2E       ; .

      digit1-9 = %x31-39         ; 1-9

      e = %x65 / %x45            ; e E

      exp = e [ minus / plus ] 1*DIGIT

      frac = decimal-point 1*DIGIT

      int = zero / ( digit1-9 *DIGIT )

      minus = %x2D               ; -

      plus = %x2B                ; +

      zero = %x30                ; 0

* ------------------------------------------------------------------------ */

/* Assumption: ptr is pointing to start of number  */
static json_result_t json_number(json_state_t *jsp)
{
    json_token_t *jtok = &jsp->token;
    int is_neg = 0;
    uint64_t un;
    int64_t  in;
    uchar *start, *p = jsp->buf.ptr;
    uchar *stop = jsp->buf.stop;
    start = p;
    if (*p == '-') {
        is_neg=1;
        p++;
        if (p == stop) {
            return json_result_error_on_eof(jsp);
        }
    }
    for (un = 0;;) {
        if (p == stop){if (jsp->eof) break; else return json_result_more;}
        if ((*p >= '0') && (*p <= '9')) {
            uint64_t tmp = (*p - '0');
            if (un > (UINT64_MAX / 10))
                goto not_an_integer;
            un *= 10;
            if ((UINT64_MAX - un) < tmp)
                goto not_an_integer;
            un += tmp;
            p++;
        } else {
            if ((*p == '.') || (*p == 'e') || (*p == 'E')) {
                goto not_an_integer;
            } else {
                break;
            }
        }
    }
    if (is_neg) {
        if (un > (INT64_MAX - 1))
            goto not_an_integer;
        in = un;
        in = 0 - in;
        jtok->type = json_token_number;
        jtok->value.number = in;
    } else {
        jtok->type = json_token_number_unsigned;
        jtok->value.number_unsigned = un;
    }
    update_position(jsp, p);
    return json_result_token;

not_an_integer:
    /* The number won't fit as an integer, it is either too big or a
     * floating point number, so continue scanning until end of
     * number. If it seems like a floating point try strtod()
     * otherwise return it as string.
     */
    {
        int is_float = 0;
        for (;;) {
            if (p == stop){if (jsp->eof) break; else return json_result_more;}
            if ((*p >= '0') && (*p <= '9')) {
                p++;
            } else {
                if (is_float) {
                    if ((*p == 'e') || (*p == 'E') ||
                        (*p == '-') || (*p == '+')) {
                        /* Close enough, errors will be caught by strtod() */
                        p++;
                    } else {
                        break;
                    }
                } else {
                    if ((*p == '.') || (*p == 'e') || (*p == 'E')) {
                        is_float = 1;
                        p++;
                    } else {
                        break;
                    }
                }
            }
        }
        if (is_float) {
            double d = 0.0;
            size_t len = (p - start) + 1;
            char tmp[len];
            char *tmp_end;
            int err = 0;
            memcpy(tmp, start, len-1);
            tmp[len-1] = 0;
            d = strtod(tmp, &tmp_end);
            err = errno;
            if (tmp_end == (tmp + (len - 1)) && (err != ERANGE)) {
                jtok->type = json_token_number_double;
                jtok->value.number_double = d;
                update_position(jsp, p);
                return json_result_token;
            } else {
                jtok->type = json_token_error;
                jtok->value.error.code = json_error_invalid_number;
                jtok->value.error.string = "invalid floating point number";
                return json_result_error;
            }
        }
        jtok->type = json_token_number_string;
        jtok->value.string.string = start;
        jtok->value.string.size = p - start;
        jtok->value.string.need_free = 0;
        update_position(jsp, p);
        return json_result_token;
    }
}

/* ------------------------------------------------------------------------ */

#define is_json_ws(C) (((C) == 0x20) || ((C) == 0x09) || \
                       ((C) == 0x0a) || ((C) == 0x0d))

/* Forward the pointers in jsp past any whitespace */
void json_skip_ws(json_state_t *jsp)
{
    uchar *p = jsp->buf.ptr;
    uchar *stop = jsp->buf.stop;

    for (p=jsp->buf.ptr; p && (p < stop) && is_json_ws(*p); p++) {
        if (*p == 0x0a) nl(jsp);
    }
    if (p) update_position(jsp, p);
    return;
}

json_result_t json_next_token(json_state_t *jsp)
{
    uchar *p = jsp->buf.ptr;
    uchar *stop = jsp->buf.stop;
    json_token_t *jtok = &jsp->token;

    for (p=jsp->buf.ptr; p && (p < stop) && is_json_ws(*p); p++) {
        if (*p == 0x0a) nl(jsp);
    }
    if (p == NULL || p == stop) {
        if (p) update_position(jsp, p);
        if (jsp->eof) {
            return json_result_eof;
        } else {
            return json_result_more;
        }
    }
    update_position(jsp, p);
    jtok->position = jsp->position;
    switch (*p) {
    case json_char_begin_array:
        jtok->type = json_token_begin_array;
        update_position(jsp, ++p);
        return json_result_token;
        break;
    case json_char_end_array:
        jtok->type = json_token_end_array;
        update_position(jsp, ++p);
        return json_result_token;
        break;
    case json_char_begin_object:
        jtok->type = json_token_begin_object;
        update_position(jsp, ++p);
        return json_result_token;
        break;
    case json_char_end_object:
        jtok->type = json_token_end_object;
        update_position(jsp, ++p);
        return json_result_token;
        break;
    case json_char_name_separator:
        jtok->type = json_token_name_separator;
        update_position(jsp, ++p);
        return json_result_token;
        break;
    case json_char_value_separator:
        jtok->type = json_token_value_separator;
        update_position(jsp, ++p);
        return json_result_token;
        break;
    case 'f':
        if ((p+4) >= stop) {
            update_position(jsp, p);
            goto need_more;
        }
        if ((*(p+1) == 'a') && (*(p+2) == 'l') &&
            (*(p+3) == 's') && (*(p+4) == 'e')) {
            jtok->type = json_token_false;
            update_position(jsp, p+5);
            return json_result_token;
        } else {
            goto invalid_char;
        }
        break;
    case 'n':
        if ((p+3) >= stop) {
            update_position(jsp, p);
            goto need_more;
        }
        if ((*(p+1) == 'u') && (*(p+2) == 'l') && (*(p+3) == 'l')) {
            jtok->type = json_token_null;
            update_position(jsp, p+4);
            return json_result_token;
        } else {
            goto invalid_char;
        }
        break;
    case 't':
        if ((p+3) >= stop) {
            update_position(jsp, p);
            goto need_more;
        }
        if ((*(p+1) == 'r') && (*(p+2) == 'u') && (*(p+3) == 'e')) {
            jtok->type = json_token_true;
            update_position(jsp, p+4);
            return json_result_token;
        } else {
            goto invalid_char;
        }
        break;
    case json_char_quotation_mark:
        update_position(jsp, p);
        return json_string(jsp);
        break;
    default:
        if (((*p >= '0') && (*p <= '9')) || (*p == '-')) {
            update_position(jsp, p);
            return json_number(jsp);
        } else {
            goto invalid_char;
        }
    }
need_more:
    return json_result_error_on_eof(jsp);
invalid_char:
    update_position(jsp, p);
    jtok->type = json_token_error;
    jtok->value.error.code = json_error_invalid_char;
    jtok->value.error.string = "invalid character";
    return json_result_error;
}


#define json_must_escape_char(C) ( ((C) <= 0x1f) || \
                                   ((C) == json_char_quotation_mark) || \
                                   ((C) == json_char_reverse_solidus) )

int json_string_escape_size(uchar *start, size_t size, size_t *newsize)
{
    int need_escape = 0;
    uchar *p, *stop = start + size;

    for (p = start; p < stop; p++) {
        if (json_must_escape_char(*p)) {
            need_escape = 1;
            break;
        }
    }
    if (need_escape) {
        size_t sz = p - start;
        for (; p < stop; sz++,p++) {
            switch (*p) {
            case 0x22:
            case 0x5C:
            case 0x08:
            case 0x0C:
            case 0x0A:
            case 0x0D:
            case 0x09:
                sz++;
                break;
            default:
                if (json_must_escape_char(*p)) {
                    sz += 5;
                }
            }
        }
        *newsize = sz;
        return 1;
    } else {
        *newsize = size;
        return 0;
    }
}

static inline uchar hex_char(int c)
{
    c &= 0xf;
    if (c < 10) { return '0' + c; }
    return 'A' + (c - 10);
}

int json_string_escape(uchar *src, size_t size, uchar *dst, size_t dst_size)
{
#if 0
    uchar *dbg_src = src, *dbg_dst = dst;
#endif
    uchar *stop = src + size;
    for (; src<stop; src++,dst++) {
        switch (*src) {
        case 0x22: *dst++ = json_char_reverse_solidus; *dst = *src; break;
        case 0x5C: *dst++ = json_char_reverse_solidus; *dst = *src; break;
        case 0x08: *dst++ = json_char_reverse_solidus; *dst = 'b'; break;
        case 0x0C: *dst++ = json_char_reverse_solidus; *dst = 'f'; break;
        case 0x0A: *dst++ = json_char_reverse_solidus; *dst = 'n'; break;
        case 0x0D: *dst++ = json_char_reverse_solidus; *dst = 'r'; break;
        case 0x09: *dst++ = json_char_reverse_solidus; *dst = 't'; break;
        default:
            if (json_must_escape_char(*src)) {
                int c = (int)*src;
                *dst++ = json_char_reverse_solidus;
                *dst++ = 'u';
                *dst++ = '0';
                *dst++ = '0';
                *dst++ = hex_char(c >> 4);
                *dst = hex_char(c);
            } else {
                *dst = *src;
            }
        }
    }
#if 0
    {
        char dbg_src_str[size+1];
        char dbg_dst_str[dst_size+1];
        int i; uchar *p;
        for (i=0,p=dbg_src; i<size; i++,p++) {
            if (isprint(*p)) { dbg_src_str[i] = *p; }
            else { dbg_src_str[i] = '.'; }
        }
        dbg_src_str[i] = 0;
        for (i=0,p=dbg_dst; i<dst_size; i++,p++) {
            if (isprint(*p)) { dbg_dst_str[i] = *p; }
            else { dbg_dst_str[i] = '.'; }
        }
        dbg_dst_str[i] = 0;
        fprintf(stderr, "src|%s|%d(%d) dst|%s|%d(%d)\n\r",
                dbg_src_str, (int)size, (int)(src-dbg_src-size),
                dbg_dst_str, (int)dst_size, (int)(dst-dbg_dst-dst_size));
    }
#endif
    return 1;
}
