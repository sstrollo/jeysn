/*

  The JavaScript Object Notation (JSON) Data Interchange Format

  https://tools.ietf.org/html/rfc7159


  JSON Encoding of Data Modeled with YANG

  https://tools.ietf.org/html/draft-ietf-netmod-yang-json-02

 */

#include <stdio.h>
#include <string.h>
#include <stdint.h>

#include <stdlib.h>
//#include <errno.h>

#include "json.h"

#define json_state_at_eob(JSP) (((JSP)->buf.buf == NULL) || \
                                ((JSP)->buf.ptr == \
                                 ((JSP)->buf.buf + (JSP)->buf.size)))
#define json_state_at_eof(JSP) ((JSP)->eof && json_state_at_eob(JSP))


static void update_position(json_state_t *jsp, uchar *newpos)
{
    size_t count = newpos - jsp->buf.ptr;
    jsp->pos.pos += count;
    jsp->pos.col += count;
    jsp->buf.ptr = newpos;
}

static void nl(json_state_t *jsp)
{
    jsp->pos.pos += 1;
    jsp->pos.line += 1;
    jsp->pos.col = 0;
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

#define is_hex_char(C) ((((C) >= '0') && ((C) <= '9')) || \
                        (((C) >= 'a') && ((C) <= 'f')) || \
                        (((C) >= 'A') && ((C) <= 'F')))

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
                if ((p+5) >= stop) {
                    /* XXX not if there is an end-quote within those five */
                    return json_result_error_on_eof(jsp);
                }
                if (is_hex_char(*(p+1)) && is_hex_char(*(p+2)) &&
                    is_hex_char(*(p+3)) && is_hex_char(*(p+4)))
                {
                    need_escape = 1;
                    if ((*(p+1) == '0') && (*(p+2) == '0')) {
                        sz += 1;
                    } else {
                        sz += 2;
                    }
                    p += 5;
                } else {
                    sz++;
                }
                break;
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
                    if (is_hex_char(*(src+1)) && is_hex_char(*(src+2)) &&
                        is_hex_char(*(src+3)) && is_hex_char(*(src+4)))
                    {
                        src++;
                        *dst = get_hex_byte(src);
                        if (*dst)
                            dst++;
                        src+=2;
                        *dst++ = get_hex_byte(src);
                        src+=2;
                    } else {
                        *dst++ = json_char_reverse_solidus;
                    }
                    break;
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
            uint64_t tmp = 10 * un;
            if (tmp < un)
                goto rescan;
            un = tmp + (*p - '0');
            p++;
        } else {
            if (*p == '.') {
                goto rescan;
            } else {
                break;
            }
        }
    }
    if (is_neg) {
        in = un;
        if (in != un) {
            goto rescan;
        }
        in = 0 - in;
        jtok->type = json_token_number;
        jtok->value.number = in;
    } else {
        jtok->type = json_token_number_unsigned;
        jtok->value.number_unsigned = un;
    }
    update_position(jsp, p);
    return json_result_token;

rescan:
    /* Continue scanning until end of number. If it seems like a
     * floating point try strtod() otherwise return it as string.
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
                        p++;    /* XXX close enough... */
                    } else {
                        break;
                    }
                } else {
                    if (*p == '.') {
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
            memcpy(tmp, start, len-1);
            tmp[len-1] = 0;
            d = strtod(tmp, &tmp_end);
//            if (tmp_end == (tmp + (len - 1)) && (errno != ERANGE)) {
                jtok->type = json_token_number_double;
                jtok->value.number_double = d;
                update_position(jsp, p);
                return json_result_token;
//            }
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
            update_position(jsp, p);
            /* FIXME set error token */
            return json_result_error;
        }
    }
need_more:
    return json_result_error_on_eof(jsp);
}
