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


#define is_json_ws(C) (((C) == 0x20) || ((C) == 0x09) || \
                       ((C) == 0x0a) || ((C) == 0x0d))

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




/*
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
*/

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

/* return:
 *  -1 - start of something other than string found
 *   0 - more bytes (only ws or non-terminated string)
 *   1 - pointer to string of size 'size' (in your buffer)
 *   2 - pointer to translated string of size 'size' (you need to free())
 *
 * str size new_pos
 */
int json_string(uchar *buf, uchar *stop,
                uchar **str, size_t *size, uchar **np)
{
    int need_escape = 0;
    size_t sz = 0;
    uchar *p, *start = buf;
//    printf("parse_string: <%p> <%p> %d |%.*s|\n",
//           buf, stop, (int)(stop - buf), (int)(stop - buf), buf);
    for (p=start; (p < stop) && is_json_ws(*p); p++) ;
    if (p == stop) { *np = p; return 0; }
    if (*p != json_char_quotation_mark) {
        *np = p;
        return -1;
    }
    start = p;
    if (++p == stop) { *np = start; return 0; }
    /* Now p is looking at the first character of the string, first
     * scan to find end and figure out if we need to quote
     */
    for (;*p != json_char_quotation_mark;) {
//        printf("parse_string: <%p> %c |%.*s|\n", p, *p, (int)(stop - p), p);
        if (*p == json_char_escape) {
            if ((p+1) == stop) { *np = start; return 0; }
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
                /* XXX not if there is an end-quote within those five */
                if ((p+5) >= stop) { *np = start; return 0; }
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
        if (p == stop) { *np = start; return 0; }
    }
    start++;
//    printf("need_escape=%d %d %d |%.*s|\n",
//           need_escape, (int)(p-start), (int)sz, (int)(p-start), start);
    if (!need_escape) {
        *str = start;
        *size = p - start;
        *np = p + 1;
        return 1;
    } else {
        uchar *src = start, *dst, *tmp = malloc(sz);
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
//                        printf("xx1: |%.2s| %d\n", src, (int)get_hex_byte(src));
                        *dst = get_hex_byte(src);
                        if (*dst)
                            dst++;
                        src+=2;
//                        printf("xx2: |%.2s| %d\n", src, (int)get_hex_byte(src));
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
        *str = tmp;
        *size = sz;
        *np = p + 1;
        return 2;
    }
}


/*
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
*/

#if 0
/* Assumption: ptr is pointing to start of number  */
static int json_number(json_state_t *jsp)
{
    int is_neg = 0;
    int has_frac = 0;
    int has_exp = 0;
    size_t len = 1;
    uint64_t un;
    int64_t  in;
    double d;
    uchar *start, *p = jsp->buf.ptr;
    uchar *stop = jsp->buf.stop;
    start = p;
    if (*p == '-') { is_neg=1; p++; }
    /* first scan for frac and exp */
    for (;;) {

    }
}

int json_number(uchar *buf, uchar *stop)
{
    return 42;
}
#endif

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
        switch (json_string(p, stop, &jtok->value.string.string,
                            &jtok->value.string.size, &p))
        {
        case 0:
            update_position(jsp, p);
            goto need_more;
        case 1:
            update_position(jsp, p);
            jtok->value.string.need_free = 0;
            jtok->type = json_token_string;
            return json_result_token;
        case 2:
            update_position(jsp, p);
            jtok->value.string.need_free = 1;
            jtok->type = json_token_string;
            return json_result_token;
        }
        break;
    default:
        if (((*p >= '0') && (*p <= '9')) || (*p == '-')) {
            /* XXX proper number parser */
            jtok->type = json_token_number;
            jtok->value.number = 0;
            for (;;) {
                if (p == stop) {
                    if (jsp->eof) {
                        update_position(jsp, p);
                        return json_result_token;
                    } else {
                        return json_result_more;
                    }
                }
                if (((*p >= '0') && (*p <= '9')) || (*p == '.')) {
                    jtok->value.number = (10 * jtok->value.number) + (*p - '0');
                    p++;
                } else {
                    break;
                }
            }
            update_position(jsp, p);
            return json_result_token;
        } else {
            update_position(jsp, p);
            /* FIXME set error token */
            return json_result_error;
        }
    }
need_more:
    if (jsp->eof) {
        jsp->token.type = json_token_error;
        jsp->token.value.error.code = json_error_eof;
        jsp->token.value.error.string = "premature end while scanning token";
    } else {
        return json_result_more;
    }
    /* NOT REACHED */
    return json_result_error;
}
