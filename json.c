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



int json_token(uchar *buf, uchar *stop, json_token_t *jtok, uchar **np)
{
    uchar *p;
    for (p=buf; (p < stop) && is_json_ws(*p); p++) ;
    if (p == stop) { *np = p; return 0; }
    switch (*p) {
    case json_char_begin_array:
        jtok->type = json_token_begin_array;
        *np = ++p;
        return 1;
        break;
    case json_char_end_array:
        jtok->type = json_token_end_array;
        *np = ++p;
        return 1;
        break;
    case json_char_begin_object:
        jtok->type = json_token_begin_object;
        *np = ++p;
        return 1;
        break;
    case json_char_end_object:
        jtok->type = json_token_end_object;
        *np = ++p;
        return 1;
        break;
    case json_char_name_separator:
        jtok->type = json_token_name_separator;
        *np = ++p;
        return 1;
        break;
    case json_char_value_separator:
        jtok->type = json_token_value_separator;
        *np = ++p;
        return 1;
        break;
    case 'f':
        if ((p+4) >= stop) { *np = p; return 0; }
        if ((*(p+1) == 'a') && (*(p+2) == 'l') &&
            (*(p+3) == 's') && (*(p+4) == 'e')) {
            jtok->type = json_token_false;
            *np = p+5;
            return 1;
        }
        break;
    case 'n':
        if ((p+3) >= stop) { *np = p; return 0; }
        if ((*(p+1) == 'u') && (*(p+2) == 'l') && (*(p+3) == 'l')) {
            jtok->type = json_token_null;
            *np = p+4;
            return 1;
        }
        break;
    case 't':
        if ((p+3) >= stop) { *np = p; return 0; }
        if ((*(p+1) == 'r') && (*(p+2) == 'u') && (*(p+3) == 'e')) {
            jtok->type = json_token_true;
            *np = p+4;
            return 1;
        }
        break;

    case json_char_quotation_mark:
        switch (json_string(p, stop, &jtok->value.string.string,
                            &jtok->value.string.size, np))
        {
        case 0:
            *np = p;
            return 0;
        case 1:
            jtok->value.string.need_free = 0;
            jtok->type = json_token_string;
            return 1;
        case 2:
            jtok->value.string.need_free = 1;
            jtok->type = json_token_string;
            return 1;
        }
        break;
    default:
        *np = p;
        return -1;
    }
    return -1;
}


/*

    looking for: value
      ws  -> looking for value
      'f' -> looking for false
      'n' -> looking for null
      't' -> looking for true
      begin-object -> looking for object
      begin-array  -> looking for array
      number
      string

 */

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

int json_number(uchar *buf, uchar *stop)
{
    return 42;
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

