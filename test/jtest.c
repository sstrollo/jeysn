#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <ctype.h>

#include "json.h"

const char *json_token_to_string(json_token_t *jt)
{
    switch (jt->type) {
    case json_token_error:
        /* XXX */
        return "ERROR";
    case json_token_begin_array:
        return "[";
        break;
    case json_token_begin_object:
        return "{";
        break;
    case json_token_end_array:
        return "]";
        break;
    case json_token_end_object:
        return "}";
        break;
    case json_token_name_separator:
        return ":";
        break;
    case json_token_value_separator:
        return ",";
        break;
    case json_token_false:
        return "false";
        break;
    case json_token_null:
        return "null";
        break;
    case json_token_true:
        return "true";
        break;
    case json_token_number:
    case json_token_number_unsigned:
    case json_token_number_double:
    case json_token_number_string:
        return "NUMBER";
        break;
    case json_token_string:
        return "STRING";
    }
    return "?";
}

void print_str(uchar *str, size_t sz)
{
    int i; uchar *s;
    printf("%d [", (int)sz);
    for (i=0, s=str; i<sz; s++, i++) {
        printf("%d", (int)*s);
        if ((i+1) < sz) { printf(","); }
    }
    printf("]");
    printf(" [");
    for (i=0, s=str; i<sz; s++, i++) {
        printf("0x%02x", (int)*s);
        if ((i+1) < sz) { printf(","); }
    }
    printf("]");
    printf(" [");
    for (i=0, s=str; i<sz; s++, i++) {
        int c = (int)*s;
        if (isprint(c))
            printf("'%c'", c);
        else
            printf("'\\%03o'", c);
        if ((i+1) < sz) { printf(","); }
    }
    printf("]");
    printf("\n");
}

int main(int argc, char *argv[])
{
    if (argc < 2) {
        exit(1);
    } else {
        json_state_t jsp;
        int bufsz = atoi(argv[1]);
        int fd = open(argv[2], O_RDONLY);
        uchar buf[bufsz];
        json_state_init(&jsp, malloc, free);
        if (fd < 0) {
            perror("open()");
            goto done;
        }
        for (;;) {
            switch (json_next_token(&jsp)) {
            case json_result_error:
                printf("ERROR\n");
                goto done;
            case json_result_eof:
                printf("EOF\n");
                goto done;
            case json_result_more:
                printf("MORE BYTES\n");
                size_t rsz = read(fd, buf, bufsz);
                if (rsz > 0) {
                    json_state_add_buffer(&jsp, buf, rsz);
                } else {
                    if (rsz == 0) {
                        json_state_set_eof(&jsp);
                    } else {
                        perror("FILE ERROR");
                        goto done;
                    }
                }
                break;
            case json_result_token:
                printf("TOKEN(%d: %s) ", jsp.token.type,
                       json_token_to_string(&jsp.token));
                if (jsp.token.type == json_token_string) {
                    print_str(jsp.token.value.string.string,
                              jsp.token.value.string.size);
                    if (jsp.token.value.string.need_free) {
                        free(jsp.token.value.string.string);
                    }
                } else {
                    printf("\n");
                }
            }
        }
    done:
        json_state_destroy(&jsp);
    }
    exit(0);
}
