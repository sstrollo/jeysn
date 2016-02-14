#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>

#include "json.h"



void print_str(unsigned char *s, size_t sz)
{
    int i;
    printf("%d [", (int)sz);
    for (i=0; i<sz; s++, i++) {
        printf("%d", (int)*s);
        if ((i+1) < sz) { printf(","); }
    }
    printf("]\n");
}

void tokenize_string(char *string)
{
    unsigned char *str = (unsigned char *)(string ? string : "\"abc\"");
    unsigned char *stop = str + strlen((char *)str);

#if 1
    for (;;) {
        json_token_t jtok;
        switch (json_token(str, stop, &jtok, &str)) {
        case -1:
            printf("ERROR\n");
            goto done;
        case 0:
            printf("MORE BYTES\n");
            goto done;
            break;
        case 1:
            printf("TOKEN(%d) ", jtok.type);
            if (jtok.type == json_token_string) {
                print_str(jtok.value.string.string, jtok.value.string.size);
                if (jtok.value.string.need_free) {
                    free(jtok.value.string.string);
                }
            } else {
                printf("\n");
            }
        }
    }
#endif
#if 0
    for (;;) {
        unsigned char *res;
        size_t size;
        switch (json_string(str, stop, &res, &size, &str)) {
        case -1:
            printf("NOT A STRING\n");
            goto done;
            break;
        case 0:
            printf("MORE BYTES\n");
            goto done;
            break;
        case 1:
            print_str(res, size);
            break;
        case 2:
            print_str(res, size);
            free(res);
            break;
        }
    }
#endif
done:
    printf("REST: |%s|\n", (char *)str);

}

int main(int argc, char *argv[])
{
    if (argc < 2) {
        tokenize_string((argc>1) ? argv[1] : NULL);
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
            case -1:
                printf("ERROR\n");
                goto done;
            case 0:
                if (jsp.eof) {
                    printf("EOF\n");
                    goto done;
                }
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
            case 1:
                printf("TOKEN(%d) ", jsp.token.type);
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
