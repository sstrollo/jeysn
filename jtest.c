#include <stdio.h>
#include <string.h>

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

int main(int argc, char *argv[])
{
    unsigned char *str = (unsigned char *)((argc > 1) ? argv[1] : "\"abc\"");
    unsigned char *stop = str + strlen((char *)str);

//    printf("|%.*s|\n", 4, "hello world");
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
done:
    printf("REST: |%s|\n", (char *)str);
    exit(0);
}
