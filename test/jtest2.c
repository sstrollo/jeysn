#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <ctype.h>

#include "json.h"

int main(int argc, char *argv[])
{
    int result = 0;
    if (argc != 2) {
        exit(1);
    } else {
        json_state_t jsp;
        int bufsz = 8192;
        int fd = open(argv[1], O_RDONLY);
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
                result = 1;
                goto done;
            case json_result_eof:
                printf("EOF\n");
                goto done;
            case json_result_more:
            {
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
            }
            case json_result_token:
                printf("TOKEN(%d)\n", jsp.token.type);
                break;
            }
        }
    done:
        json_state_destroy(&jsp);
    }
    exit(1);
}
