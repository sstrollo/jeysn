#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "erl_nif.h"
#include "json.h"

/* http://www.w3.org/TR/REC-xml/ */
/* http://expat.sourceforge.net/ */


typedef struct eparse_state {
    int x;
    unsigned int lineno;
    unsigned char *buf;
    unsigned char *ptr;
    size_t buf_sz;
} eparse_state_t;


static ErlNifResourceType *eparse_state_type = NULL;

static void destroy_state(ErlNifEnv *env, void *obj)
{
    eparse_state_t *ps = obj;
    if (ps->buf) { enif_free(ps->buf); }
    enif_fprintf(stderr, "destroy_state(<%p>)\n", ps);
    return;
}

static ERL_NIF_TERM parse(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    eparse_state_t *ps = NULL;
    ErlNifBinary bin;
    if (!enif_get_resource(env, argv[0], eparse_state_type, (void **)&ps)) {
        return enif_make_badarg(env);
    }
    if (enif_is_empty_list(env, argv[1])) {
        if (ps->buf == NULL)
            goto more_bytes;
    } else {
        if (!enif_is_binary(env, argv[1]) ||
            !enif_inspect_binary(env, argv[1], &bin))
        {
            return enif_make_badarg(env);
        }
        // enif_inspect_iolist_as_binary(env, argv[1], &bin);

        if (ps->buf) {
            unsigned char *tmp = ps->buf;
            size_t keep = ps->buf_sz - (ps->ptr - ps->buf);
            ps->buf_sz = keep + bin.size;
            ps->buf = enif_alloc(ps->buf_sz);
            memcpy(ps->buf, ps->ptr, keep);
            memcpy(ps->buf + keep, bin.data, bin.size);
            enif_free(tmp);
        } else {
            ps->buf_sz = bin.size;
            ps->buf = enif_alloc(ps->buf_sz);
            memcpy(ps->buf, bin.data, bin.size);
        }
        ps->ptr = ps->buf;
    }
    unsigned char *stop = ps->buf + ps->buf_sz;
    for (;;) {
        if (ps->ptr == stop) goto more_bytes;
        if (isspace((int)*ps->ptr)) {
            if (*ps->ptr == '\n') ps->lineno++;
            ps->ptr++;
        } else {
            switch (*ps->ptr) {
            case '{':
            case '}':
            case '[':
            case ']':
            case ':':
            case ',':
            {
                ERL_NIF_TERM atom =
                    enif_make_atom_len(env, (char *)ps->ptr, 1);
                ps->ptr++;
                return enif_make_tuple2(env, enif_make_int(env,2), atom);
                break;
            }
/*
            case 'f':
                break;
            case 'n':
                break;
            case 't':
                break;
*/
            case '"':
            {
                unsigned char *str = NULL;
                size_t sz = 0;
                ERL_NIF_TERM bin;
                switch (json_string(ps->ptr, stop, &str, &sz, &(ps->ptr)))
                {
                case -1:
                    return enif_make_badarg(env);
                case 0:
                    goto more_bytes;
                case 1:
                    memcpy(enif_make_new_binary(env, sz, &bin), str, sz);
                    break;
                case 2:
                    memcpy(enif_make_new_binary(env, sz, &bin), str, sz);
                    free(str);
                    break;
                }
                return enif_make_tuple2(env, enif_make_int(env,2), bin);
            }
            default:
            {
                unsigned char *p = ps->ptr;
                for (; (p < stop) && !isspace((int)*p); p++) ;
                if (isspace((int)*p)) {
                    ERL_NIF_TERM atom =
                        enif_make_atom_len(env, (char *)ps->ptr,
                                           p - ps->ptr);
                    ps->ptr = p;
                    return enif_make_tuple2(env, enif_make_int(env,2), atom);
                } else {
                    goto more_bytes;
                }
            }
            }
        }
    }

    more_bytes:
    return enif_make_tuple1(env, enif_make_int(env, 1));
}

static ERL_NIF_TERM debug(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM buf1, buf2;
    eparse_state_t *ps = NULL;
    if (!enif_get_resource(env, argv[0], eparse_state_type, (void **)&ps)) {
        return enif_make_badarg(env);
    }
    memcpy(enif_make_new_binary(env, ps->buf_sz, &buf1),
           ps->buf, ps->buf_sz);
    size_t sz2 = ps->buf_sz - (ps->ptr - ps->buf);
    memcpy(enif_make_new_binary(env, sz2, &buf2), ps->ptr, sz2);
    return enif_make_tuple3(env, enif_make_uint(env, ps->lineno), buf1, buf2);
}

static ERL_NIF_TERM init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM ret;
    eparse_state_t *ep;

    ep = enif_alloc_resource(eparse_state_type, sizeof(eparse_state_t));
    memset(ep, 0, sizeof(*ep));
    enif_fprintf(stderr, "new_state(<%p>)\n", ep);

    /* transfer ownership to calling process */
    ret = enif_make_resource(env, ep);
    enif_release_resource(ep);

    return ret;
}


static int atload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceType* st;

    st = enif_open_resource_type(env, "eparse", "state",
                                 destroy_state,
                                 ERL_NIF_RT_CREATE, NULL);
    if (st == NULL)
        return -1;

    eparse_state_type = st;

    return 0;
}

static ErlNifFunc nif_funcs[] = {
    {"init", 1, init}
    , {"parse", 2, parse}
    , {"debug", 1, debug}
};

ERL_NIF_INIT(eparse, nif_funcs, atload, NULL, NULL, NULL);
