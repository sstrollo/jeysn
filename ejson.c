#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "erl_nif.h"
#include "json.h"


static ERL_NIF_TERM am_json_token_begin_array;
static ERL_NIF_TERM am_json_token_begin_object;
static ERL_NIF_TERM am_json_token_end_array;
static ERL_NIF_TERM am_json_token_end_object;
static ERL_NIF_TERM am_json_token_name_separator;
static ERL_NIF_TERM am_json_token_value_separator;
static ERL_NIF_TERM am_json_token_false;
static ERL_NIF_TERM am_json_token_null;
static ERL_NIF_TERM am_json_token_true;
static ERL_NIF_TERM am_more;
static ERL_NIF_TERM am_end;
static ERL_NIF_TERM am_token;
static ERL_NIF_TERM am_string;
static ERL_NIF_TERM am_number;
static ERL_NIF_TERM am_error;


typedef struct ejson_state {
    int string_format;     /* 1 binary, 2 string, 3 atom, 4 existing atom */
    unsigned int lineno;
    uchar *buf;
    uchar *ptr;
    size_t buf_sz;
    json_token_t jtok;
} ejson_state_t;

static ERL_NIF_TERM make_json_token(ErlNifEnv *env, ejson_state_t *ejs,
                                    int string_format)
{
    int sfmt = (string_format == 0) ? ejs->string_format : string_format;
    switch (ejs->jtok.type) {
    case json_token_begin_array:
        return enif_make_copy(env, am_json_token_begin_array);
        break;
    case json_token_begin_object:
        return enif_make_copy(env, am_json_token_begin_object);
        break;
    case json_token_end_array:
        return enif_make_copy(env, am_json_token_end_array);
        break;
    case json_token_end_object:
        return enif_make_copy(env, am_json_token_end_object);
        break;
    case json_token_name_separator:
        return enif_make_copy(env, am_json_token_name_separator);
        break;
    case json_token_value_separator:
        return enif_make_copy(env, am_json_token_value_separator);
        break;
    case json_token_false:
        return enif_make_copy(env, am_json_token_false);
        break;
    case json_token_null:
        return enif_make_copy(env, am_json_token_null);
        break;
    case json_token_true:
        return enif_make_copy(env, am_json_token_true);
        break;
    case json_token_number:
        return enif_make_int64(env, ejs->jtok.value.number);
        break;
    case json_token_number_unsigned:
        return enif_make_uint64(env, ejs->jtok.value.number_unsigned);
        break;
    case json_token_number_double:
        return enif_make_double(env, ejs->jtok.value.number_double);
        break;
    case json_token_string:
    {
        ERL_NIF_TERM string;
        uchar *str = ejs->jtok.value.string.string;
        size_t strsz = ejs->jtok.value.string.size;
        switch (sfmt) {
        case 2:
            string = enif_make_string_len(env, (const char *)str, strsz,
                                          ERL_NIF_LATIN1);
            break;
        case 3:
            string = enif_make_atom_len(env, (const char *)str, strsz);
            break;
        case 4:
            /* XXX existing atom */
            string = enif_make_atom_len(env, (const char *)str, strsz);
            break;
        case 5:
        case 6:
        case 7:
        {
            /* split */
            uchar *split = memchr(str, ':', strsz);
            if (split) {
                ERL_NIF_TERM hd, tl;
                /* XXX */
                hd = enif_make_atom_len(env, (const char *)str, split - str);
                split++;
                tl = enif_make_atom_len(env, (const char *)split,
                                        (str + strsz) - split);
                string = enif_make_list_cell(env, hd, tl);
            } else {
                string = enif_make_atom_len(env, (const char *)str, strsz);
            }
            break;
        }
        default:
            memcpy(enif_make_new_binary(env, strsz, &string), str, strsz);
        }
        if (ejs->jtok.value.string.need_free) {
            enif_free(ejs->jtok.value.string.string);
        }
        return enif_make_tuple2(env, enif_make_copy(env, am_string), string);
        break;
    }
    }
}


static ErlNifResourceType *ejson_state_type = NULL;

static void destroy_state(ErlNifEnv *env, void *obj)
{
    ejson_state_t *ejs = obj;
    if (ejs->buf) { enif_free(ejs->buf); }
    enif_fprintf(stderr, "destroy_state(<%p>)\n", ejs);
    return;
}

static ERL_NIF_TERM next_token(ErlNifEnv* env,
                               int argc, const ERL_NIF_TERM argv[])
{
    ejson_state_t *ejs = NULL;
    ErlNifBinary bin;
    int string_as = 0, end_as_more = 0;
    if (!enif_get_resource(env, argv[0], ejson_state_type, (void **)&ejs)) {
        return enif_make_badarg(env);
    }
    if (!enif_get_int(env, argv[2], &string_as)) {
        return enif_make_badarg(env);
    }
    if (!enif_get_int(env, argv[3], &end_as_more)) {
        return enif_make_badarg(env);
    }
    if (enif_is_empty_list(env, argv[1])) {
        if (ejs->buf == NULL)
            return enif_make_tuple1(env, enif_make_int(env, 1));
    } else {
        if (!enif_is_binary(env, argv[1]) ||
            !enif_inspect_binary(env, argv[1], &bin))
        {
            if (!enif_inspect_iolist_as_binary(env, argv[1], &bin)) {
                return enif_make_badarg(env);
            }
        }

        if (ejs->buf) {
            uchar *tmp = ejs->buf;
            size_t keep = ejs->buf_sz - (ejs->ptr - ejs->buf);
            ejs->buf_sz = keep + bin.size;
            ejs->buf = enif_alloc(ejs->buf_sz);
            memcpy(ejs->buf, ejs->ptr, keep);
            memcpy(ejs->buf + keep, bin.data, bin.size);
            enif_free(tmp);
        } else {
            ejs->buf_sz = bin.size;
            ejs->buf = enif_alloc(ejs->buf_sz);
            memcpy(ejs->buf, bin.data, bin.size);
        }
        ejs->ptr = ejs->buf;
    }
    uchar *stop = ejs->buf + ejs->buf_sz;

    if (ejs->ptr == stop) {
        goto return_more_or_end;
    }
    switch (json_token(ejs->ptr, stop, &ejs->jtok, &ejs->ptr)) {
    case -1:
        /* error */
        return enif_make_tuple2(env, enif_make_copy(env, am_error),
                                enif_make_int(env, -1));
        break;
    case 0:
        /* more bytes */
        goto return_more_or_end;
        break;
    case 1:
    default:
        return enif_make_tuple2(env, enif_make_copy(env, am_token),
                                make_json_token(env, ejs, string_as));
        break;
    }
    return_more_or_end:
    if ((ejs->buf == NULL) || (ejs->ptr == stop)) {
        /* end */
        if (end_as_more) {
            return enif_make_copy(env, am_more);
        } else {
            return enif_make_copy(env, am_end);
        }
    } else {
        return enif_make_copy(env, am_more);
    }
}

static ERL_NIF_TERM debug(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM buf1, buf2;
    ejson_state_t *ejs = NULL;
    if (!enif_get_resource(env, argv[0], ejson_state_type, (void **)&ejs)) {
        return enif_make_badarg(env);
    }
    memcpy(enif_make_new_binary(env, ejs->buf_sz, &buf1),
           ejs->buf, ejs->buf_sz);
    size_t sz2 = ejs->buf_sz - (ejs->ptr - ejs->buf);
    memcpy(enif_make_new_binary(env, sz2, &buf2), ejs->ptr, sz2);
    return enif_make_tuple3(env, enif_make_uint(env, ejs->lineno), buf1, buf2);
}

static ERL_NIF_TERM init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM ret;
    ejson_state_t *ep;

    ep = enif_alloc_resource(ejson_state_type, sizeof(ejson_state_t));
    memset(ep, 0, sizeof(*ep));
    ep->string_format = 1;
    enif_fprintf(stderr, "new_state(<%p>)\n", ep);

    /* transfer ownership to calling process */
    ret = enif_make_resource(env, ep);
    enif_release_resource(ep);

    return ret;
}


static int atload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceType* st;

    st = enif_open_resource_type(env, "ejson", "state",
                                 destroy_state,
                                 ERL_NIF_RT_CREATE, NULL);
    if (st == NULL)
        return -1;

    ejson_state_type = st;

    am_json_token_begin_array = enif_make_atom(env, "[");
    am_json_token_begin_object = enif_make_atom(env, "{");
    am_json_token_end_array = enif_make_atom(env, "]");
    am_json_token_end_object = enif_make_atom(env, "}");
    am_json_token_name_separator = enif_make_atom(env, ":");
    am_json_token_value_separator = enif_make_atom(env, ",");
    am_json_token_false = enif_make_atom(env, "false");
    am_json_token_null = enif_make_atom(env, "null");
    am_json_token_true = enif_make_atom(env, "true");
    am_more = enif_make_atom(env, "more");
    am_end = enif_make_atom(env, "eof");
    am_token = enif_make_atom(env, "token");
    am_string = enif_make_atom(env, "string");
    am_number = enif_make_atom(env, "number");
    am_error = enif_make_atom(env, "error");

    return 0;
}

static ErlNifFunc nif_funcs[] = {
    {"init", 1, init}
    , {"next_token", 4, next_token}
//    , {"data", 2, data}
//    , {"get_position", 1, get_position}
//    , {"get_remaining", 1, get_remaining}
    , {"debug", 1, debug}
};

ERL_NIF_INIT(ejson, nif_funcs, atload, NULL, NULL, NULL);
