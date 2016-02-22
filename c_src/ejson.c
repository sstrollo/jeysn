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
static ERL_NIF_TERM am_eof;
static ERL_NIF_TERM am_string;
static ERL_NIF_TERM am_number;
static ERL_NIF_TERM am_error;
static ERL_NIF_TERM am_ok;


typedef struct ejson_state {
    ErlNifPid owner;       /* pid of the owning process */
    int string_format;     /* 0 binary, 1 string, 2 atom, 3 existing atom */
    int string_split;      /* -1 or 0..127 */
    json_state_t js;
} ejson_state_t;

static inline ERL_NIF_TERM make_json_string(ErlNifEnv *env, int sfmt,
                                            const char *str, size_t strsz)
{
    ERL_NIF_TERM term;
    switch (sfmt) {
    case 1:
        term = enif_make_string_len(env, str, strsz, ERL_NIF_LATIN1);
        break;
    case 2:
        term = enif_make_atom_len(env, str, strsz);
        break;
    case 3:
        if (enif_make_existing_atom_len(env, str, strsz, &term, ERL_NIF_LATIN1))
            break;
        /* fall-through to binary */
    default:
        memcpy(enif_make_new_binary(env, strsz, &term), str, strsz);
    }
    return term;
}

static ERL_NIF_TERM make_json_token(ErlNifEnv *env, ejson_state_t *ejs,
                                    int string_format, int string_split)
{
    int sfmt = (string_format == -1) ? ejs->string_format : string_format;
    int splitc = (string_split == -2) ? ejs->string_split : string_split;
    switch (ejs->js.token.type) {
    case json_token_error:
        /* XXX */
        return enif_make_copy(env, am_error);
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
        return enif_make_int64(env, ejs->js.token.value.number);
        break;
    case json_token_number_unsigned:
        return enif_make_uint64(env, ejs->js.token.value.number_unsigned);
        break;
    case json_token_number_double:
        return enif_make_double(env, ejs->js.token.value.number_double);
        break;
    case json_token_number_string:
    {
        const char *str = (const char *)ejs->js.token.value.string.string;
        size_t strsz = ejs->js.token.value.string.size;
//        enif_fprintf(stderr, "NUMBER |%*.s|\n", (int)strsz, str);
        return enif_make_tuple2(env,
                                enif_make_copy(env, am_number),
                                enif_make_string_len(env, str, strsz,
                                                     ERL_NIF_LATIN1));
    }
    case json_token_string:
    {
        ERL_NIF_TERM string;
        uchar *splitp;
        uchar *str = ejs->js.token.value.string.string;
        size_t strsz = ejs->js.token.value.string.size;
        if (splitc < 0 || (splitp = memchr(str, splitc, strsz)) == NULL) {
            string = make_json_string(env, sfmt, (const char *)str, strsz);
        } else {
            ERL_NIF_TERM hd, tl;
            hd = make_json_string(env, sfmt, (const char *)str, splitp - str);
            splitp++;
            tl = make_json_string(env, sfmt, (const char *)splitp,
                                  (str + strsz) - splitp);
            string = enif_make_list_cell(env, hd, tl);
        }
        if (ejs->js.token.value.string.need_free) {
            enif_free(ejs->js.token.value.string.string);
        }
        return enif_make_tuple2(env, enif_make_copy(env, am_string), string);
    }
    }
}

static ERL_NIF_TERM make_position(ErlNifEnv* env, ejson_state_t *ejs)
{
    ERL_NIF_TERM buf1, buf2;
    if (ejs->js.buf.buf) {
        size_t sz1 = ejs->js.buf.ptr - ejs->js.buf.buf;
        memcpy(enif_make_new_binary(env, sz1, &buf1), ejs->js.buf.buf, sz1);
        size_t sz2 = ejs->js.buf.stop - ejs->js.buf.ptr;
        memcpy(enif_make_new_binary(env, sz2, &buf2), ejs->js.buf.ptr, sz2);
    } else {
        enif_make_new_binary(env, 0, &buf1);
        enif_make_new_binary(env, 0, &buf2);
    }
    ERL_NIF_TERM pos =
        enif_make_tuple3(env, enif_make_uint(env, ejs->js.pos.pos),
                         enif_make_uint(env, ejs->js.pos.line),
                         enif_make_uint(env, ejs->js.pos.col));
    return enif_make_tuple3(env, pos, buf1, buf2);
}

static ErlNifResourceType *ejson_state_type = NULL;

static void destroy_state(ErlNifEnv *env, void *obj)
{
    ejson_state_t *ejs = obj;
    json_state_destroy(&ejs->js);
//    enif_fprintf(stderr, "destroy_state(<%p>)\n", ejs);
    return;
}

static inline int get_ejson_state_t(ErlNifEnv *env, ERL_NIF_TERM arg,
                                    ejson_state_t **ejs)
{
    ErlNifPid pid;
    if (!enif_get_resource(env, arg, ejson_state_type, (void **)ejs)) {
        return 0;
    }
    memset(&pid, 0, sizeof(pid));
    enif_self(env, &pid);
    /* Ensure resource is only used by one (owning) process (easier
     * than adding mutex around every call). Note: the nif library
     * should have a way of comparing two ErlNifPid objects. */
    if (memcmp(&pid, &(*ejs)->owner, sizeof(pid)) != 0) {
        return 0;
    }
    return 1;
}


static int get_string_format_arg(ErlNifEnv* env, ERL_NIF_TERM arg,
                                 int *string_format, int *string_split)
{
    const ERL_NIF_TERM *tup;
    ERL_NIF_TERM format;
    int arity = 0, tmp;
    if (enif_get_tuple(env, arg, &arity, &tup) && (arity == 2)) {
        format = tup[0];
        if (enif_get_int(env, tup[1], &tmp) && (tmp >= 0) && (tmp <= 127)) {
            *string_split = tmp;
        } else {
            return 0;
        }
    } else {
        format = arg;
    }
    if (enif_get_int(env, format, &tmp) && (tmp >= 0) && (tmp <= 3)) {
        *string_format = tmp;
        return 1;
    }
    return 0;
}

static ERL_NIF_TERM next_token(ErlNifEnv* env,
                               int argc, const ERL_NIF_TERM argv[])
{
    ejson_state_t *ejs = NULL;
    int string_format = -1, string_split = -2;
    if (!get_ejson_state_t(env, argv[0], &ejs)) {
        return enif_make_badarg(env);
    }
    if ((argc == 2) &&
        !get_string_format_arg(env, argv[1], &string_format, &string_split)) {
        return enif_make_badarg(env);
    }
    switch (json_next_token(&ejs->js)) {
    case json_result_more:
        return enif_make_copy(env, am_more);
    case json_result_eof:
        return enif_make_copy(env, am_eof);
    case json_result_token:
    {
        ERL_NIF_TERM token =
            make_json_token(env, ejs, string_format, string_split);
        if (ejs->js.token.type == json_token_error) {
            return enif_make_tuple3(env,
                                    enif_make_copy(env, am_error),
                                    token,
                                    make_position(env, ejs));
        } else {
            return token;
        }
    }
    case json_result_error:
        return enif_make_tuple2(env, enif_make_copy(env, am_error),
                                enif_make_int(env, -1));
    }
    /* NOT REACHED */
    return enif_make_badarg(env);
}

static ERL_NIF_TERM data(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ejson_state_t *ejs = NULL;
    ErlNifBinary bin;
    if (!get_ejson_state_t(env, argv[0], &ejs)) {
        return enif_make_badarg(env);
    }
    if (enif_is_atom(env, argv[1]) && (enif_compare(am_eof, argv[1]) == 0)) {
        json_state_set_eof(&ejs->js);
        return enif_make_copy(env, am_ok);
    }
    if (ejs->js.eof) {
        return enif_make_tuple2(env, enif_make_copy(env, am_error),
                                enif_make_copy(env, am_eof));
    }
    if ((enif_is_binary(env, argv[1]) &&
         enif_inspect_binary(env, argv[1], &bin)) ||
        enif_inspect_iolist_as_binary(env, argv[1], &bin))
    {
        json_state_add_buffer(&ejs->js, bin.data, bin.size);
        return enif_make_copy(env, am_ok);
    }
    return enif_make_badarg(env);
}

static ERL_NIF_TERM
get_position(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ejson_state_t *ejs = NULL;
    if (!get_ejson_state_t(env, argv[0], &ejs)) {
        return enif_make_badarg(env);
    }
    return make_position(env, ejs);
}

static ERL_NIF_TERM debug(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM buf1, buf2;
    ejson_state_t *ejs = NULL;
    if (!get_ejson_state_t(env, argv[0], &ejs)) {
        return enif_make_badarg(env);
    }
    if (ejs->js.buf.buf) {
        size_t sz1 = ejs->js.buf.stop - ejs->js.buf.buf;
        memcpy(enif_make_new_binary(env, sz1, &buf1), ejs->js.buf.buf, sz1);
        size_t sz2 = ejs->js.buf.stop - ejs->js.buf.ptr;
        memcpy(enif_make_new_binary(env, sz2, &buf2), ejs->js.buf.ptr, sz2);
    } else {
        buf1 = enif_make_list(env, 0);
        buf2 = enif_make_list(env, 0);
    }
    ERL_NIF_TERM pos =
        enif_make_tuple3(env, enif_make_uint(env, ejs->js.pos.pos),
                         enif_make_uint(env, ejs->js.pos.line),
                         enif_make_uint(env, ejs->js.pos.col));
    return enif_make_tuple3(env, pos, buf1, buf2);
}

static ERL_NIF_TERM init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM ret;
    ejson_state_t *ejs;

    ejs = enif_alloc_resource(ejson_state_type, sizeof(ejson_state_t));
    memset(ejs, 0, sizeof(*ejs));
    json_state_init(&ejs->js, enif_alloc, enif_free);
    ejs->string_format = 0;
    ejs->string_split = -1;
    enif_self(env, &ejs->owner);

    if ((argc > 0) &&
        enif_is_list(env, argv[0]) && !enif_is_empty_list(env, argv[0]))
    {
        ERL_NIF_TERM hd, tl = argv[0];
        do {
            const ERL_NIF_TERM *tup;
            int arity = 0;
            enif_get_list_cell(env, tl, &hd, &tl);
            if (enif_get_tuple(env, hd, &arity, &tup) && (arity == 2)) {
                if (enif_compare(am_string, tup[0]) == 0) {
                    get_string_format_arg(env, tup[1],
                                          &ejs->string_format,
                                          &ejs->string_split);
                }
            }
        } while (enif_is_list(env, tl) && !enif_is_empty_list(env, tl));
    }

//    enif_fprintf(stderr, "new_state(<%p>)\n", ejs);

    /* transfer ownership to calling process */
    ret = enif_make_resource(env, ejs);
    enif_release_resource(ejs);

    return ret;
}

static ERL_NIF_TERM escape_string(ErlNifEnv* env, int argc,
                                  const ERL_NIF_TERM argv[])
{
    int return_original_arg = 0;
    ErlNifBinary bin;
    if (enif_is_binary(env, argv[0]) &&
        enif_inspect_binary(env, argv[0], &bin))
    {
        return_original_arg = 1;
    } else {
        if (!enif_inspect_iolist_as_binary(env, argv[0], &bin)) {
            return enif_make_badarg(env);
        }
    }
    {
        size_t sz;
        switch (json_string_escape_size(bin.data, bin.size, &sz)) {
        case -1:
            return enif_make_badarg(env);
        case 1:
        {
            ERL_NIF_TERM newbin;
            uchar *dst = enif_make_new_binary(env, sz, &newbin);
            json_string_escape(bin.data, bin.size, dst, sz);
            return newbin;
        }
        default:
            if (return_original_arg) {
                return argv[0];
            } else {
                return enif_make_binary(env, &bin);
            }
        }
    }
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
    am_eof = enif_make_atom(env, "eof");
    am_string = enif_make_atom(env, "string");
    am_number = enif_make_atom(env, "number");
    am_error = enif_make_atom(env, "error");
    am_ok = enif_make_atom(env, "ok");

    return 0;
}

static ErlNifFunc nif_funcs[] = {
    {"init", 0, init}
    , {"init_nif", 1, init}
    , {"next_token_nif", 1, next_token}
    , {"next_token_nif", 2, next_token}
    , {"data", 2, data}
    , {"get_position", 1, get_position}
    , {"debug", 1, debug}
    , {"escape_string", 1, escape_string}
};

ERL_NIF_INIT(ejson, nif_funcs, atload, NULL, NULL, NULL);
