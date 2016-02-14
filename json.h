

typedef unsigned char uchar;


#define json_state_at_eob(JSP) (((JSP)->buf.buf == NULL) || \
                                ((JSP)->buf.ptr == \
                                 ((JSP)->buf.buf + (JSP)->buf.size)))
#define json_state_at_eof(JSP) ((JSP)->eof && json_state_at_eob(JSP))

#define json_state_set_eof(JSP) { (JSP)->eof = 1; }

/*

      JSON-text = ws value ws

   These are the six structural characters:

      begin-array     = ws %x5B ws  ; [ left square bracket

      begin-object    = ws %x7B ws  ; { left curly bracket

      end-array       = ws %x5D ws  ; ] right square bracket

      end-object      = ws %x7D ws  ; } right curly bracket

      name-separator  = ws %x3A ws  ; : colon

      value-separator = ws %x2C ws  ; , comma

   Insignificant whitespace is allowed before or after any of the six
   structural characters.

      ws = *(
              %x20 /              ; Space
              %x09 /              ; Horizontal tab
              %x0A /              ; Line feed or New line
              %x0D )              ; Carriage return


      value = false / null / true / object / array / number / string

      false = %x66.61.6c.73.65   ; false

      null  = %x6e.75.6c.6c      ; null

      true  = %x74.72.75.65      ; true
*/

typedef enum json_token_type {
    json_token_error,
    json_token_begin_array,
    json_token_begin_object,
    json_token_end_array,
    json_token_end_object,
    json_token_name_separator,
    json_token_value_separator,
    json_token_false,
    json_token_null,
    json_token_true,
    json_token_number,
    json_token_number_unsigned,
    json_token_number_double,
    json_token_number_string,
    json_token_string
} json_token_type_t;

typedef enum json_error {
    json_error_invalid_char,    /* unexpected character */
    json_error_eof              /* eof while scanning token */
} json_error_t;

typedef struct json_token {
    json_token_type_t type;
    union {
        struct error {
            json_error_t code;
            const char *string;
        } error;
        struct string {
            uchar *string;
            size_t size;
            int need_free;
        } string;
        int64_t  number;
        uint64_t number_unsigned;
        double   number_double;
    } value;
} json_token_t;


struct json_buffer {
    uchar *buf;
    uchar *stop;                /* last valid position */
    uchar *ptr;                 /* current position in buf */
    size_t size;                /* size of the buffer */
    // struct json_buffer *next;
};

struct json_pos {
    size_t pos;
    size_t line;
    size_t col;
};

typedef struct json_state {
    int eof;
    struct json_pos pos;
    struct json_buffer buf;
    json_token_t token;
    void *(*alloc)(size_t size);
    void (*free)(void *ptr);
} json_state_t;

typedef enum json_result {
    json_result_eof,
    json_result_more,
    json_result_token,
    json_result_error
} json_result_t;

extern void json_state_init(json_state_t *jsp, void *(*alloc)(size_t size),
                            void (*free)(void *ptr));

extern void json_state_destroy(json_state_t *jsp);

extern void json_state_add_buffer(json_state_t *jsp, void *p, size_t sz);

extern json_result_t json_next_token(json_state_t *jsp);

extern int json_token(uchar *buf, uchar *stop, json_token_t *jtok, uchar **np);

extern int json_string(unsigned char *buf, unsigned char *stop,
                       unsigned char **str, size_t *size, unsigned char **np);

