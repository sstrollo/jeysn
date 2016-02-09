

typedef unsigned char uchar;

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
    json_token_string
} json_token_type_t;

typedef struct json_token {
    json_token_type_t type;
    union {
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

extern int json_token(uchar *buf, uchar *stop, json_token_t *jtok, uchar **np);

extern int json_string(unsigned char *buf, unsigned char *stop,
                       unsigned char **str, size_t *size, unsigned char **np);

