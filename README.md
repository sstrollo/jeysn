# jeysn - yet another JSON parser for Erlang

Jeysn is a small, *fast*, [PropEr](https://proper-testing.github.io)ly
tested, and (currently) strict
[JSON](https://tools.ietf.org/html/rfc7159) parser for Erlang. It can
decode JSON values represented as iodata, or from a streaming source
such as a socket or a file. It is built around a re-entrant tokenizer
implemented as a NIF (written in plain C).

## Examples

```
1> V = jeysn:decode(<<"{\"foo\": 42, \"bar\": null, \"baz\": [1,2,3]}">>).
#{<<"bar">> => null,<<"baz">> => [1,2,3],<<"foo">> => 42}
2> Compact = jeysn:encode(V).
<<"{\"bar\":null,\"baz\":[1,2,3],\"foo\":42}">>
3> Pretty = jeysn:encode(V, [pretty]).
<<"{\n  \"bar\": null,\n  \"baz\": [\n    1,\n    2,\n    3\n  ],\n  \"foo\": 42\n}\n">>
```

```
 cat test/t.json
{
  "foo": [1, 2, 3, 4],
  "bar": true,
  "x": false,
  "y": {},
  "z": {"1": "a", "2": "b"},
  "zzz": "aaa",
  "Z": null
}

% rebar3 shell

Eshell V12.2  (abort with ^G)
1> jeysn:decode_file("test/t.json").
#{<<"Z">> => null,<<"bar">> => true,
  <<"foo">> => [1,2,3,4],
  <<"x">> => false,<<"y">> => #{},
  <<"z">> => #{<<"1">> => <<"a">>,<<"2">> => <<"b">>},
  <<"zzz">> => <<"aaa">>}
2> jeysn:decode_file("test/t.json", [{object, list}]).
[{<<"foo">>,[1,2,3,4]},
 {<<"bar">>,true},
 {<<"x">>,false},
 {<<"y">>,[{}]},
 {<<"z">>,[{<<"1">>,<<"a">>},{<<"2">>,<<"b">>}]},
 {<<"zzz">>,<<"aaa">>},
 {<<"Z">>,null}]
```

## Using in your project

### Using [rebar](https://rebar3.org/)

Add the following `deps` to your `rebar.config`:

```Erlang
{deps,
 [
  {jeysn, {git, "https://github.com/sstrollo/jeysn.git", {branch, "main"}}}
 ]}.
```

### Using [erlang.mk](https://erlang.mk/)

Add Jeysn as a dependency using:

```Makefile
DEPS      = jeysn
dep_jeysn = git https://github.com/sstrollo/jeysn.git
```


## API

### Decoding API

#### JSON -> Erlang

JSON strings are converted using the `jeysn:decode/1` function. By
default JSON values are converted according to the table below (but
there are several options that can change the default).

| JSON      | Erlang representation    |           |
|-----------|--------------------------|-----------|
| `false`   | `'false'`                |           |
| `null`    | `'null'`                 |           |
| `true`    | `'true'`                 |           |
| `number`  | `integer()` or `float()` | See below |
| `string`  | `binary()`               | See below |
| `float()` | `number`                 |           |
| `object`  | `map()`                  | See below |
| `array`   | `list()`                 |           |

##### Numbers

Numbers that have a decimal point will become a `float()` in Erlang,
otherwise they will be returned as integers.

##### Strings

The default is to return strings as binaries, but there are

##### Objects

The default is to return maps for objects, by using the option
`{object, list}` property lists can be returned instead. For property
lists the empty object `{}` is by default represented as `[{}]` in
Erlang.

##### Arrays

The default is to return lists

#### Exported decode functions

```Erlang
-spec decode(String::iodata()) -> json_term().
-spec decode(String::iodata(), Options::decode_options()) -> json_term().

-spec decode_file(FileName::iodata()) -> json_term().
-spec decode_file(iodata(), Options::decode_options()) -> json_term().

-spec decode_io(read_fun()) -> json_term().
-spec decode_io(read_fun(), Options::decode_options()) -> json_term().
```

| Option                                                | Description                                                               |
|-------------------------------------------------------|---------------------------------------------------------------------------|
| `{'string', 'binary' \| 'string' \| 'existing_atom'}` | Decode JSON strings to one of these formats                               |
| `{'name, 'binary' \| 'string' \| 'existing_atom'}`    | Decode the name portion of a JSON name-value pair to one of these formats |
| `{'object', 'map'}`                                   | Decode a JSON object to an Erlang map (the default)                       |
| `{'object', 'list'}`                                  | Decode a JSON object to a property list                                   |

Example:

```Erlang
jeysn:decode(<<"{\"jeysn\": true, \"foobarbaz\": 42}">>, [{object,list},{name, existing_atom}]).
[{jeysn,true},{<<"foobarbaz">>,42}]
```


### Encoding API

Jeysn tries to be generous when creating JSON values from Erlang
terms, the default mapping is the following:

| Erlang Type | JSON Representation |
|-------------|---------------------|
| `'false'`   | `false`             |
| `'null'`    | `null`              |
| `'true'`    | `true`              |
| `integer()` | `number`            |
| `float()`   | `number`            |
| `binary()`  | `string`            |
| `atom()`    | `string`            |
| `list()`    | `array`             |
| `map()`     | `object`            |

But even without supplying options there are a number of more Erlang
terms that will be encoded to JSON:

| Erlang Type                                               | JSON Representation |
|-----------------------------------------------------------|---------------------|
| `{'string', iodata() \| string() \| atom() \| integer()}` | `string`            |
| `{'array', list()}`                                       | `array`             |
| `[{}]`                                                    | `{}`                |
| `{[]}`                                                    | `{}`                |
| `{'struct', list()}`                                      | `object`            |
| `{'object, list()}`                                       | `object`            |
| `{list()}`                                                | `object`            |
| `[{iodata()\|atom()\|integer(), json_term()}]`            | `object`            |

Jeysn can even encode records, see encode options below.

#### Exported encode functions

```Erlang
-spec encode(json_term()) -> iodata().
-spec encode(json_term(), Options::encode_options()) -> any().

-spec encode_file(json_term(), FileName::file:name_all()) -> 'ok'.
-spec encode_file(json_term(), FileName::file:name_all(), Options::encode_options()) -> 'ok'.

-spec encode_io(json_term()) -> 'ok'.
-spec encode_io(json_term(), Options::encode_options()) -> 'ok'.
-spec encode_io(json_term(),
                io:device() | write_fun(),
                Options::encode_options()) ->
                       any().
```

| Option                 | Description                                                               | Default |
|------------------------|---------------------------------------------------------------------------|---------|
| `{'space', N::0..}`    | Add N spaces after commas and colon                                       | `0`     |
| `'space'`              | Equivalent to `{space, 1}`                                                |         |
| `{'indent', N::0..}`   | When N > 0, add a newline and indent N spaces for every level             | `0`     |
| `'nl'`                 | When indenting, add a trailing newline after the last item                | `false` |
| `'pretty'`             | Equivalent to `[{space, 1}, {indent, 2}, {nl, true}]`                     |         |
| `'list_may_be_string'` | A list is treated as a string if `io_lib:printable_list()` returns `true` | `false` |

#### Encoding records

Jeysn can encode Erlang records as objects if you supply record
information in a map to the encode function. For example, given:

```Erlang
-record(foo, {a = 42, b = false, c = null, d, e}).
-record(bar, {boo, baz, bing}).
```

Then the following:

```Erlang
    Term = #foo{d = #bar{boo = <<"hello">>}},

    RI = #{foo => record_info(fields, foo),
           bar => record_info(fields, bar)},

    jeysn:encode_io(Term, [{records, RI}, pretty]).
```

Would result in

```JSON
{
  "a": 42,
  "b": false,
  "c": null,
  "d": {
    "boo": "hello",
    "baz": null,
    "bing": null
  },
  "e": null
}
```

Note that the atom `undefined` is translated to `null`. The option
`record_undefined` can be used to change this. Either set it to
`'remove'` to exclude all record items that are set to `undefined` or
to any other string that you would like `undefined` be translated
to. Using the same record definition and instance as above, the
following:

```Erlang
jeysn:encode_io(Term, [{records, RI}, {record_undefined, remove}, pretty]).
```

would result in

```JSON
{
  "a": 42,
  "b": false,
  "c": null,
  "d": {
    "boo": "hello"
  }
}
```


## Background

A number of years ago I wanted to be able to parse XML and JSON using
the same grammar, and using a saxing parser to support the grammar. I
started looking at [Expat](https://github.com/libexpat/libexpat) and
wrapped it in a NIF, and when it came to JSON I wrote my own tiny JSON
tokenizer in C. I never got around to publish it, but now I needed the
JSON parser, so I cleaned the original implementation up and called it
Jeysn. (The Expat wrapper is still in my desktop drawer, maybe I'll
need that one day in the future too...)

## Contributing

I'm happy to take pull requests, bug reports, and feature
requests. But please be aware that until I release 1.0 the API is
still shifting around a bit.

## Roadmap

- Decide on the API (function names, returns - wrapped vs exceptions)
  and release version 1.0

- Make sure jeysn is usable from Elixir

- Note: trailing text after a complete JSON value is currently ignored
  (will add possibility to return trailing text in future, see
  streaming below)

- Optionally allow some slack in the JSON input (misplaced commas,
  single quoted strings, barewords as names)

- Support various
  [JSON  Streaming](https://en.wikipedia.org/wiki/JSON_streaming) formats,
  with useful API:s (including possibility to return remaining
  characters)

- Add an event based pluggable callback API

- Add a top-level re-entrant decode API (for when you need the control loop

- Provide size limitations (make it possible to specify the maximum
  size of strings, objects, and arrays).
