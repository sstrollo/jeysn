# jeysn - yet another JSON parser for Erlang

Jeysn is a small, *fast*, and (currently) strict
[JSON](https://tools.ietf.org/html/rfc7159) parser for Erlang. It is
built around a re-entrant tokenizer implemented as a NIF.

## Erlang representation

### JSON -> Erlang

JSON strings are converted using the `decode/1` function (see
[API](#API) below). By default JSON values are converted according to
the table below (but there are several options that can change the
default).

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

#### Numbers

Numbers that have a decimal point will become a `float()` in Erlang,
otherwise they will be returned as integers.

#### Strings

The default is to return strings as binaries, but there are

#### Objects

The default is to return maps for objects, by using the option
`{object, list}` property lists can be returned instead. For property
lists the empty object `{}` is by default represented as `[{}]` in
Erlang.

#### Arrays

The default is to return lists


### Erlang -> JSON

| Erlang Type | JSON Representation |
|-------------|---------------------|
| `'false'`   | `false`             |
| `'null'`    | `null`              |
| `'true'`    | `true`              |
| `integer()` | `number`            |
| `float()`   | `number`            |
| `binary()`  | `string`            |
| `list()`    | `array`             |
| `map()`     | `object`            |

## API


## Roadmap

- Decide on the API (function names, returns - wrapped vs exceptions)
  and release version 1.0

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
