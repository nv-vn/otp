OCaml Type Providers
--------------------

F#-like "type providers" implemented in OCaml with the Extension Points API.

Using `ppx_json_types`:
-----------------------

The `ppx_json_types` type provider is meant for simple REST services where
existing APIs like Swagger would be overkill or too much complexity to deal
with, although (since it is not specifically targetting REST services) it
can be used for any sort of HTTP-based service where data is transferred in
JSON by a dynamically changing API.

In order to use the simplest version of the extension, you can simply initalize
a new JSON-type module by hosting a correctly defined type specification file*
and initializing the module with:
```ocaml
module MyJsonApi = [%json "http://mywebsite.com/typespec.json"]
```

As for the type specification, it must follow the simple type definition format.
The core format is a JSON object with two fields, `kind` and `val`. The `kind`
field is always one of the following options: `record`, `product`, `list`, or
`name`.

The simplest kind is `name`, which just uses a string for its `val` field (being
the name of the type). In this particular implementation of the API, the types
are mapped directly to existing OCaml types, so if you need compatibility with
a type specification defined for another language, simply define an alias to your
type. The next simplest kind is `list`. Lists just use another type for their `val`,
although most of the time this would be a `name`. The `product` kind describes a
product type/tuple, and uses a list of types for its `val` field. Finally, a `record`
describes a "struct" type, using a list of field objects. The field objects are
defined as objects with the sub-fields `name` (the name of the field) and `type`,
the type of the field as defined in the rest of the syntax.

An example is shown below:
```json
{
  "kind": "product",
  "val": [
    {"kind": "name", "val": "string"},
    {
      "kind": "list",
      "val": {
        "kind": "record",
        "val": [
          {
            "name": "x",
            "type": {"kind": "name", "type": "int"}
          },
          {
            "name": "y",
            "type": {"kind": "name", "type": "int"}
          }
        ]
      }
    }
  ]
}
```

In an OCaml-esque syntax, we could define this as
`string * {x : int; y : int} list`.

Since we are using the simpler `[%json ...]` version of the syntax extension,
the following functions will be defined for our module:
```ocaml
module MyModule = sig
  type t
  val show : t -> string (* Generic `show` *)
  val to_json : t -> Yojson.Safe.json
  val of_json : Yojson.Safe.json -> t
end
```

However, we can use the extra functions by changing our invocation of the PPX
to be:
```ocaml
module MyModule = [%json_extra "http://mywebsite.com/typespec.json"]
```

With these extra functions (which also depend on Lwt and Cohttp) the new module's
signature will be:
```ocaml
module MyModule = sig
  type t
  val show : t -> string (* Generic `show` *)
  val to_json : t -> Yojson.Safe.json
  val of_json : Yojson.Safe.json -> t
  val from_url : string -> t Lwt.t
  val to_string : t -> string (* JSON-formatted `show` *)
  val from_string : string -> t
end
```
