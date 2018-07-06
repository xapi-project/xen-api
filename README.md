ocaml-mustache
==============

mustache.js logic-less templates in OCaml


Example usage
-------------

```ocaml
let tmpl =
  Mustache.of_string "Hello {{name}}\n\
                      Mustache is:\n\
                      {{#qualities}}\
                      * {{name}}\n\
                      {{/qualities}}"

let json =
  `O [ "name", `String "OCaml"
     ; "qualities", `A [ `O ["name", `String "awesome"]
                       ; `O ["name", `String "simple"]
                       ; `O ["name", `String "fun"]
                       ]
     ]

let rendered =
  Mustache.render tmpl json
```

Spec compliance
-----------

ocaml-mustache complies¹ to the latest [mustache specification](https://github.com/mustache/spec/tree/v1.1.3), and is automatically tested against it.

¹: except for lambdas and set delimiters tags.

Todo/Wish List
-----------
* Support for ropes


http://mustache.github.io/
