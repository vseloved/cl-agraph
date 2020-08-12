# CL-AGRAPH — A minimal portable Lisp client for AllegroGraph

Tested with AG v7.0.0.

## TL;DR

The current docs, likewise the client itself, are minimal. A brief introduction can be found in [this blog post](https://lisp-univ-etc.blogspot.com/2020/08/announcing-cl-agraph.html).

## Data Structures

The basic data structure of agraph is a `triple` (actually, a quad, but it's called a triple for historic reasons)

```
(defstruct (triple (:conc-name nil)
                   (:print-object print-triple))
  s p o g
  obj-lang obj-type)
```

Triple components are, `uri`s, `blank-node`s, and literals (strings, numbers, booleans).

When the triple is printed, it is displayed in the standard nquads format:

```
AGRAPH> (<> (make-blank-node) "http://foo.com/foo" "bar" :g "http://foo.com/baz" :lang "en")
_:bn1899 <http://foo.com/foo> "bar"@en <http://foo.com/baz> .
```

I have chosen the diamond sign (`<>`) to signify triples (as in ntriples/nquads formats, the URIs are enclosed in it). So, the API functions that deal with triples are mostly accompanied by this sign. The parts enclosed in `<>` in the nquads representation are `uri`s.

The function `<>` ensures proper types of the triple components. There's also raw `make-triple` that creates the `triple` structure with the arguments passed as is.

RDF permits specifying expansions for uri prefixes and the `uri` function is aware of that:

```
AGRAPH> (register-prefix "foo" "http://foo.com/")
"http://foo.com/"
AGRAPH> (<> (make-blank-node) "rdf:type" (uri "foo:quux"))
_:bn1921 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://foo.com/quux> .
```

You can see that we have used the default expansion for prefix `rdf` and the user-defined one for prefix `foo`. The object of the triple was explicitly converted to `uri` before it was passed to the `<>` function as objects may be either strings or uris.

The other core data structure of CL-AGRAPH is `ag-config`. It lists the connection parameters that are used to make the client HTTP requests. Most of the parameters have reasonable defaults, but at least the `:repo` should be specified. The macro `with-ag` is intended for controlling the config parameters.


## API

The main functions of the triple API are the following:

- `<>` is a shorthand for creating new triples that ensures that its arguments are converted to proper types. You have already seen it in action above
- `get<>` is for getting a list of triples by subject (`:s`), predicate (`:p`), object (`:o`), and graph (`:g`)
- `add<>` and `rem<>` will add/remove the appropriate triples
- `map<>` and `do<>` allows iteration over them

Here is a simple session of interacting with the triple-store:

```
AGRAPH> (with-ag (:repo "test" :port 12345)
          (let ((subj (make-blank-node)))
            (add<> (<> subj "rdf:type" (uri "foo:bar"))
                   (<> subj "foo:baz" "quux" :lang "en"))))
2
AGRAPH> (with-ag (:repo "test" :port 12345)
          (get<>))
(_:bF049DE41x7325578 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://foo.com/bar> .
 _:bF049DE41x7325578 <http://foo.com/baz> "quux"@en .)
AGRAPH> (with-ag (:repo "test" :port 12345)
          (rem<> :g (uri "foo:bar")))
0
AGRAPH> (with-ag (:repo "test" :port 12345)
          (get<>))
(_:bF049DE41x7325578 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://foo.com/bar> .
 _:bF049DE41x7325578 <http://foo.com/baz> "quux"@en .)
AGRAPH> (with-ag (:repo "test" :port 12345)
          (rem<> :o (uri "foo:bar")))
1
AGRAPH> (with-ag (:repo "test" :port 12345)
          (get<>))
(_:bF049DE41x7325578 <http://foo.com/baz> "quux"@en .)
AGRAPH> (with-ag (:repo "test" :port 12345)
          (count<>))
1
```

As you can see, `add<>` functions a bit differently from other functions in this list
as it needs full triples to operate, not just triple components.

The function `sparql` allows running complex SPARQL queries and obtaining the results either as a list of hash-tables (default) or as triples (in case, either the query is a `select *` or some of the variables `S`, `P`, `O`, `G` are present in the results. To make `sparql` output resulting triples, `:want-triples` should be set.

```
AGRAPH> (with-ag (:repo "test" :port 12345)
          (sparql "select * { ?s ?p ?o }" :want-triples t))
(_:bF049DE41x7325578 <http://foo.com/baz> "quux"@en .)
AGRAPH> (with-ag (:repo "test" :port 12345)
          (sparql "select ?p { ?s ?p ?o }"))
(#{EQUALP
   "p" <http://foo.com/baz>
  } )
```

The queries to `sparql` function may be supplied not only as string but also as sexps that are first converted to strings and then sent to the engine. Those are called **Symbolic SPARQL**.

Here is a simple example:

```
AGRAPH> (with-ag (:repo "test" :port 12345)
          (sparql '(select * (?s ?p (:@ "quux" "en")))))
(#{EQUALP
   "P" <http://foo.com/baz>
   "S" _:bF049DE41x7325578
  } )
```

You can more elaborate ones in the [test file](test/test-sparql.lisp).

The transaction API supports the basic `commit` and `rollback` commands.

The repo API implements repo creation and deletion with `create-repo` and `delete-repo`.


## License

MIT (See LICENSE file for details).
