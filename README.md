# Fish

This repo contains an implementation of the Fish game for [cs4500 Software
Development]( https://www.ccs.neu.edu/home/matthias/4500-f20/index.html) taught by [Matthias Felleisen](https://felleisen.org/matthias/) at
NEU in 2020 fall semester.

## To users
_TODO_

- How to build
- How to run the server
- How to run a demo client

## To developers
_TODO_

- A road map for the code base
- How to run unit tests
- How to run integration tests
- How to program a client in another language (the JSON communicationp protocol)

## TODO

- wait for the `include_subdirs qualified` feature from dune, so that we can
  have modules in subdirectories referred to like package system in Java.
  [Reference](https://github.com/ocaml/dune/pull/3111)
- Explore [inline
  tests](https://github.com/janestreet/ppx_inline_test/blob/master/example/example.ml),
  which seems to behave like Racket's test modules, and enable hiding certain
  modules, such as `player_list`?
