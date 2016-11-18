#!/bin/bash
if [ ! -d "_build/src" ]; then
    mkdir -p _build/src
fi
cp src/urange.ml _build/src
ocamlc -o _build/src/urange.native _build/src/urange.ml
_build/src/urange.native > src/utf8uni.ml
