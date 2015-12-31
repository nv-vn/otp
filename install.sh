#!/bin/bash
if [ -n `ocamlfind query ppx_json_types` ]; then
  echo 'Removing previously installed versions of `ppx_json_types`'
  ocamlfind remove ppx_json_types
fi
ocamlfind install ppx_json_types ./src/json_types/META
ocamlfind install -add ppx_json_types ./json_types.native
