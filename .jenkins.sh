
opam pin add --no-action deriving .
opam install type_conv
opam install --deps-only deriving
opam install --verbose deriving

do_build_doc () {
  make wikidoc
  cp -Rf doc/manual-wiki/*.wiki ${MANUAL_SRC_DIR}
  cp -Rf _build/deriving-api.wikidocdir/*.wiki ${API_DIR}
}

do_remove () {
  opam remove --verbose deriving
}
