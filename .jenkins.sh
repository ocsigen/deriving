
opam pin add --no-action deriving .
opam install type_conv
opam install --deps-only deriving
opam install --verbose deriving

do_build_doc () {

}

do_remove () {
  opam remove --verbose deriving
}
