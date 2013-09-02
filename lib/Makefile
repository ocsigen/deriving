include ../Makefile.config

OCAMLC     := ${OCAMLFIND} ocamlc
OCAMLOPT   := ${OCAMLFIND} ocamlopt
OCAMLDEP   := ${OCAMLFIND} ocamldep
PP         := -package camlp4 -ppopt ../syntax/pa_deriving.cma -syntax camlp4o

OCAMLFLAGS := -w ae

SOURCES = deriving_Show.ml           \
          deriving_interned.ml       \
          deriving_Eq.ml             \
          deriving_Bounded.ml        \
          deriving_Enum.ml           \
          deriving_monad.ml          \
          deriving_Dump.ml           \
          deriving_Typeable.ml       \
          deriving_dynmap.ml         \
          deriving_Pickle.ml	     \
          deriving_Functor.ml        \
          deriving_Default.ml        \

##

all: byte opt

byte: deriving.cma deriving_num.cma
opt:: deriving.cmxa deriving_num.cmxa
ifeq "${NATDYNLINK}" "YES"
opt:: deriving.cmxs deriving_num.cmxs
endif

##

deriving.cma: ${SOURCES:.ml=.cmo}
	${OCAMLC} -a -o $@ $^
deriving.cmxa: ${SOURCES:.ml=.cmx}
	${OCAMLOPT} -a -o $@ $^


deriving_num.cma: deriving_num.cmo
	${OCAMLC} -a -o $@ $^
deriving_num.cmxa: deriving_num.cmx
	${OCAMLOPT} -a -o $@ $^

# Common rules

%.cmi: %.mli
	${OCAMLC} ${OCAMLFLAGS} ${PP} -c $<
%.cmo: %.ml
	${OCAMLC} ${OCAMLFLAGS} ${PP} -c $<
%.cmx: %.ml
	${OCAMLOPT} ${OCAMLFLAGS} ${PP} -c $<
%.cmxs: %.cmxa
	$(OCAMLOPT) -shared -linkall -o $@ $<

# Clean up
clean:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *${OBJEXT} *${LIBEXT} *.annot
distclean: clean
	-rm -f .*.deps
	-rm -f *~ \#* .\#*

# Dependencies

DEPS := $(patsubst %.ml,.%.ml.deps,$(wildcard *.ml)) \
        $(patsubst %.mli,.%.mli.deps,$(wildcard *.mli))

.depend: ${DEPS}
	cat .*.deps > .depend

.SECONDARY: ${DEPS}
.%.ml.deps: %.ml
	${OCAMLDEP} ${PP} $^ > $@
.%.mli.deps: %.mli
	${OCAMLDEP} ${PP} $^ > $@

ifneq (${DEPEND},no)
include .depend
endif
