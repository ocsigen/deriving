Deriving (was Deriving-ocsigen)
===============================

This release of deriving is based on the library by Jeremy Yallop. See:

 * http://code.google.com/p/deriving/

Compared to the original library, it adds:

 * META file for ocamlfind compatibility
 * a type-conv compatibility mode
 * the generated code do not rely on recursive modules (this allows compatibility with js_of_ocaml)
 * minimalistic support of GADT

See CHANGES for more details.

Requirements:
-------------

 * ocaml and camlp4 (>= 3.12)
 * optcomp
 * type-conv (optionnal)

Build intructions:
------------------

```
 $ ./configure [--disable-tc]
 $ make

 # make install
```

Documention and examples of the original library:
-------------------------------------------------

 * http://code.google.com/p/deriving/wiki/Introduction
 * http://code.google.com/p/deriving/wiki/Classes

Examples:
---------

```
 $ ocaml
        Objective Caml version 4.01.0

 # #use "topfind";;
 - : unit = ()
 # #camlp4o;;
	Camlp4 Parsing version 4.01.0

 # #require "deriving";;
 # type t = A of int | B of t deriving (Show);;
 type t = A of int | B of t
 module rec Show_t : sig ... end
 # Show.show<t> (B (A 4));;
 - : string = "B A 4"
```

Examples with type-conv:
------------------------

```
 $ ocaml
        Objective Caml version 4.01.0@

 # #use "topfind";;
 - : unit = ()
 # #camlp4o;;
	Camlp4 Parsing version 4.01.0

 # #require "deriving.tc";;
 # type t = A of int | B of t with show;;
 type t = A of int | B of t
 module rec Show_t : sig ... end
```
