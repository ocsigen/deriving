(* Copyright Jeremy Yallop 2007.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

(* Extend the OCaml grammar to include the `deriving' clause after
   type declarations in structure and signatures. *)

open Utils

open Camlp4.PreCast

let instantiate _loc t classname =
  try
    let class_ = Base.find classname in
    let module U = Type.Untranslate(struct let _loc = _loc end) in
    let binding = Ast.TyDcl (_loc, "inline", [], t, []) in
    let decls = Base.display_errors _loc Type.Translate.decls binding in
    if List.exists Type.contains_tvars_decl decls then
      Base.fatal_error _loc ("deriving: type variables cannot be used in `method' instantiations");
    let tdecls = List.map U.decl decls in
    let m = Base.derive_str _loc decls class_ in
    <:module_expr< struct
      type $list:tdecls$
	$m$
      include $uid:classname ^ "_inline"$
    end >>
  with Base.NoSuchClass classname ->
    Base.fatal_error _loc ("deriving: " ^ classname ^ " is not a known `class'")

module Deriving (S : Camlp4.Sig.Camlp4Syntax) = struct

  include Syntax

  let rec drop n l =
     if n <= 0 then
       l
     else
       match l with
       | [] -> []
       | _ :: l -> drop (n - 1) l

  let test_val_longident_dot_lt =
    Gram.Entry.of_parser "test_val_longident_dot_lt" (fun strm ->
      let rec test_longident_dot pos tokens =
        match tokens with
        | (ANTIQUOT ((""|"id"|"anti"|"list"), _), _) :: tokens ->
          test_longident_dot (pos+1) tokens
        | (UIDENT _, _) :: (KEYWORD ".", _) :: (LIDENT _, _) :: tokens ->
          test_longident_dot (pos+3) tokens
        | _ :: _ ->
          test_delim pos tokens
        | [] -> fetch_more test_longident_dot pos
      and test_delim pos tokens =
        if pos = 0 then
          raise Stream.Failure
        else
          match tokens with
          | (KEYWORD ("<"), _) :: _ -> ()
          | _ :: _ -> raise Stream.Failure
          | [] -> fetch_more test_delim pos
      and fetch_more k pos =
        match drop pos (Stream.npeek (pos + 10) strm) with
        | [] -> raise Stream.Failure
        | tokens -> k pos tokens
      in fetch_more test_longident_dot 0
    )

  open Ast

  EXTEND Gram
  expr: LEVEL "simple"
  [
  [ TRY[ test_val_longident_dot_lt; e1 = val_longident ; "<" ; t = ctyp; ">" ->
     match e1 with
       | <:ident< $uid:classname$ . $lid:methodname$ >> ->
	   let m = instantiate _loc t classname in
	   <:expr< let module $uid:classname$ = $m$
                   in $uid:classname$.$lid:methodname$ >>
       | _ ->
           Base.fatal_error _loc ("deriving: this looks a bit like a method application, but "
                            ^"the syntax is not valid");
  ]]];

  module_expr: LEVEL "simple"
  [
  [ TRY[ test_val_longident_dot_lt; e1 = val_longident ; "<" ; t = ctyp; ">" ->
     match e1 with
       | <:ident< $uid:classname$ >> ->
	   instantiate _loc t classname
       | _ ->
           Base.fatal_error _loc ("deriving: this looks a bit like a class instantiation, but "
                            ^"the syntax is not valid");
  ]]];
  END

end

module M = Camlp4.Register.OCamlSyntaxExtension(Id)(Deriving)
