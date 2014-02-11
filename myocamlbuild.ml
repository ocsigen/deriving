(* OASIS_START *)
(* OASIS_STOP *)

let _ =

  (* FIX START *)
  (* fix needed by ocaml(build) 3.12.1(,4.00.1?) in order to pick the right ocamlfind *)

  (* Fixed in later version with the following commit *)
  (* ocamlbuild should look for ocamlfind on the path not in the root directory *)
  (* https://github.com/ocaml/ocaml/commit/9d51dccfaebb2c3303ae0bb1d4f28fe6f8d10915 *)

  let _ = Ocamlbuild_pack.Ocamlbuild_where.bindir := "/" in
  (* FIX STOP *)

  Ocamlbuild_plugin.dispatch
    (fun hook ->
       dispatch_default hook;
       match hook with
       | After_rules ->
           (* Internal syntax extension *)
           List.iter
             (fun dir ->
                let tag = "use_pa_deriving_" ^ dir and file = "syntax/" ^ dir ^ "/pa_deriving_" ^ dir ^ ".cma" in
                flag ["ocaml"; "compile"; tag] & S[A"-ppopt"; A file];
                flag ["ocaml"; "ocamldep"; tag] & S[A"-ppopt"; A file];
                flag ["ocaml"; "doc"; tag] & S[A"-ppopt"; A file];
                dep ["ocaml"; "ocamldep"; tag] [file])
             ["common"; "std"; "tc"; "classes"];
       | _ -> ())
