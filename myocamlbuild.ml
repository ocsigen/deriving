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

           (* Use an introduction page with categories *)
           tag_file "deriving-api.docdir/index.html" ["apiref"];
           dep ["apiref"] ["doc/apiref-intro"];
           flag ["apiref"] & S[A "-intro"; P "doc/apiref-intro"; A"-colorize-code"];

       | _ -> ())



(* Compile the wiki version of the Ocamldoc.

   Thanks to Till Varoquaux on usenet:
   http://www.digipedia.pl/usenet/thread/14273/231/

*)

let ocamldoc_wiki tags deps docout docdir =
  let tags = tags -- "extension:html" in
  Ocamlbuild_pack.Ocaml_tools.ocamldoc_l_dir tags deps docout docdir

let () =
  try
    let wikidoc_dir =
      let base = Ocamlbuild_pack.My_unix.run_and_read "ocamlfind query wikidoc" in
      String.sub base 0 (String.length base - 1)
    in

    Ocamlbuild_pack.Rule.rule
      "ocamldoc: document ocaml project odocl & *odoc -> wikidocdir"
      ~insert:`top
      ~prod:"%.wikidocdir/index.wiki"
      ~stamp:"%.wikidocdir/wiki.stamp"
      ~dep:"%.odocl"
      (Ocamlbuild_pack.Ocaml_tools.document_ocaml_project
         ~ocamldoc:ocamldoc_wiki
         "%.odocl" "%.wikidocdir/index.wiki" "%.wikidocdir");

    tag_file "deriving-api.wikidocdir/index.wiki" ["apiref";"wikidoc"];
    flag ["wikidoc"] & S[A"-i";A wikidoc_dir;A"-g";A"odoc_wiki.cma"]

  with Failure e -> () (* Silently fail if the package wikidoc isn't available *)
