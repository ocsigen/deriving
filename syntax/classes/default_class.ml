open Pa_deriving_common
open Utils

module Description : Defs.ClassDescription = struct
  let classname = "Default"
  let default_module = None
  let runtimename = "Deriving_Default"
  let alpha = None
  let allow_private = true
  let predefs = [
    ["int"      ], ["Deriving_Default";"int"];
    ["bool"     ], ["Deriving_Default";"bool"];
    ["unit"     ], ["Deriving_Default";"unit"];
    ["char"     ], ["Deriving_Default";"char"];
    ["int32"    ], ["Deriving_Default";"int32"];
    ["Int32";"t"], ["Deriving_Default";"int32"];
    ["int64"    ], ["Deriving_Default";"int64"];
    ["Int64";"t"], ["Deriving_Default";"int64"];
    ["nativeint"], ["Deriving_Default";"nativeint"];
    ["float"    ], ["Deriving_Default";"float"];
    ["string"   ], ["Deriving_Default";"string"];
    ["list"     ], ["Deriving_Default";"list"];
    ["ref"      ], ["Deriving_Default";"ref"];
    ["option"   ], ["Deriving_Default";"option"];
    ["array"    ], ["Deriving_Default";"array"];
  ]
  let depends = []
end

module Builder(Generator : Defs.Generator) = struct

  open Generator.Loc
  open Camlp4.PreCast
  open Description

  module Helpers = Generator.AstHelpers

  let wrap expr = [ <:str_item< value default () = $expr$ >> ]


  let generator = (object (self)

  inherit Generator.generator

    method proxy unit =
      None, [ <:ident< default >> ]

    method tuple ctxt args  =
      let l : Ast.expr list = List.map (fun ty -> <:expr<$self#call_expr ctxt ty "default"$ () >>) args
      in
      wrap (Helpers.tuple_expr l)

    method case ctxt (name, args) =
      match args with
        | [] -> <:expr< $uid:name$ >>
        | _ ->
          let tuple = List.map (fun ty -> <:expr<$self#call_expr ctxt ty "default"$ () >>) args in
          <:expr< $uid:name$ $Helpers.tuple_expr tuple$ >>

    method sum ?eq ctxt tname params constraints summands =
      wrap (self#case ctxt (List.hd summands))

    method record ?eq ctxt tname params constraints fields =
      let contents = List.map (fun (name, (_,ty), _) ->
          name,
          <:expr< $ self # call_expr ctxt ty "default"$ ()>> ) fields in
      wrap (Helpers.record_expr contents)

    method polycase ctxt = function
      | Type.Tag (name, []) ->
        <:expr< `$name$ >>
      | Type.Tag (name, [ty]) ->
        let c = self#call_expr ctxt ty "default" in
        <:expr<`$name$ ($c$ ()) >>
      | Type.Tag (name, tys) ->
        let ty = `Tuple tys in
        let c = self#call_expr ctxt ty "default" in
        <:expr<`$name$ ($c$ ()) >>
      | Type.Extends t  -> <:expr< assert false >>

    method variant ctxt tname params constraints (_,tags) =
      wrap (self#polycase ctxt (List.hd tags))

  end :> Generator.generator)

  let classname = Description.classname
  let runtimename = Description.runtimename
  let generate = Generator.generate generator
  let generate_sigs = Generator.generate_sigs generator
  let generate_expr = Generator.generate_expr generator

end

include Base.RegisterFullClass(Description)(Builder)
