(* Copyright Jeremy Yallop 2007.
   Copyright Grégoire Henry 2011.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

open Utils
open Type
open Defs
open Camlp4.PreCast

exception Underivable of string
exception NoSuchClass of string

let fatal_error loc msg =
  Syntax.print_warning loc msg;
  exit 1

let display_errors loc f p =
  try
    f p
  with
    Underivable msg | Failure msg -> fatal_error loc msg

(** *)

let instantiate, instantiate_repr =
  let o lookup = object
    inherit transform as super
    method expr = function
    | `Param (name, _) -> lookup name
    | `GParam ((name, _), e) ->
         if not (contains_tvars e) then e else lookup name
    | e                -> super # expr e
  end in
  (fun (lookup : name -> expr) -> (o lookup)#expr),
  (fun (lookup : name -> expr) -> (o lookup)#repr)

let instantiate_modargs, instantiate_modargs_repr =
  let lookup argmap var =
    try `Constr (NameMap.find var argmap @ ["a"], [])
    with NameMap.Not_found _ ->
      `Param (var, None)
  in (fun argmap -> instantiate (lookup argmap)),
  (fun argmap -> instantiate_repr (lookup argmap))

module AstHelpers(Loc : Loc) = struct

  open Loc

  module Loc = Loc
  module Untranslate = Type.Untranslate(Loc)

  (** Expression sequences *)

  let seq l r = <:expr< $l$ ; $r$ >>
  let rec seq_list = function
    | [] -> <:expr< () >>
    | [e] -> e
    | e::es -> seq e (seq_list es)


  (** Record *)

  let record_pattern ?(prefix="") (fields : Type.field list) : Ast.patt =
    <:patt<{$list:
              (List.map (fun (label,_,_) -> <:patt< $lid:label$ = $lid:prefix ^ label$ >>)
                      fields) $}>>

  let record_expr : (string * Ast.expr) list -> Ast.expr =
    fun fields ->
      let fs =
      List.fold_left1
        (fun l r -> <:rec_binding< $l$ ; $r$ >>)
        (List.map (fun (label, exp) -> <:rec_binding< $lid:label$ = $exp$ >>)
           fields) in
        Ast.ExRec (_loc, fs, Ast.ExNil _loc)

  let record_expression ?(prefix="") : Type.field list -> Ast.expr =
    fun fields ->
      let es = List.fold_left1
        (fun l r -> <:rec_binding< $l$ ; $r$ >>)
        (List.map (fun (label,_,_) -> <:rec_binding< $lid:label$ = $lid:prefix ^ label$ >>)
           fields) in
        Ast.ExRec (_loc, es, Ast.ExNil _loc)


  (** Record *)

  let expr_list : Ast.expr list -> Ast.expr =
      (fun exprs ->
         List.fold_right
           (fun car cdr -> <:expr< $car$ :: $cdr$ >>)
           exprs
         <:expr< [] >>)

  let patt_list : Ast.patt list -> Ast.patt =
      (fun patts ->
         List.fold_right
           (fun car cdr -> <:patt< $car$ :: $cdr$ >>)
           patts
         <:patt< [] >>)


  (** Tuple *)

  let tuple_expr : Ast.expr list -> Ast.expr = function
    | [] -> <:expr< () >>
    | [x] -> x
    | x::xs -> Ast.ExTup (_loc, List.fold_left (fun e t -> Ast.ExCom (_loc, e,t)) x xs)

  let tuple_patt : Ast.patt list -> Ast.patt = function
    | [] -> <:patt< () >>
    | [x] -> x
    | x::xs -> Ast.PaTup (_loc, List.fold_left (fun e t -> Ast.PaCom (_loc, e,t)) x xs)

  let tuple ?(param="v") n : string list * Ast.patt * Ast.expr =
    let v n = Printf.sprintf "%s%d" param n in
      match n with
        | 0 -> [], <:patt< () >>, <:expr< () >>
        | 1 -> [v 0], <:patt< $lid:v 0$ >>, <:expr< $lid:v 0$ >>
        | n ->
            let patts, exprs =
              (* At time of writing I haven't managed to write anything
                 using quotations that generates an n-tuple *)
              List.fold_left
                (fun (p, e) (patt, expr) -> Ast.PaCom (_loc, p, patt), Ast.ExCom (_loc, e, expr))
                (<:patt< >>, <:expr< >>)
                (List.map (fun n -> <:patt< $lid:v n$ >>, <:expr< $lid:v n $ >>)
                   (List.range 0 n))
            in
              List.map v (List.range 0 n), Ast.PaTup (_loc, patts), Ast.ExTup (_loc, exprs)

  (** *)

  let rec lident qname =
    match qname with
      | [] -> invalid_arg "ident"
      | [t] -> <:ident< $lid:t$ >>
      | t::ts -> <:ident< $uid:t$.$lident ts$ >>

  let cast_pattern argmap ?(param="x") ty =
    match ty with
    | `Constr (id, _) ->
      let ty = Untranslate.expr (instantiate_modargs argmap ty) in
      (<:patt< #$id:lident id$ as $lid:param$ >>,
       <:expr< >>,
       <:expr< $lid:param$ >>)
    | ty ->
    let ty = Untranslate.expr (instantiate_modargs argmap ty) in
    (<:patt< $lid:param$ >>,
     <:expr<
       let module M =
           struct
             type $Ast.TyDcl (_loc, "t", [], ty, [])$
             let test = function #t -> true | _ -> false
           end in M.test $lid:param$ >>,
     <:expr<
       (let module M =
            struct
              type $Ast.TyDcl (_loc, "t", [], ty, [])$
              let cast = function #t as t -> t | _ -> assert false
            end in M.cast $lid:param$ )>>)

  (**  *)

  let atype_expr argmap ty =
    let ty = instantiate_modargs argmap ty in
    match ty with
    | `Constr(["a"],_) ->
	raise (Underivable ("deriving: types called `a' are not allowed.\n"
			    ^ "Please change the name of your type and try again."));
    | ty -> Untranslate.expr ty

  let rec modname_from_qname ~qname ~classname =
    match qname with
      | [] -> invalid_arg "modname_from_qname"
      | [t] -> <:ident< $uid:classname ^ "_"^ t$ >>
      | t::ts -> <:ident< $uid:t$.$modname_from_qname ~qname:ts ~classname$ >>

  let mproject mexpr (name:string) =
    match mexpr with
      | <:module_expr< $id:m$ >> -> <:expr< $id:m$.$lid:name$ >>
      | _ -> <:expr< let module M = $mexpr$ in M.$lid:name$ >>

  let mProject mexpr name =
    match mexpr with
      | <:module_expr< $uid:m$ >> -> <:module_expr< $uid:m$.$uid:name$ >>
      | _ -> <:module_expr< struct module M = $mexpr$ include M.$uid:name$ end >>


end

module type InnerClassDescription = sig
  include ClassDescription
  val find_predefined: Type.qname -> Type.qname
  val depends: (module DepClassBuilder) list
end

module InnerGenerator(Loc: Loc)(Desc : InnerClassDescription) = struct

  (** How does it works ?

      For each type declaration, we generate a functor taking as
      parameters the class instances for the type parameters.

      For (mutually) recursive type declaration(s), we compute the
      (finite) set of required recursive class instances (see
      "cluster.mli") and generate a functor containing all these
      class  instances. Then we generate a non-recursive functor for
      each type declaration.

      For the set of recursive class instances we use "lazy
      first-order module" instead of "recursive modules" to be
      compatible with 'js_of_ocaml' (that do not allow recursive
      modules). E.g. for two mutually recursive type 'a t and 'a t2:

      module Show_RandomId(M_a: Show) = struct
        let rec make_t =
          lazy (module struct ... end : Show with type a = M_a.a t)
        and make_t2 =
          lazy (module struct ... end : Show with type a = M_a.a t2)
      end

      module Show_t(M_a: Show) = struct
        module Show_RandomId = Show_RandomId(M_a)
        type a = M_a.a t
        let show =
          let module M = (val Lazy.force Show_RandomId.make_t) in
          M.show
        ...
      end
      module Show_t2(M_a: Show) =
        module Show_RandomId = Show_RandomId(M_a)
        type a = M_a.a t2
        let show =
          let module M = (val Lazy.force Show_RandomId.make_t2) in
          M.show
        ...
      end

  *)

  module Loc = Loc
  module AstHelpers = AstHelpers(Loc)
  module Helpers = AstHelpers
  module Untranslate = Helpers.Untranslate

  open Loc

  type context = {
    (* Maps type expression to name of a module's value inside the
       cluster's functor. *)
    mod_insts : Ast.module_expr Type.EMap.t;
    (* Maps name of type's parameter name to module name of functor's
       parameters *)
    argmap : Type.qname Type.NameMap.t;
  }

  let make_argmap params =
    List.fold_left
      (fun params (name, _) -> NameMap.add name (["M_" ^ name]) params)
      NameMap.empty
      params

  let cast_pattern ctxt ?param ty =
    Helpers.cast_pattern ctxt.argmap ?param ty
  let instantiate_modargs_repr ctxt = instantiate_modargs_repr ctxt.argmap
  let instantiate_gparam e =
    let map = object (self)
      inherit Type.transform as super
      method expr e = match e with
      | `GParam (p, e) ->
          if Type.contains_tvars e then
            `Param p
          else
            e
      | e -> super#expr e
    end in
    map#expr e

  let import_depend ctxt ty depend =
    let module Dep = (val depend : DepClassBuilder)(Loc) in
    let argmap =
      NameMap.map (fun qname -> qname @ [Dep.classname]) ctxt.argmap
    and mod_insts =
     EMap.map (fun m -> Helpers.mProject m Dep.classname) ctxt.mod_insts
    in
    let mod_insts = match ty with
      | `Constr ([tname],params) ->
	  EMap.remove (tname,params) mod_insts
      | _ -> mod_insts
	  in
    <:str_item<
      module $uid:Dep.classname$ =
	$Dep.generate_expr mod_insts argmap ty$
    >>

  let import_depends ctxt ty =
    List.map (import_depend ctxt ty) Desc.depends

  class virtual generator = object (self)

    (* *)

    method call_expr ctxt ty name = Helpers.mproject (self#expr ctxt ty) name

    method call_poly_expr ctxt (params, ty : Type.poly_expr) name =
      match Desc.alpha with
      | None when params <> [] ->
	  raise (Underivable
		 (Desc.classname ^ " cannot be derived for record types "
		  ^ "with polymorphic fields"))
      | None -> self#call_expr ctxt ty name
      | Some mod_name ->
	let ctxt =
	  { ctxt with
	    argmap = List.fold_left
	      (fun argmap (pname, _) -> NameMap.add pname ["M_"^pname] argmap)
	      ctxt.argmap params}
	in
	let expr = self#call_expr ctxt ty name in
	List.fold_right
	  (fun (pname,_) expr ->
	    (* This is not a function... much more a scope for a type variable... *)
	    <:expr< fun (type t) ->
	              let module $uid:"M_"^pname$ =
			    $uid:Desc.runtimename$.$uid:mod_name$(struct type a = t end) in
		      $expr$ >>)
	  params
	  expr

    (* *)

    method class_sig argmap ty =
      <:module_type<
	$uid:Desc.runtimename$.$uid:Desc.classname$
          with type a = $Helpers.atype_expr argmap ty$
      >>

    method pack argmap ty m =
      match m with
      | <:module_expr< (val $e$) >> -> e
      | _ ->
	  (* <:expr< (module $m$ : $class_sig ctxt decl) >> *)
	  Ast.ExPkg (_loc, (Ast.MeTyc (_loc, m, self#class_sig argmap ty)))

    method unpack argmap ty e =
      match e with
      | <:expr< (module $m$) >> -> m
      | _ ->
	  (* (val $e$ : $class_sig gen argmap decl$) *)
	  Ast.MePkg (_loc,
		     Ast.ExTyc (_loc, e,
				Ast.TyPkg (_loc, self#class_sig argmap ty)))

    (** *)

    method wrap ctxt ?(default = Desc.default_module) ty items =
      let mexpr =
	<:module_expr< struct
	  type a = ($Helpers.atype_expr ctxt.argmap ty$)
	  $list:import_depends ctxt ty$
	  $list:items$
	end >> in
      match default with
      | None -> mexpr
      | Some name ->
	  <:module_expr< $uid:Desc.runtimename$.$uid:name$($mexpr$) >>

    (** *)

    method expr ctxt (ty: Type.expr) =
      match ty with
      | `Param p    ->                   (self#param      ctxt p)
      | `GParam p   ->                   (self#gparam     ctxt p)
      | `Object o   -> self#wrap ctxt ty (self#object_    ctxt o)
      | `Class c    -> self#wrap ctxt ty (self#class_     ctxt c)
      | `Label l    -> self#wrap ctxt ty (self#label      ctxt l)
      | `Function f -> self#wrap ctxt ty (self#function_  ctxt f)
      | `Constr c   ->                   (self#constr     ctxt c)
      | `Tuple t    -> self#wrap ctxt ty (self#tuple      ctxt t)

    method rhs ctxt subst (tname, params, rhs, constraints, _ : Type.decl) =

      let params =
	List.map (substitute_expr subst) (List.map (fun p -> `Param p) params)
      in

      let ty = `Constr([tname], params) in
      let rhs = substitute_rhs subst rhs in
      match rhs with
        | `Fresh (_, _, `Private) when not Desc.allow_private ->
            raise (Underivable ("The class " ^ Desc.classname
				^ " cannot be derived for private types"))
        | `Fresh (eq, Sum summands, _) ->
	    self#wrap ctxt ty (self#sum ?eq ctxt tname params constraints summands)
        | `Fresh (eq, GSum (tname', summands), _) ->
	    self#wrap ctxt ty (self#gsum ?eq ctxt tname params constraints summands)
        | `Fresh (eq, Record fields, _) ->
	    self#wrap ctxt ty (self#record ?eq ctxt tname params constraints fields)
        | `Expr e -> self#expr ctxt e
        | `Variant (v,p) ->
	    self#wrap ctxt ty (self#variant ctxt tname params constraints v)
        | `Nothing -> <:module_expr< >>

    method param ctxt (name, _) =
      <:module_expr< $id:Untranslate.qName (NameMap.find name ctxt.argmap)$ >>

    method gparam ctxt (p, e) =
      if Type.contains_tvars e then self#param ctxt p else self#expr ctxt e

    method constr ctxt (qname, params) =
      match qname with
      | [tname] when EMap.mem (tname,params) ctxt.mod_insts ->
	  (* Instance in the current cluster. *)
	  EMap.find (tname, params) ctxt.mod_insts
      | _ ->
	(* External module: apply classical functor. *)
        let qname =
	  try Desc.find_predefined qname
	  with Not_found -> qname in
	List.fold_left
	  (fun m p -> <:module_expr< $m$ ($self#expr ctxt p$) >>)
	  <:module_expr<
	      $id:Helpers.modname_from_qname
	            ~qname ~classname:Desc.classname$ >>
	  params

    method virtual proxy: unit -> Type.name option * Ast.ident list

    (* *)

    method virtual variant:
	context ->
	  Type.name -> Type.expr list -> Type.constraint_ list ->
	    variant -> Ast.str_item list
    method virtual sum:
	?eq:expr -> context ->
	  Type.name -> Type.expr list -> Type.constraint_ list ->
	    summand list -> Ast.str_item list
    method virtual record:
	?eq:expr -> context ->
	  Type.name -> Type.expr list -> Type.constraint_ list ->
	    field list -> Ast.str_item list
    method virtual tuple: context -> expr list -> Ast.str_item list

    method gsum ?eq ctxt tname params constraints gsummands =
      raise (Underivable (Desc.classname ^ " cannot be derived for GADT"))
    method object_   _ o =
      raise (Underivable (Desc.classname ^ " cannot be derived for object types"))
    method class_    _ c =
      raise (Underivable (Desc.classname ^ " cannot be derived for class types"))
    method label     _ l =
      raise (Underivable (Desc.classname ^ " cannot be derived for label types"))
    method function_ _ f =
      raise (Underivable (Desc.classname ^ " cannot be derived for function types"))

  end

  let add_functor_param argmap (pname,_) body =
    match NameMap.find pname argmap with
    | [name] ->
	<:module_expr<
	  functor ( $uid:name$
		      : $uid:Desc.runtimename$.$uid:Desc.classname$)
	    -> $body$ >>
    | _ -> assert false

  let add_functor_param_sig argmap (pname,_) body =
    match NameMap.find pname argmap with
    | [name] ->
	<:module_type<
	  functor ( $uid:name$
		      : $uid:Desc.runtimename$.$uid:Desc.classname$)
	    -> $body$ >>
    | _ -> assert false

  let create_subst params eparams =
    List.fold_right2 NameMap.add
      (* (fun p ep map -> *)
	 (* match ep with *)
	 (* | `Param (p',_ ) when p' = p -> map *)
	 (* | _ -> NameMap.add p ep map) *)
      (List.map fst params) eparams
      NameMap.empty

  (** ... *)

  let generate (gen : generator) decls =

    let generate_cluster cluster =

      let cluster_name =
	let id = random_id 32 in
	Printf.sprintf "%s_%s" Desc.classname id in

      let fun_names =
	let cpt = ref 0 in
	List.fold_left
	  (fun map (tname, _ as inst) ->
	     incr cpt;
	     EMap.add inst (Printf.sprintf "make_%s_%d" tname !cpt) map)
	  EMap.empty
	  cluster.Clusters.instances in

      let cluster_argmap = make_argmap cluster.Clusters.params in

      let rec wrap_local_types (args : expr list) body =
#if ocaml_version < (4, 00)
        body
#else
        match args with
        | [] -> body
        | `Param (arg, _) :: args
        | `GParam ((arg, _),_) :: args ->
          let id = "deriving_" ^ random_id 8 ^ "_" ^ arg in
            let pat =
              (* (module $M_arg$ : $class_sig ...$ ) *)
              Ast.PaTyc(_loc, Ast.PaMod (_loc, "M_" ^ arg),
                        Ast.TyPkg(_loc,
                                  gen#class_sig NameMap.empty (`Constr ([id], []))))
            in
            wrap_local_types args
              <:expr< (fun (type $lid:id$) -> (function $pat$ -> $body$))
                     (module $uid:"M_"^arg$) >>
        | _ -> assert false
#endif
      in

      let generate_instance (tname, eparams as inst) =
	let mod_insts =
	  EMap.mapi
	    (fun (tname, params) fname ->
	       gen#unpack cluster_argmap (`Constr ([tname], params))
		 (<:expr< Lazy.force $lid:fname$ >>))
	    fun_names
	in
	let ctxt = { argmap = cluster_argmap; mod_insts; } in
	let ty = `Constr([tname], eparams) in
	let (_,params,_,_,_ as decl) =
	  List.find (fun (tn,_,_,_,_) -> tname = tn) decls in
	let subst = create_subst params eparams in
	let body = gen#pack ctxt.argmap ty (gen#rhs ctxt subst decl) in
        let id = EMap.find inst fun_names in
	id, <:expr< lazy (ignore($lid:id$); $body$) >>
      in

      let generate_functor (tname,params,_,_,_ as decl) =
	let argmap = make_argmap params in
	let mod_insts =
	  EMap.mapi
	    (fun (tname, params) fname ->
	       let e =
		 Helpers.mproject
		   (List.fold_left
		      (fun m (pname,_) ->
			 let p = NameMap.find pname argmap in
			 <:module_expr< $m$ ($id:Untranslate.qName p$) >>)
		      (<:module_expr< $uid:cluster_name$ >>)
		      cluster.Clusters.params)
		   fname
	       in
	       let ty = `Constr ([tname], params) in
	       gen#unpack argmap ty (<:expr< Lazy.force $e$ >>))
	    fun_names
	in
	let ctxt = { argmap; mod_insts; } in
	let body =
	  let params = List.map (fun p -> `Param p) params in
	  let ty = `Constr ([tname], params) in
	  try
	    let tfname = EMap.find (tname, params) fun_names in
	    let default, ids = gen#proxy () in
	    let m =
	      List.fold_left
		(fun m p -> <:module_expr< $m$ ($gen#expr ctxt p$) >>)
	      <:module_expr< $uid:cluster_name$ >>
		params in
	    let items =
	      <:str_item< module M = $m$ >> ::
		(let m = Helpers.mproject <:module_expr< M >> tfname in
		 let m = gen#unpack argmap ty <:expr< Lazy.force $m$ >> in
		 List.map
		   (fun id ->
		      <:str_item< let $id:id$ = let module M = $m$ in M.$id:id$ >>)
		   ids)
	    in
	    (gen#wrap ~default ctxt ty items)
	  with EMap.Not_found _ -> gen#rhs ctxt NameMap.empty decl

	in
	let body =
	  let ty = `Constr ([tname], List.map (fun p -> `Param p) params) in
	  <:module_expr< ($body$ : $gen#class_sig argmap ty$) >> in
	let body =
	  List.fold_right
	    (add_functor_param argmap)
	    params
	    body
	in
	<:str_item<
          module $uid:Printf.sprintf "%s_%s" Desc.classname tname$ =
	  $body$
	>>
      in

      let is_gadt = function
        | (_,_,`Fresh(_,GSum _,_),_,_) -> true
        | _ -> false in
      let contains_gadt c = List.exists is_gadt c.Clusters.decls in

      if cluster.Clusters.instances <> [] then
        let inst_exprs = List.map generate_instance cluster.Clusters.instances in
        let bindings =
          List.map (fun (id, e) -> <:binding< $lid:id$ = $e$ >>) inst_exprs in
        let items =
          if not (contains_gadt cluster) then
            <:str_item< let rec $list:bindings$ >>
          else
            let e = List.map (fun (id, _) -> <:expr< $lid:id$ >>) inst_exprs in
            let p = List.map (fun (id, _) -> <:patt< $lid:id$ >>) inst_exprs in
            let body =
              wrap_local_types
                (List.map (fun p -> `Param p) cluster.Clusters.params)
                <:expr< let rec $list:bindings$ in $Helpers.tuple_expr e$ >> in
            <:str_item< let $Helpers.tuple_patt p$ = $body$ >>
        in
	let mod_expr =
	  List.fold_right
	    (add_functor_param cluster_argmap)
	    cluster.Clusters.params
	    <:module_expr< struct $items$ end >>
	in
	<:str_item<
          module $uid:cluster_name$ = $mod_expr$
	  $list:List.map generate_functor cluster.Clusters.decls$
        >>
      else
	<:str_item< $list:List.map generate_functor cluster.Clusters.decls$ >>
    in

    <:str_item< $list:List.map generate_cluster (Clusters.make decls)$ >>

  (** ... *)
  let generate_sigs (gen:generator) decls =

    let generate_sig (tname,params,rhs,_,generated) =
      if generated then
	<:sig_item< >>
      else
	let argmap = make_argmap params in
	let ty =
	  match rhs with
	  | `Fresh _ | `Variant _ | `Nothing ->
	      `Constr ([tname], List.map (fun p -> `Param p) params)
	  | `Expr e -> e
	in
	let body =
	  List.fold_right
	    (add_functor_param_sig argmap)
	    params
	    (gen#class_sig argmap ty)
	in
	<:sig_item<
          module $uid:Printf.sprintf "%s_%s" Desc.classname tname$ : $body$
	>>
    in

    <:sig_item< $list:List.map generate_sig decls$ >>

    let generate_expr (gen:generator) mod_insts argmap ty =
      gen#expr { argmap; mod_insts; } ty

end

let derive_str _loc decls class_builder =
  let module Loc = struct let _loc = _loc end in
  let module Class = (val class_builder : InnerClassBuilder)(Loc) in
  display_errors _loc Class.generate decls

let derive_sig _loc decls class_builder =
  let module Loc = struct let _loc = _loc end in
  let module Class = (val class_builder : InnerClassBuilder)(Loc) in
  display_errors _loc Class.generate_sigs decls

let generators : (string, (module InnerClassBuilder)) Hashtbl.t =
  Hashtbl.create 15
let hashtbl_add desc deriver =
  let module Desc = (val desc : ClassDescription) in
  Hashtbl.add generators Desc.classname deriver
let register_hook = ref [hashtbl_add]
let add_register_hook f = register_hook := f :: !register_hook
let register desc deriver =
  List.iter (fun f -> f desc deriver) !register_hook
let find classname =
  try Hashtbl.find generators classname
  with Not_found -> raise (NoSuchClass classname)
let is_registered classname = Hashtbl.mem generators classname

module MakeInnerDesc(Desc : Defs.ClassDescription) =
struct
  include Desc
  let predefs_tbl : (Type.qname, Type.qname) Hashtbl.t = Hashtbl.create 17
  let find_predefined qname = Hashtbl.find predefs_tbl qname
  let register_predefined ty instance = Hashtbl.add predefs_tbl ty instance
  let () = List.iter (fun (a, b) -> register_predefined a b) predefs
end

module RegisterClass
    (Desc : Defs.ClassDescription)
    (MakeClass : ClassBuilder) = struct

  module InnerDesc = MakeInnerDesc(Desc)
  module Builder(Loc: Loc) = MakeClass(InnerGenerator(Loc)(InnerDesc))

  let _ = register (module Desc : Defs.ClassDescription) (module Builder : InnerClassBuilder)

  let register_predefs = InnerDesc.register_predefined

end

module RegisterFullClass
    (Desc : Defs.ClassDescription)
    (MakeClass : FullBuilder) = struct

  module InnerDesc = MakeInnerDesc(Desc)
  module Builder(Loc: Loc) = MakeClass(InnerGenerator(Loc)(InnerDesc))

  let _ = register (module Desc : Defs.ClassDescription) (module Builder : InnerClassBuilder)

  let depends = (module Builder : DepClassBuilder)
  let register_predefs = InnerDesc.register_predefined

end

(* Compat with <= 0.4-ocsigen *)

module Register
    (Desc : Defs.ClassDescription)
    (MakeClass : InnerClassBuilder) = struct

  let _ = register (module Desc : Defs.ClassDescription) (module MakeClass : InnerClassBuilder)

end

module Generator(Loc : Loc)(Desc : ClassDescription) =
  InnerGenerator(Loc)(MakeInnerDesc(Desc))
