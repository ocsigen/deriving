(* Copyright Jeremy Yallop 2007.
   Copyright Grégoire Henry 2011.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

open Utils

(* auxiliary definitions *)
type name = string
type qname = name list
module NameMap = Map.Make(String)
module NameSet = Set.Make(String)

type param = name * [`Plus | `Minus] option

(* no support for private types yet *)
type decl = name * param list * rhs * constraint_ list
    (* whether the type was inserted by deriving *)
    * bool
and rhs = [`Fresh of expr option * repr * [`Private|`Public]
          |`Expr of expr
          |`Variant of variant * [ `Private | `Public ]
          |`Nothing]
and repr =
    Sum of summand list
  | GSum of name * gsummand list
  | Record of field list
and field = name * poly_expr * [`Mutable | `Immutable]
and summand = name * expr list
and gsummand = name * expr list * expr list
and constraint_ = expr * expr
and expr =  (* elements that can be nested *)
    [ `Param of param
    | `GParam of param * expr
    | `Label of ([`Optional|`NonOptional] * name * expr * expr)
    | `Function of (expr * expr)
    | `Constr of (qname * expr list)
    | `Tuple of expr list
    | `Object of [`NYI]
    | `Class of [`NYI] ]
and poly_expr = param list * expr
    (* no support for < > variants yet.
       no support for '&' yet.
    *)
and variant = [`Gt | `Lt | `Eq] * tagspec list
and tagspec = Tag of name * expr list
              | Extends of expr

let rec compare_expr (e1: expr) (e2: expr) = match e1, e2 with
| `Param p1, `Param p2
| `Param p1, `GParam (p2, _)
| `GParam (p1, _), `Param p2
| `GParam (p1, _), `GParam(p2, _) -> compare p1 p2
| _, `Param _ -> -1
| `Param _, _ -> 1
| _, `GParam _ -> -1
| `GParam _, _ -> 1

| `Label (t1, n1, e1, e1'), `Label (t2, n2, e2, e2') ->
   let c = compare t1 t2 in
   if c <> 0 then c else
   let c = compare n1 n2 in
   if c <> 0 then c else
   let c = compare_expr e1 e2 in
   if c <> 0 then c else
   let c = compare_expr e1' e2' in
   c
| _, `Label _ -> -1
| `Label _, _ -> 1

| `Function (e1, e1'), `Function (e2, e2') ->
   let c = compare_expr e1 e2 in
   if c <> 0 then c else
   let c = compare_expr e1' e2' in
   c
| `Function _, _ -> -1
| _, `Function _ -> 1

| `Constr (n1, es1), `Constr (n2, es2) ->
   let c = compare n1 n2 in
   if c <> 0 then c else
   let c = compare_expr_list es1 es2 in
   c
| `Constr _, _ -> -1
| _, `Constr _ -> 1

| `Tuple es1, `Tuple es2 -> compare_expr_list es1 es2
| _, `Tuple _ -> -1
| `Tuple _, _ -> 1

| `Object _, `Object _ -> 0
| `Object _, _ -> -1
| _, `Object _ -> 1

| `Class _, `Class _ -> 0

and compare_expr_list es1 es2 = match es1, es2 with
| [],  [] -> 0
| _, [] -> -1
| [], _ -> 1
| e1 :: es1, e2 :: es2 ->
   let c = compare_expr e1 e2 in
   if c <> 0 then c else
   let c = compare_expr_list es1 es2 in
   c


module Param = struct type t = param let compare = compare end
module ParamSet = Set.Make(Param)
module ParamMap = Map.Make(Param)

module Expr = struct type t = expr let compare = compare_expr end
module ExprSet = Set.Make(Expr)
module ExprMap = Map.Make(Expr)

module E = struct
  type t = name * expr list
  let compare (n1, es1) (n2, es2) =
    let c = compare n1 n2 in
    if c <> 0 then c else
    let c = compare_expr_list es1 es2 in
    c
end
module ESet = Set.Make(E)
module EMap = Map.Make(E)


class virtual ['result] fold =
object (self : 'self)
  method virtual crush : 'result list -> 'result

  method decl (d:decl) =
    self#crush (match d with
                  | (_, _, rhs, cs,_) ->
                      self#rhs rhs :: List.map self#constraint_ cs)

  method rhs (r:rhs) =
    self#crush (match r with
                  | `Fresh (Some e, r, _) -> [self#expr e; self#repr r]
                  | `Fresh (None, r, _)   -> [self#repr r]
                  | `Expr e               -> [self#expr e]
                  | `Variant (v,_)        -> [self#variant v]
                  | `Nothing              -> [])


  method repr r =
    self#crush (match r with
                    | Sum summands ->
                        List.map self#summand summands
                    | GSum (_, summands) ->
                        List.map self#gsummand summands
                    | Record fields ->
                        List.map self#field fields)

  method field (name, pexpr, flag) =
    self#crush [self#poly_expr pexpr]

  method summand (_,es) =
    self#crush (List.map self#expr es)

  method gsummand (_,es1,es2) =
    self#crush (List.map self#expr (es1 @ es2))

  method constraint_ (e1,e2) =
    self#crush [self#expr e1; self#expr e2]

  method expr e =
    self#crush (match e with
                    `Param _
                  | `GParam (_, _)
                  | `Object _
                  | `Class _ -> []
                  | `Label (_, _, e1, e2)
                  | `Function (e1, e2) -> [self#expr e1; self#expr e2]
                  | `Constr (_, exprs)
                  | `Tuple exprs  -> List.map self#expr exprs)

  method poly_expr (params,e) =
    self#crush [self#expr e]

  method variant (_,tagspecs) =
    self#crush (List.map self#tagspec tagspecs)

  method tagspec t =
    self#crush (match t with
                  | Tag (_, exprs) -> List.map self#expr exprs
                  | Extends e -> [self#expr e])
end

class transform =
object (self : 'self)

  method decl (name, params, rhs, constraints,g:decl) : decl =
    (name, params, self#rhs rhs, List.map (self # constraint_) constraints, g)

  method rhs = function
    | `Fresh (eopt, repr, p) -> `Fresh (Option.map (self # expr) eopt,
                                        self # repr repr, p)
    | `Expr e -> `Expr (self # expr e)
    | `Variant (v,p) -> `Variant (self # variant v, p)
    | `Nothing -> `Nothing

  method repr = function
    | Sum summands -> Sum (List.map (self # summand) summands)
    | GSum (name, summands) -> GSum (name, List.map (self # gsummand) summands)
    | Record fields -> Record (List.map (self # field) fields)

  method field (name, poly_expr, flag) =
    (name, self # poly_expr poly_expr, flag)

  method summand (name, exprs) =
    (name, List.map (self # expr) exprs)

  method gsummand (name, exprs, params) =
    (name, List.map (self # expr) exprs, List.map (self # expr) params)

  method constraint_ (e1, e2) =
    (self#expr e1, self#expr e2)

  method expr = function
    | `Object _
    | `Class _
    | `Param _
    | `GParam _ as e -> e
    | `Label (flag, name, e1, e2) -> `Label (flag, name, self # expr e1, self # expr e2)
    | `Function (e1, e2) -> `Function (self # expr e1, self # expr e2)
    | `Constr (qname, exprs) -> `Constr (qname, List.map (self # expr) exprs)
    | `Tuple exprs -> `Tuple (List.map self # expr exprs)

  method poly_expr (params, expr)
    = (params, self # expr expr)

  method variant (t, tagspecs)
    = (t, List.map (self # tagspec) tagspecs)

  method tagspec = function
    | Tag (name, exprs) -> Tag (name, List.map self # expr exprs)
    | Extends e -> Extends (self # expr e)
end

module Translate =
struct
  open Camlp4.PreCast

  let anon_param =
    let id = ref 0 in
    fun () -> incr id; "_" ^ string_of_int !id

  let param = function
    | Ast.TyQuP (loc, name) -> name, Some `Plus
    | Ast.TyQuM (loc, name) -> name, Some `Minus
    | Ast.TyQuo (loc, name)  -> name, None
#if ocaml_version >= (4, 00)
    | Ast.TyAnP _ -> anon_param (), Some `Plus
    | Ast.TyAnM _ -> anon_param (), Some `Minus
    | Ast.TyAny _ -> anon_param (), None
#endif
    | _ -> assert false

  let params = List.map param

  let split_and = function
    | Ast.TyAnd (_,l,r) -> Left (l,r)
    | t -> Right t

  let split_comma = function
    | Ast.TyCom (_,l,r) -> Left (l,r)
    | t -> Right t

  let split_semi = function
    | Ast.TySem (_,l,r) -> Left (l,r)
    | t -> Right t

  let split_or = function
    | Ast.TyOr (_,l,r) -> Left (l,r)
    | t -> Right t

  let split_amp = function
    | Ast.TyAmp (_,l,r) -> Left (l,r)
    | t -> Right t

  let split_ofamp = function
    | Ast.TyOfAmp (_,l,r) -> Left (l,r)
    | t -> Right t

  let split_star = function
    | Ast.TySta (_,l,r) -> Left (l,r)
    | t -> Right t

  let list (one : Ast.ctyp -> 'a) (split : Ast.ctyp -> (Ast.ctyp * Ast.ctyp, Ast.ctyp) either) : Ast.ctyp -> 'a list =
    let rec aux = function
      | Ast.TyNil _ -> []
      | ctyp ->
          match split ctyp with
            | Left (l,r) -> aux l @ aux r
            | Right item -> [one item]
    in aux

  let ident : Ast.ident -> name = function
    | Ast.IdAcc _
    | Ast.IdAnt _
    | Ast.IdApp _ -> assert false
    | Ast.IdLid (_, i)
    | Ast.IdUid (_, i) -> i

  let rec qident : Ast.ident -> qname = function
    | Ast.IdAcc (_,l,r) -> qident l @ qident r
    | Ast.IdAnt _
    | Ast.IdApp _ -> assert false
    | Ast.IdLid _
    | Ast.IdUid _ as i -> [ident i]

  type vmap = (name * variant * name option) list

  let fresh_name, set_name_prefix
    =
    let name_prefix = ref "" in
    let counter = ref 0 in
      ((fun () ->
        incr counter;
          "deriving_" ^ !name_prefix ^ "_" ^ string_of_int !counter),
       (fun name -> name_prefix := name; counter := 0))

  module WithParams(P : sig val params : param list end) =
  struct
    include P

    let apply_t name =
      `Constr([name], List.map (fun p -> `Param p) params)

    let rec expr : Ast.ctyp -> expr * vmap  = function
      | Ast.TyObj _ -> `Object `NYI, []
      | Ast.TyCls _ -> `Class `NYI, []
      | Ast.TyQuP (_,_)
      | Ast.TyQuM (_,_)
      | Ast.TyQuo (_,_) as p -> `Param (param p), []
      | Ast.TySum _
      | Ast.TyRec _ -> failwith "deriving: top level element found nested"
      | Ast.TyAny _ -> failwith "deriving does not support `_' in type definitions"
      | Ast.TyArr (_,f,t) ->
          let f, v1 = expr f and t,v2 = expr t in
            `Function (f, t), v1 @ v2
      | Ast.TyApp _ as app -> let app, v = application app in `Constr app, v
      | Ast.TyId (_, i) -> `Constr (qident i, []), []
      | Ast.TyTup (_, t) -> let es, vs = List.split (list expr split_star t) in `Tuple es, List.concat vs
      | Ast.TyVrnEq  (_, t) -> variant t `Eq
      | Ast.TyVrnSup (_, t) -> variant t `Gt
      | Ast.TyVrnInf (_, t) -> variant t `Lt
      | Ast.TyAli (_, _, Ast.TyQuo (_,name)) when List.mem_assoc name params ->
          failwith ("Alias names must be distinct from parameter names for "
                    ^"\nderived types, but '"^name^" is both an alias and a parameter")
      | Ast.TyAli (_, Ast.TyVrnEq  (_, t), Ast.TyQuo (_,name)) -> variant t ~alias:name `Eq
      | Ast.TyAli (_, Ast.TyVrnSup (_, t), Ast.TyQuo (_,name)) -> variant t ~alias:name `Gt
      | Ast.TyAli (_, Ast.TyVrnInf (_, t), Ast.TyQuo (_,name)) -> variant t ~alias:name `Lt
      | Ast.TyVrnInfSup (_, _, _) -> failwith "deriving does not currently support [ < > ] variant types"
      | Ast.TyLab _ -> failwith "deriving does not support label types"
      | e -> failwith ("unexpected type at expr : " ^ Utils.DumpAst.ctyp e)
    and tagspec = function
      | Ast.TyVrn (_,tag)                  -> Tag (tag, []), []
      | Ast.TyOf (_, Ast.TyVrn (_,tag), t) ->
          let es, vs = List.split (list expr split_comma t) in
            Tag (tag, es), List.concat vs
      | t                                  -> let e, v = expr t in Extends e, v
    and application : Ast.ctyp -> (qname * expr list) * vmap = function
      | Ast.TyApp (_, (Ast.TyApp _ as a), t) ->
          let (tcon, args), vs = application a in
          let e, vs' = expr t in
            (tcon, args @ [e]), vs @ vs'
      | Ast.TyApp (_, (Ast.TyId (_, tcon)), t) ->
          let e, v = expr t in (qident tcon, [e]), v
      | _ -> assert false
    and variant tags ?alias spec =
      let name = fresh_name () in
      let tags, vs = List.split (list tagspec split_or tags) in
        (apply_t name,
         [name, (spec, tags), alias] @ List.concat vs)
    let rec polyexpr : Ast.ctyp -> poly_expr * vmap = function
      | Ast.TyPol (_, ps, t) ->
          begin match polyexpr t with
            | (ps',t'), [] -> (list param split_comma ps @ ps', t'), []
            |  _ -> failwith ("deriving does not support polymorphic variant "
                              ^"definitions within polymorphic record field types")
          end
      | t -> let e, v = expr t in ([], e), v


    let field : Ast.ctyp -> field * vmap = function
      | Ast.TyCol (_, Ast.TyId (_,name), Ast.TyMut (_, t)) ->
          let p, v = polyexpr t in (ident name, p, `Mutable), v
      | Ast.TyCol (_, Ast.TyId (_,name), t) ->
          let p, v = polyexpr t in (ident name, p, `Immutable), v
      | _ -> assert false

    let summand : Ast.ctyp -> summand * vmap = function
      | Ast.TyId (_, c)                  -> (ident c, []), []
      | Ast.TyOf (_, Ast.TyId (_, c), t) ->
          let es, vs = List.split (list expr split_and t) in (ident c, es), List.concat vs
      | _                                -> assert false

    let replace_param params e =
      let params = List.combine params P.params in
      let map = object (self)
        inherit transform as super
        method expr e =
          try
            let p = List.assoc e params in
            `GParam (p, e)
          with Not_found ->
            match e with
            | `Param p ->
              failwith ("deriving does not support existantial type "
                        ^"or partially instantiated return type in GADT");
            | e -> super # expr e
      end
      in
      map # expr e

    let ret_type rt =
      match expr rt with
      | `Constr ([id], params), [] -> id, params
      | `Constr ([_], _), _ :: _ ->
        failwith ("deriving does not currently support polymorphic variant "
                  ^"within GADT")
      | _ -> assert false

    let gsummand : Ast.ctyp -> string * gsummand = function
      | Ast.TyCol (_, Ast.TyId (_, c), Ast.TyArr(_, t, rt)) ->
          let tname, params = ret_type rt in
          let args, vs = List.split (list expr split_and t) in
          if List.concat vs <> [] then
            failwith ("deriving does not currently support polymorphic variant "
                      ^"within GADT");
          let args = List.map (replace_param params) args in
          tname, (ident c, args, params)
      | Ast.TyCol (_, Ast.TyId (_, c), rt) ->
          let tname, params = ret_type rt in
          tname, (ident c, [], params)
      | _                                 -> assert false

    let is_gadt summands =
      List.exists (function Ast.TyCol _ -> true | _ -> false) summands

    let rec repr = function
      | Ast.TyRec (loc, fields) ->
          let fields, vs = List.split (list field split_semi fields) in
            Record fields, List.concat vs
      | Ast.TySum (loc, summands) ->
          let summands = list (fun x -> x) split_or summands in
          if is_gadt summands then
            let tname, summands = List.split (List.map gsummand summands) in
            GSum (List.hd tname, summands), []
          else
            let summands, vs = List.split (List.map summand summands) in
            Sum summands, List.concat vs
      | e -> failwith ("deriving: unexpected representation type ("^Utils.DumpAst.ctyp e^")")

    let toplevel : Ast.ctyp -> rhs * vmap  = function
      | Ast.TyPrv (_, (Ast.TyRec _ | Ast.TySum _ as r)) ->
          let repr, vs = repr r in `Fresh (None, repr, `Private), vs
      | Ast.TyRec _ | Ast.TySum _ as r ->
          let repr, vs = repr r in `Fresh (None, repr, `Public), vs
      | Ast.TyVrnEq (_, t)  ->
          let es, vs = List.split (list tagspec split_or t) in
            `Variant ((`Eq, es), `Public), List.concat vs
      | Ast.TyPrv (_, Ast.TyVrnSup (_, t)) ->
          let es, vs = List.split (list tagspec split_or t) in
            `Variant ((`Gt, es), `Private), List.concat vs
      | Ast.TyVrnSup (_, t) ->
          let es, vs = List.split (list tagspec split_or t) in
            `Variant ((`Gt, es), `Public), List.concat vs
      | Ast.TyVrnInf (_, t) ->
          let es, vs = List.split (list tagspec split_or t) in
            `Variant ((`Lt, es), `Public), List.concat vs
      | Ast.TyVrnInfSup (_, _, _) -> failwith "deriving does not currently support [ < > ] types"
      | Ast.TyNil _ -> `Nothing, []
      | Ast.TyPrv _ -> failwith "deriving does not currently support private rows"
      | Ast.TyMan (_, eq, (Ast.TyRec _ | Ast.TySum _ as r)) ->
          let repr, v1 = repr r and ex, v2 = expr eq in
            `Fresh (Some ex, repr, `Public), v1 @ v2
      | Ast.TyMan (_, eq, Ast.TyPrv (_, (Ast.TyRec _ | Ast.TySum _ as r))) ->
          let repr, v1 = repr r and ex, v2 = expr eq in
            `Fresh (Some ex, repr, `Private), v1 @ v2
      | t -> let e, v = expr t in `Expr e, v

    let constraints : (Ast.ctyp * Ast.ctyp) list -> constraint_ list * vmap =
      fun cs ->
        List.fold_right
          (fun (c1,c2) (es,vs) ->
             let e1,v1 = expr c1
             and e2,v2 = expr c2
             in ((e1,e2)::es), (v1 @ v2 @ vs))
          cs
          ([],[])

    let declify =
      let declify1 (name, variant, alias) : decl * (name * expr) option =
        (name, params, `Variant (variant,`Public), [], true), Option.map (fun a -> a, apply_t name) alias in
        List.map declify1
  end

  type alias_map = expr NameMap.t
  let build_alias_map : (name * expr) option list -> alias_map = fun m ->
    NameMap.fromList (List.concat_map (function None -> [] | Some e -> [e]) m)

  let split : Ast.ctyp -> Ast.ctyp list =
    let rec aux t = match split_and t with
      | Left (l, r) -> aux l @ aux r
      | Right t -> [t]
    in aux

  let rec decl : Ast.ctyp -> decl list * alias_map = function
    | Ast.TyDcl (loc, name, ps, rhs, cs) ->
        set_name_prefix name;
        let module P = WithParams(struct let params = params ps end) in
        let tl, vs = P.toplevel rhs in
        let cs, vcs = P.constraints cs in
        let decls, aliases = List.split (P.declify (vs @ vcs)) in
          [(name, P.params, tl, cs, false)] @ decls, build_alias_map aliases
    | _ -> assert false

  let substitute_aliases : alias_map -> decl -> decl = fun map ->
  object
    inherit transform as super
    method expr = function
      | `Param (p,_) when NameMap.mem p map -> NameMap.find p map
      | e -> super#expr e
  end # decl

  let decls : Ast.ctyp -> decl list =
    fun ctyp ->
      let decls, aliases = List.split (List.map decl (split ctyp)) in
        List.concat
          (List.map
             (List.map
                (substitute_aliases (NameMap.union_disjoint aliases))) decls)
end

module type Untranslate = sig
  open Camlp4.PreCast
  val param: string * [< `Minus | `Plus ] option -> Ast.ctyp
  val qname: string list -> Ast.ident
  val qName: string list -> Ast.ident
  val expr: expr -> Ast.ctyp
  val poly: param list * expr -> Ast.ctyp
  val rhs: rhs -> Ast.ctyp
  val tagspec: tagspec -> Ast.ctyp
  val summand: summand -> Ast.ctyp
  val field: field -> Ast.ctyp
  val repr: repr -> Ast.ctyp
  val constraint_: expr * expr -> Ast.ctyp * Ast.ctyp
  val decl: decl -> Ast.ctyp
  val sigdecl: decl -> Ast.ctyp list
end

module Untranslate (C:sig val _loc : Camlp4.PreCast.Ast.Loc.t end) : Untranslate =
struct
  open Camlp4.PreCast
  open C

  let param (name, v) =
    if name.[0] = '_'
    then
#if ocaml_version < (4, 00)
      Ast.TyAny _loc
#else
      match v with
      | None        -> Ast.TyAny _loc
      | Some `Plus  -> Ast.TyAnP _loc
      | Some `Minus -> Ast.TyAnM _loc
#endif
    else
      match v with
      | None        -> <:ctyp<  '$lid:name$ >>
      | Some `Plus  -> <:ctyp< +'$lid:name$ >>
      | Some `Minus -> <:ctyp< -'$lid:name$ >>

  let rec qname = function
    | [] -> assert false
    | [x] -> <:ident< $lid:x$ >>
    | x::xs -> <:ident< $uid:x$.$qname xs$ >>

  let rec qName = function
    | [] -> assert false
    | [x] -> <:ident< $uid:x$ >>
    | x::xs -> <:ident< $uid:x$.$qName xs$ >>

  let expr =
    let rec expr : expr -> Ast.ctyp = function
        `Param p -> param p
      | `GParam (p, e) -> expr e
      | `Function (f, t) -> <:ctyp< $expr f$ -> $expr t$ >>
      | `Tuple [t] -> expr t
      | `Tuple ts -> Ast.TyTup (_loc, Ast.tySta_of_list (List.map expr ts))
      | `Constr (tcon, args) -> app (Ast.TyId (_loc, qname tcon)) args
      | _ -> assert false
    and app f = function
      | []    -> f
      | [x]   -> <:ctyp< $f$ $expr x$ >>
      | x::xs -> app (<:ctyp< $f$ $expr x$ >>) xs
    in expr

  let poly (params, t) =
    List.fold_right
      (fun (p : param) (t : Ast.ctyp) ->
         Ast.TyPol (_loc, param p, t))
      params
      (expr t)

  let rec rhs : rhs -> Ast.ctyp = function
      | `Fresh (None, t, `Private) -> <:ctyp< private $repr t$ >>
      | `Fresh (None, t, `Public) -> repr t
      | `Fresh (Some e, t, `Private) -> <:ctyp< $expr e$ == private $repr t$ >>
      | `Fresh (Some e, t, `Public) -> Ast.TyMan (_loc, expr e, repr t)
      | `Expr t          -> expr t
      | `Variant ((`Eq, tags), `Public) ->
        <:ctyp< [= $Ast.tyOr_of_list (List.map tagspec tags)$ ] >>
      | `Variant ((`Eq, tags), `Private) ->
        <:ctyp< private [= $Ast.tyOr_of_list (List.map tagspec tags)$ ] >>
      | `Variant ((`Gt, tags), `Public) ->
        <:ctyp< [> $Ast.tyOr_of_list (List.map tagspec tags)$ ] >>
      | `Variant ((`Gt, tags), `Private) ->
        <:ctyp< private [> $Ast.tyOr_of_list (List.map tagspec tags)$ ] >>
      | `Variant ((`Lt, tags), `Public) ->
        <:ctyp< [< $Ast.tyOr_of_list (List.map tagspec tags)$ ] >>
      | `Variant ((`Lt, tags), `Private) ->
        <:ctyp< private [< $Ast.tyOr_of_list (List.map tagspec tags)$ ] >>
      | `Nothing -> <:ctyp< >>
  and tagspec = function
      | Tag (c, []) -> <:ctyp< `$c$ >>
      | Tag (c, ts) -> <:ctyp< `$c$ of $expr (`Tuple ts)$ >>
      | Extends t -> <:ctyp< $expr t$ >>
  and summand (name, (args : expr list)) =
      let args = Ast.tyAnd_of_list (List.map expr args) in
        <:ctyp< $uid:name$ of $args$ >>
  and gsummand tname (name, (args : expr list), (params : expr list)) =
      let rt = expr (`Constr ([tname], params)) in
      let arg =
        match args with
        | [] -> rt
        | _ :: _ -> <:ctyp< $Ast.tyAnd_of_list (List.map expr args)$ -> $rt$ >> in
      <:ctyp< $uid:name$ : $arg$ >>

  and field ((name, t, mut) : field) = match mut with
      | `Mutable   -> <:ctyp< $lid:name$ : mutable $poly t$ >> (* mutable l : t doesn't work; perhaps a camlp4 bug *)
      | `Immutable -> <:ctyp< $lid:name$ : $poly t$ >>
  and repr = function
      | Sum summands  -> Ast.TySum (_loc,Ast.tyOr_of_list (List.map summand summands))
      | GSum (tname, summands)  ->
          Ast.TySum (_loc,Ast.tyOr_of_list (List.map (gsummand tname) summands))
      | Record fields -> <:ctyp< { $list:List.map field fields $ }>>

  let constraint_ (e1,e2) = (expr e1, expr e2)

  let decl ((name, params, r, constraints,_): decl) =
    Ast.TyDcl (_loc, name, List.map param params, rhs r, List.map constraint_ constraints)

  let sigdecl ((name, params, r, constraints, _): decl) =
    [Ast.TyDcl (_loc, name, List.map param params, rhs r, List.map constraint_ constraints)]

end

let free_tvars =
  (* FIXME polycase *)
  let o = object
     inherit [ParamSet.t] fold as default
     method crush = List.fold_left ParamSet.union ParamSet.empty
     method poly_expr = assert false
     method expr = function
       | `Param p  -> ParamSet.singleton p
       | `GParam (p, _)  -> ParamSet.singleton p
       | e -> default#expr e
  end in o#expr

let contains_tvars, contains_tvars_decl =
  let o = object
     inherit [bool] fold as default
     method crush = List.exists F.id
     method expr = function
       | `Param _
       | `GParam _ -> true
       | e -> default#expr e
  end in (o#expr, o#decl)

type subst = expr NameMap.t
let build_subst l = NameMap.fromList l
let substitute map = object
  inherit transform as super
  method expr = function
    | `Param (p,_) when NameMap.mem p map ->
      begin match NameMap.find p map with
        | `GParam (p, _) -> `Param p
        | e -> e
      end
    | `GParam ((p, _), e) when NameMap.mem p map ->
      begin match NameMap.find p map with
        | `Param p -> `GParam (p, e)
        | e -> e
      end
    | e -> super#expr e
end
let substitute_decl map =
  (substitute map)#decl
let substitute_expr map =
  (substitute map)#expr
let substitute_rhs map =
  (substitute map)#rhs
let substitute_constraint map =
  (substitute map)#constraint_

let rename map = object (self)
  inherit transform as super
  method expr = function
    | `Param (p,v) when NameMap.mem p map ->
      `Param(NameMap.find p map,v)
    | `GParam ((p,v),e) when NameMap.mem p map ->
      `GParam((NameMap.find p map,v), self#expr e)
    | e -> super#expr e
end
let rename_rhs map =
  (rename map)#rhs
let rename_constraint map =
  (rename map)#constraint_


(** Pretty-print for error-message *)
open Camlp4.PreCast

module Printer = Camlp4.Printers.OCaml.Make(Syntax)
module Unt = Untranslate(struct let _loc = Loc.ghost end)
let print_expr ty =
  ignore(Format.flush_str_formatter ());
  Printer.print None (fun p fmt -> p#ctyp Format.str_formatter) (Unt.expr ty);
  Format.flush_str_formatter ()

let print_rhs ty =
  ignore(Format.flush_str_formatter ());
  Printer.print None (fun p fmt -> p#ctyp Format.str_formatter) (Unt.rhs ty);
  Format.flush_str_formatter ()
