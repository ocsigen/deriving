(* Copyright Jeremy Yallop 2007.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

(* Interned strings *)
module BytesMap = Map.Make(Bytes)

(* global state *)
let map = ref BytesMap.empty
let counter = ref 0

type t = int * string
    deriving (Show)

let intern s =
  let bs = Bytes.of_string s in
  try BytesMap.find bs !map
  with Not_found ->
    let fresh = (!counter, s) in begin
      map := BytesMap.add bs fresh !map;
      incr counter;
      fresh
    end

let to_string (_,s) = s
let name = snd
let compare (l,_) (r,_) = compare l r
let eq (l,_) (r,_) = l = r
