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
  try BytesMap.find s !map
  with Not_found ->
    let fresh = (!counter, Bytes.of_string s) in begin
      map := BytesMap.add s fresh !map;
      incr counter;
      fresh
    end

let to_string (_,s) = Bytes.to_string s
let name = snd
let compare (l,_) (r,_) = compare l r
let eq (l,_) (r,_) = l = r
