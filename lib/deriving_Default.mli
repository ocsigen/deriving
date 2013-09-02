
module type Default = sig
  type a
  val default : unit -> a
end

module Defaults(D : Default) : Default with type a = D.a
module Default_string : Default with type a = string
module Default_int64 : Default with type a = int64
module Default_int : Default with type a = int
module Default_bool : Default with type a = bool
module Default_unit : Default with type a = unit
module Default_char : Default with type a = char
module Default_float : Default with type a = float
module Default_list (A : Default)  : Default with type a = A.a list
module Default_option (A : Default) : Default with type a = A.a option
module Default_array (A : Default) : Default with type a = A.a array
module Default_ref (A : Default) : Default with type a = A.a ref
