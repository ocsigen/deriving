module type Default = sig
  type a
  val default : unit -> a
end

module Defaults(D : Default) : Default with type a = D.a = struct
  include D
end

module Default_string = Defaults(struct
  type a = string
  let default () = ""
end)

module Default_int64 = Defaults(struct
  type a = int64
  let default () = 0L
end)


module Default_int = Defaults(struct
  type a = int
  let default () = 0
end)

module Default_bool = Defaults(struct
  type a = bool
  let default () = true
end)

module Default_unit = Defaults(struct
  type a = unit
  let default () = ()
end)

module Default_char = Defaults(struct
  type a = char
  let default () = '0'
end)

module Default_float = Defaults(struct
  type a = float
  let default () = 0.0
end)

module Default_list (A : Default) = Defaults(struct
  type a = A.a list
  let default () = []
end)

module Default_option (A : Default) = Defaults(struct
  type a = A.a option
  let default () = None
end)

module Default_array (A : Default) = Defaults(struct
  type a = A.a array
  let default () = [||]
end)


module Default_ref (A : Default) = Defaults(struct
  type a = A.a ref
  let default () = ref (A.default ())
end)
