(* Some signatures shared by different chapters *)

module type Ordered = sig
  type t

  val eq : t -> t -> bool
  val lt : t -> t -> bool
  val leq : t -> t -> bool

  (* Not included in Okasaki, easier for experiments *)
  val compare : t -> t -> int
  val to_string : t -> string

end

(* For easier testing *)
module Int_ordered : Ordered with type t = int = struct
  type t = int
  let compare = compare
  let lt = ( < )
  let eq = ( = )
  let leq = (<=)
  let to_string = string_of_int
end

module String_ordered : Ordered with type t = string = struct
  type t = string
  let compare = compare
  let lt = ( < )
  let eq = ( = )
  let leq = (<=)
  let to_string x = x
end
