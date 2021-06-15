(* Some signatures shared by different chapters *)

module type Ordered = sig
  type t

  val eq : t -> t -> bool
  val lt : t -> t -> bool
  val leq : t -> t -> bool

  (* Not included in Okasaki, easier for experiments *)
  val to_string : t -> string
end
