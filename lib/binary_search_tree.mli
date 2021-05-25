module type Set = sig
  type elem
  type t

  val empty : t
  val insert : elem -> t -> t
  val member : elem -> t -> bool

  val complete : depth:int -> elem -> t

  val balanced : size:int -> elem -> t

  (* Bonus, for easing experiments *)
  val to_string : t -> string
end

module type Ordered = sig
  type t

  val eq : t -> t -> bool
  val lt : t -> t -> bool
  val leq : t -> t -> bool

  (* Not included in Okasaki, easier for experiments *)
  val to_string : t -> string
end

module Unbalanced_set (Element : Ordered) : Set with type elem = Element.t

module I : Set with type elem = int
module S : Set with type elem = string
