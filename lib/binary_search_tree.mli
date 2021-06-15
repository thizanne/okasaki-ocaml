module type Set = sig
  type elem
  type t

  val empty : t
  val insert : elem -> t -> t
  val member : elem -> t -> bool

  (* Bonus, for easing experiments *)
  val to_string : t -> string
end

module Unbalanced_set (Element : Sig.Ordered) :
  Binary_search_tree_intf.Solutions
  with type elem = Element.t

module I : Binary_search_tree_intf.Solutions with type elem = int
module S : Binary_search_tree_intf.Solutions with type elem = string
