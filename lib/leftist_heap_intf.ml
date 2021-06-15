module type Heap = sig
  module Elem : Std.Ordered

  type t

  val empty : t

  val is_empty : t -> bool

  val insert : Elem.t -> t -> t

  val merge : t -> t -> t

  (** Raises if the heap is empty *)
  val find_min : t -> Elem.t

  (** Raises if the heap is empty *)
  val delete_min : t -> t

  val to_string : t -> string

  module Exercise_3_2 : sig
    val insert : Elem.t -> t -> t
  end

  module Exercise_3_3 : sig
    val from_list : Elem.t list -> t
  end
end

module type Solutions = sig
  include Heap
end
