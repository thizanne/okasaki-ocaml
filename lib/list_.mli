type 'a t (* Rather than 'a stack *)

val empty : 'a t

val is_empty : 'a t -> bool

val cons : 'a -> 'a t -> 'a t

val head : 'a t -> 'a

val tail : 'a t -> 'a t

module Exercise_2_1 : sig

  (** O(n) time, O(n) space *)
  val suffixes : 'a t -> 'a t t

end
