module type Set = sig
  type elem
  type t

  val empty : t
  val insert : elem -> t -> t
  val member : elem -> t -> bool

  (* Bonus, for easing experiments *)
  val to_string : t -> string
end

module type Solutions = sig
  include Set

  module Exercise_2_2 : sig

    module With_option : sig
      val member : elem -> t -> bool
    end

    module With_duplication : sig
      val member : elem -> t -> bool
    end

    module Using_root : sig
      val member : elem -> t -> bool
    end

    module With_gadt : sig
      val member : elem -> t -> bool
    end

  end

  module Exercise_2_3 : sig
    val insert : elem -> t -> t
  end

  module Exercise_2_4 : sig
    val insert : elem -> t -> t
  end

  module Exercise_2_5 : sig
    val complete : depth:int -> elem -> t
    val balanced : size:int -> elem -> t
  end
end

module Unbalanced_set (Element : Sig.Ordered) : Solutions with type elem = Element.t

module I : Solutions with type elem = int
module S : Solutions with type elem = string
