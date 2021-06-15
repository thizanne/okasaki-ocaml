module type Set = sig
  type elem
  type t

  val empty : t
  val insert : elem -> t -> t
  val member : elem -> t -> bool

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


module Unbalanced_set (Element : Ordered) : Solutions with type elem = Element.t =
struct

  type elem = Element.t

  type t =
    | E
    | T of t * elem * t

  let empty = E

  let rec member x = function
    | E -> false
    | T (left, y, right) ->
      if Element.lt x y
      then member x left
      else if Element.lt y x
      then member x right
      else (* x = y *) true

  let rec insert x set = match set with
    | E -> T (E, x, E)
    | T (left, y, right) ->
      if Element.lt x y
      then T (insert x left, y, right)
      else if Element.lt y x
      then T (left, y, insert x right)
      else set

  module Exercise_2_2 = struct
    (* Improve member *)

    (* So here we have a choice to make:

       - We can use an option type for the candidate. Drawback: we may
         be saving a comparison, but we're spending an allocation
         instead. Benchmarks would be needed, but we're probably losing
         overall.

       - We can duplicate the code into a main function that has no
         candidate for equality yet, and hands over to a function that
         accepts a candidate if it traverses an element greater or equal
         to the one it's looking for. Drawback: code duplication, which is
         nice neither for the eye not the performance.

       Both solutions are implemented below, and we will then show
       a solution that actually solves both issues at once. Keep with us!
    *)

    module With_option = struct

      let rec member' ~candidate x = function
        (* Could use optional argument syntax, but that's just sugar *)
        | E ->
          begin match candidate with
            | None -> false
            | Some candidate -> Element.eq candidate x
          end
        | T (left, y, right) ->
          if Element.lt x y
          then member' ~candidate x left
          else member' ~candidate:(Some y) x right

      let member x set =
        member' ~candidate:None x set

    end

    module With_duplication = struct

      let rec member' ~candidate x = function
        | E -> Element.eq candidate x
        | T (left, y, right) ->
          if Element.lt x y
          then member' ~candidate x left
          else member' ~candidate:y x right

      let rec member x = function
        | E -> false
        | T (left, y, right) ->
          if Element.lt x y
          then member x left
          else member' ~candidate:y x right
    end

    module Using_root = struct

      (* Another clean solution that, when searching a non-empty tree,
         always starts with the root of the tree as a dummy
         candidate. As long as this candidate will be updated at some
         point during the traversal, this does not change anything to
         the high-level behaviour of the function, as the root value
         will never be used. Otherwise, upon reaching a leaf while still
         having the original root candidate, this means the value is
         actually absent -- but the code doesn't know it yet. Then the
         root will be compared with the element we're looking for, as if
         it were an actual potential candidate that turns out to be
         invalid.

         Put in another way, it avoids both code duplication and option
         allocation by using a dummy value that we can access and has
         the correct type. This is correct because candidate are
         expected to be potentially invalid.

         Compared to the GADT solution, we use one less immediate
         parameter and pattern matching, and compute one more element
         comparison when the element is absent. This solution has the
         advantage of not needing advanced-ish type-level machinery (and
         can thus for instance be implemented in simpler languages, like SML).
      *)

      let rec member' ~candidate x = function
        | E -> Element.eq candidate x
        | T (left, y, right) ->
          if Element.lt x y
          then member' ~candidate x left
          else member' ~candidate:y x right

      let member x set = match set with
        | E -> false
        | T (_left, root, _right) ->
          member' ~candidate:root x set
    end

    module With_gadt = struct
      (* And now for the actual nice solution: we're using a GADT to
         witness if we've found a candidate yet. The candidate itself is
         given unboxed -- if we found none yet, we just use any dummy
         value (here, the unit one). Upon reaching the bottom of the tree,
         we use the candidate only if the witness manifests that it's
         relevant.

         As the candidate is now unboxed (no option type) and the witness
         constructors have no parameter, there is no allocation at all
         involved -- yet we still write only one traversal!

         For a more detailed explanation on a similar pattern, read:
         https://blog.janestreet.com/why-gadts-matter-for-performance/

         Note: I find this similar to how one can implement `nth` for
         length-indexed lists in a dependently typed language like core
         Coq -- using unit as a dummy value, having types that show that
         it cannot leak.

         Another note: this transformation could probably be done
         mechanically for a lot of code that uses options. However, it's
         only winning in the Some case (by not allocating), otherwise it's
         using two words (one for No_candidate_yet + one for the unit)
         instead of one for representing None. It's still nice, and when
         we're talking about avoiding one comparison at each step,
         avoiding one allocation also at each step at the cost of using
         one extra word in the specific situation where the whole
         traversal never encounters a candidate looks definitely
         interesting. *)

      type 'a witness =
        | No_candidate_yet : _ witness (* We found no candidate yet *)
        | Found_a_candidate : elem witness (* A candidate has been found *)

      let rec member' : type candidate. candidate witness -> candidate -> elem -> t -> bool =
        fun witness candidate x set -> match set with
          | E ->
            begin match witness, candidate with
              | No_candidate_yet, _ -> false
              | Found_a_candidate, candidate -> Element.eq candidate x
            end
          | T (left, y, right) ->
            if Element.lt x y
            then member' witness candidate x left
            else member' Found_a_candidate y x right

      let member x set =
        member' No_candidate_yet () x set

    end
  end

  module Exercise_2_3 = struct

    (* Improve insertion of an existing element *)

    exception Already_present

    let rec insert_absent x set = match set with
      | E -> T (E, x, E)
      | T (left, y, right) ->
        (* Recursive call to insert_absent will be computed before
           constructing the node, thus if it raises on an already
           present element no copy will be done. The handler on insert
           will then return the original set. *)
        if Element.lt x y
        then T (insert_absent x left, y, right)
        else if Element.lt y x
        then T (left, y, insert_absent x right)
        else raise Already_present

    let insert x set =
      try insert_absent x set
      with Already_present -> set
  end

  module Exercise_2_4 = struct

    (* Exercise 2.4: improve insert, again (merge exercises 2.2 and 2.3). *)

    (* We will only use the GADT solution of exercise 2.2. *)

    type 'a witness = 'a Exercise_2_2.With_gadt.witness

    exception Already_present = Exercise_2_3.Already_present

    let rec insert_absent : type candidate. candidate witness -> candidate -> elem -> t -> t =
      fun witness candidate x set ->
      match set with
      (* candidate is the last element traversed that was lesser than or
         equal to the element we're trying to insert. Upon reaching
         a leaf, it's the greatest of the elements from the tree that
         are leq to the inserted element. Therefore, either it's equal
         to the inserted element, or this inserted element is absent
         from the tree. *)
      | E ->
        begin match witness, candidate with
          | No_candidate_yet, _ -> T (E, x, E)
          | Found_a_candidate, y ->
            if Element.eq x y
            then raise Already_present
            else T (E, x, E)
        end
      | T (left, y, right) ->
        if Element.lt x y
        then T (insert_absent witness candidate x left, y, right)
        else T (left, y, insert_absent Found_a_candidate y x right)

    let insert x set =
      try insert_absent No_candidate_yet () x set
      with Already_present -> set

  end

  module Exercise_2_5 = struct

    let rec complete ~depth elem =
      if depth = 0
      then E
      else
        let sub = complete ~depth:(depth - 1) elem in
        T (sub, elem, sub)

    let rec create2 ~size elem =
      (* Returns a pair of trees containing (size, size + 1) elements *)
      if size = 0
      then E, T (E, elem, E)
      else if size mod 2 = 1
      then
        let left, right = create2 ~size:(size / 2) elem in
        T (left, elem, left),
        T (left, elem, right)
      else
        let left, right = create2 ~size:((size - 1) / 2) elem in
        T (left, elem, right),
        T (right, elem, right)

    let balanced ~size elem =
      fst @@ create2 ~size elem

  end

  let to_string set =
    (* Tree printing utility for easier debugging *)
    let get_name = function
      | T (_, x, _) -> Element.to_string x
      | E -> "∅"
    in
    let get_children = function
      | T (left, _, right) -> [left; right]
      | E -> []
    in
    Print_tree.to_string ~get_name ~get_children set
end

(* For easier testing *)
module Int_elem = struct
  type t = int
  let lt = ( < )
  let eq = ( = )
  let leq = (<=)
  let to_string = string_of_int
end

module String_elem = struct
  type t = string
  let lt = ( < )
  let eq = ( = )
  let leq = (<=)
  let to_string x = x
end

module I = Unbalanced_set (Int_elem)
module S = Unbalanced_set (String_elem)
