module Leftist_heap (Elem : Std.Ordered) : Leftist_heap_intf.Heap with module Elem := Elem = struct
  module Elem = Elem

  type t =
    | E
    | T of int (* rank = right spine length *) * Elem.t * t * t

  let empty = E

  let singleton x =
    T (1, x, E, E)

  let is_empty = function
    | E -> true
    | T _ -> false

  let rank = function
    | E -> 0
    | T (rank, _, _, _) -> rank

  let make x a b =
    if rank a >= rank b
    then T (rank b + 1, x, a, b)
    else T (rank a + 1, x, b, a)

  let rec merge h1 h2 = match h1, h2 with
    | E, _ -> h2
    | _, E -> h1
    | T (_, x, a1, b1), T (_, y, a2, b2) ->
      if Elem.leq x y
      then make x a1 (merge b1 h2)
      else make y a2 (merge h1 b2)

  let insert x h =
    merge (singleton x) h

  let find_min = function
    | E -> failwith "find_min"
    | T (_, x, _, _) -> x

  let delete_min = function
    | E -> failwith "delete_min"
    | T (_, _, a, b) -> merge a b

  let to_string set =
    (* Tree printing utility for easier debugging *)
    let get_name = function
      | T (rank, x, _, _) -> string_of_int rank ^ ", " ^ Elem.to_string x
      | E -> "∅"
    in
    let get_children = function
      | T (_, _, left, right) -> [left; right]
      | E -> []
    in
    Print_tree.to_string ~get_name ~get_children set

  module Exercise_3_2 = struct

    let rec insert x h = match h with
      | E -> singleton x
      | T (r, y, a, b) ->
        if Elem.leq x y
        then T (r + 1, x, h, E)
        else make y a (insert x b)
  end

  module Exercise_3_3 = struct

    let from_list li =
      (* Successively merge adjacent pairs of heaps *)
      let rec aux current next = match current with
        | [] ->
          begin match next with
            | [] -> empty
            | _ -> aux next []
          end
        | [h] ->
          begin match next with
            | [] -> h
            | _ -> aux (h :: next) []
          end
        | h1 :: h2 :: hs ->
          aux hs (merge h1 h2 :: next)
      in
      (* The map to get a list of singleton heaps before iterating
         merges could probably be micro-optimised away, but it's
         off-topic here. *)
      aux (List.map singleton li) []
  end

end

module I = Leftist_heap (Std.Int_ordered)
module S = Leftist_heap (Std.String_ordered)

let%test_module _ =
  (module struct
    let elems = [-2; 4; 3; 4; -7; 1; -10; -3; -5; 0; 9; 4; 5; 6; 9]

    let sorted = List.sort Std.Int_ordered.compare elems

    let naive_of_list insert li =
      List.fold_left (fun acc x -> insert x acc) I.empty li

    let rec to_sorted_list insert h =
      if I.is_empty h
      then []
      else I.find_min h :: to_sorted_list insert (I.delete_min h)

    let test_that_heapsort_sorts insert =
      (* Parameterised by the insert to use, so that we can also test exercises *)
      List.equal
        Std.Int_ordered.eq
        sorted
        (elems |> naive_of_list insert |> to_sorted_list insert)

    let%test "textbook" =
      test_that_heapsort_sorts I.insert

    let%test "exercise_3_2" =
      test_that_heapsort_sorts I.Exercise_3_2.insert

    let%test "exercise_3_3" =
      List.equal
        Std.Int_ordered.eq
        (to_sorted_list I.insert @@ naive_of_list I.insert elems)
        (to_sorted_list I.insert @@ I.Exercise_3_3.from_list elems)
  end)
