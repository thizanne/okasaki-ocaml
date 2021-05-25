type 'a t = 'a list

exception Empty

let empty = []

let is_empty = function
  | [] -> true
  | _ :: _ -> false

let cons x xs =
  x :: xs

let head = function
  | [] -> raise Empty
  | x :: _ -> x

let tail = function
  | [] -> raise Empty
  | _ :: xs -> xs

(* Exercice 2.1 *)

let rec suffixes li = match li with
  | [] -> [[]]
  | _ :: xs ->
    li :: suffixes xs

let%test _ =
  suffixes [1; 2; 3; 4] = [[1; 2; 3; 4]; [2; 3; 4]; [3; 4]; [4]; []]
