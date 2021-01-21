(*
 * Copyright yutopp 2021 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module KnotMap = Map.M (Int)

module KnotSet = struct
  include Set.M (Int)

  let at_minus_kv g x : int =
    let seq = Set.to_sequence ~order:`Decreasing ~less_or_equal_to:x g in
    Sequence.hd_exn seq

  let at_plus g x : int =
    let seq =
      Set.to_sequence ~order:`Increasing ~greater_or_equal_to:x g
      |> Sequence.filter ~f:(fun k -> k > x)
    in
    Sequence.hd seq |> Option.value ~default:Int.max_value
end

type elem_t = {
  (* layout expression *)
  l : Expr.t;
  (* span *)
  s : int;
  (* intercept *)
  a : int;
  (* gradient *)
  b : int;
}
[@@deriving eq, show]

type t = elem_t KnotMap.t

let empty () = Map.empty (module Int)

let add g x ~l ~s ~a ~b = Map.add_exn g ~key:x ~data:{ l; s; a; b }

let knots g =
  Map.to_sequence g
  |> Sequence.map ~f:(fun (k, _v) -> k)
  |> Sequence.to_list
  |> Set.of_list (module Int)

let to_alist g = Map.to_alist g

let at_minus_kv g x : int * elem_t =
  Option.value_exn (Map.closest_key g `Less_or_equal_to x)

let at_plus g x : int =
  Option.value_map
    (Map.closest_key g `Greater_than x)
    ~f:(fun (k, _v) -> k)
    ~default:Int.max_value

let at g x =
  let (_, v) = at_minus_kv g x in
  v

let calc_v ~a ~b ~dist = a + (b * dist)

let v g x =
  let (x_m, { a; b; _ }) = at_minus_kv g x in
  calc_v ~a ~b ~dist:(x - x_m)

let l g x =
  let { l; _ } = at g x in
  l

let s g x =
  let { s; _ } = at g x in
  s

let a g x =
  let { a; _ } = at g x in
  a

let b g x =
  let { b; _ } = at g x in
  b
