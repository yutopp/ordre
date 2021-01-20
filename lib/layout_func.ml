(*
 * Copyright yutopp 2021 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module KnotMap = Map.M (Int)

type elem_t = {
  (* layout expression *)
  l : unit;
  (* span *)
  s : int;
  (* intercept *)
  a : int;
  (* gradient *)
  b : int;
}
[@@deriving eq, show]

type 'a t = elem_t KnotMap.t

let empty () = Map.empty (module Int)

let add g x ~l ~s ~a ~b = Map.add_exn g ~key:x ~data:{ l; s; a; b }

let knots g = Map.keys g

let to_alist g = Map.to_alist g

let at_minus_kv g x : int * elem_t =
  Option.value_exn (Map.closest_key g `Less_or_equal_to x)

let at g x =
  let (_, v) = at_minus_kv g x in
  v

let calc_v { l; s; a; b } = a + b

let v g x = calc_v (at g x)

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
