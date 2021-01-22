(*
 * Copyright yutopp 2021 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type t =
  | Unit
  | Txt of string
  | Stacked of t * t
  | Juxtaposition of t * t
  | Choice of t * t
[@@deriving eq, show]

(* TODO: optimize *)
let rec to_string e =
  match e with
  | Unit -> ""
  | Txt s -> s
  | Stacked (l1, l2) -> Printf.sprintf "%s\n%s" (to_string l1) (to_string l2)
  | Juxtaposition (l1, l2) ->
      Printf.sprintf "%s%s" (to_string l1) (to_string l2)
  | Choice _ -> failwith ""
