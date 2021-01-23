(*
 * Copyright yutopp 2021 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

let u = Ordre.Expr.Unit

let expr = Alcotest.testable (fun fmt v -> Ordre.Expr.pp fmt v) Ordre.Expr.equal

let elem_t =
  Alcotest.testable
    (fun fmt v -> Ordre.Layout_func.pp_elem_t fmt v)
    Ordre.Layout_func.equal_elem_t

let kv = Alcotest.(pair int elem_t)

let alist = Alcotest.(list kv)
