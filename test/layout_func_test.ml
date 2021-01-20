(*
 * Copyright yutopp 2021 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

let u = Ordre.Expr.Unit

let elem_t =
  Alcotest.testable
    (fun fmt v -> Ordre.Layout_func.pp_elem_t fmt v)
    Ordre.Layout_func.equal_elem_t

let test_empty () =
  let open Ordre.Layout_func in
  let g = empty () in
  ()

let test_add () =
  let open Ordre.Layout_func in
  let g = empty () in
  let g = add g 0 ~l:u ~s:1 ~a:2 ~b:3 in
  let g = add g 1 ~l:u ~s:10 ~a:20 ~b:30 in

  Alcotest.(check (list (pair int elem_t)))
    "same lists"
    [
      (0, { l = u; s = 1; a = 2; b = 3 }); (1, { l = u; s = 10; a = 20; b = 30 });
    ]
    (to_alist g)

let test_at_minus_kv () =
  let open Ordre.Layout_func in
  let g = empty () in
  let g = add g 0 ~l:u ~s:1 ~a:2 ~b:3 in
  let g = add g 2 ~l:u ~s:10 ~a:20 ~b:30 in
  let g = add g 10 ~l:u ~s:100 ~a:200 ~b:300 in

  Alcotest.(check (pair int elem_t))
    "at 0"
    (0, { l = u; s = 1; a = 2; b = 3 })
    (at_minus_kv g 0);
  Alcotest.(check (pair int elem_t))
    "at 1"
    (0, { l = u; s = 1; a = 2; b = 3 })
    (at_minus_kv g 1);
  Alcotest.(check (pair int elem_t))
    "at 2"
    (2, { l = u; s = 10; a = 20; b = 30 })
    (at_minus_kv g 2);
  Alcotest.(check (pair int elem_t))
    "at 3"
    (2, { l = u; s = 10; a = 20; b = 30 })
    (at_minus_kv g 3)

let () =
  let open Alcotest in
  run "Layout_func"
    [
      ("empty", [ test_case "0" `Quick test_empty ]);
      ("add", [ test_case "0" `Quick test_add ]);
      ("at_minus_kv", [ test_case "0" `Quick test_at_minus_kv ]);
    ]
