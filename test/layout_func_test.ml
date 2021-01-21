(*
 * Copyright yutopp 2021 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

let test_empty () =
  let open Ordre.Layout_func in
  let g = empty () in
  ()

let test_add () =
  let open Ordre.Layout_func in
  let g = empty () in
  let g = add g 0 ~l:Helper.u ~s:1 ~a:2 ~b:3 in
  let g = add g 1 ~l:Helper.u ~s:10 ~a:20 ~b:30 in

  Alcotest.(check Helper.alist)
    "same lists"
    [
      (0, { l = Helper.u; s = 1; a = 2; b = 3 });
      (1, { l = Helper.u; s = 10; a = 20; b = 30 });
    ]
    (to_alist g)

let test_at_minus_kv () =
  let open Ordre.Layout_func in
  let g = empty () in
  let g = add g 0 ~l:Helper.u ~s:1 ~a:2 ~b:3 in
  let g = add g 2 ~l:Helper.u ~s:10 ~a:20 ~b:30 in
  let g = add g 10 ~l:Helper.u ~s:100 ~a:200 ~b:300 in

  Alcotest.(check Helper.kv)
    "at 0"
    (0, { l = Helper.u; s = 1; a = 2; b = 3 })
    (at_minus_kv g 0);
  Alcotest.(check Helper.kv)
    "at 1"
    (0, { l = Helper.u; s = 1; a = 2; b = 3 })
    (at_minus_kv g 1);
  Alcotest.(check Helper.kv)
    "at 2"
    (2, { l = Helper.u; s = 10; a = 20; b = 30 })
    (at_minus_kv g 2);
  Alcotest.(check Helper.kv)
    "at 3"
    (2, { l = Helper.u; s = 10; a = 20; b = 30 })
    (at_minus_kv g 3)

let test_at_minus_kv_sets () =
  let open Ordre.Layout_func in
  let g = empty () in
  let g = add g 0 ~l:Helper.u ~s:1 ~a:2 ~b:3 in
  let g = add g 2 ~l:Helper.u ~s:10 ~a:20 ~b:30 in
  let g = add g 10 ~l:Helper.u ~s:100 ~a:200 ~b:300 in
  let ks = knots g in

  Alcotest.(check int) "at 0" 0 (KnotSet.at_minus_kv ks 0);
  Alcotest.(check int) "at 1" 0 (KnotSet.at_minus_kv ks 1);
  Alcotest.(check int) "at 2" 2 (KnotSet.at_minus_kv ks 2);
  Alcotest.(check int) "at 3" 2 (KnotSet.at_minus_kv ks 3)

let test_at_plus () =
  let open Ordre.Layout_func in
  let g = empty () in
  let g = add g 0 ~l:Helper.u ~s:1 ~a:2 ~b:3 in
  let g = add g 2 ~l:Helper.u ~s:10 ~a:20 ~b:30 in
  let g = add g 10 ~l:Helper.u ~s:100 ~a:200 ~b:300 in

  Alcotest.(check int) "at 0" 2 (at_plus g 0);
  Alcotest.(check int) "at 1" 2 (at_plus g 1);
  Alcotest.(check int) "at 2" 10 (at_plus g 2);
  Alcotest.(check int) "at 10" Int.max_value (at_plus g 10)

let test_at_plus_sets () =
  let open Ordre.Layout_func in
  let g = empty () in
  let g = add g 0 ~l:Helper.u ~s:1 ~a:2 ~b:3 in
  let g = add g 2 ~l:Helper.u ~s:10 ~a:20 ~b:30 in
  let g = add g 10 ~l:Helper.u ~s:100 ~a:200 ~b:300 in
  let ks = knots g in

  Alcotest.(check int) "at 0" 2 (KnotSet.at_plus ks 0);
  Alcotest.(check int) "at 1" 2 (KnotSet.at_plus ks 1);
  Alcotest.(check int) "at 2" 10 (KnotSet.at_plus ks 2);
  Alcotest.(check int) "at 10" Int.max_value (KnotSet.at_plus ks 10)

let () =
  let open Alcotest in
  run "Layout_func"
    [
      ("empty", [ test_case "0" `Quick test_empty ]);
      ("add", [ test_case "0" `Quick test_add ]);
      ( "at_minus_kv",
        [
          test_case "map" `Quick test_at_minus_kv;
          test_case "set" `Quick test_at_minus_kv_sets;
        ] );
      ( "at_plus",
        [
          test_case "map" `Quick test_at_plus;
          test_case "set" `Quick test_at_plus_sets;
        ] );
    ]
