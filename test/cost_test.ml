(*
 * Copyright yutopp 2021 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
open Ordre

let debug g =
  Map.iteri g ~f:(fun ~key ~data:Layout_func.{ l; s; a; b } ->
      Stdio.printf "key=%d l=%s s=%d a=%d b=%d\n" key (Expr.to_string l) s a b);
  failwith ""

let test_txt0 () =
  let l = Expr.Txt "aaaaa" in
  let g = Cost.from_expr ~m:10 l in

  Alcotest.(check Helper.alist)
    "s < m"
    [
      (0, { l = Expr.Txt "aaaaa"; s = 5; a = 0; b = 0 });
      (5, { l = Expr.Txt "aaaaa"; s = 5; a = 0; b = 1 });
    ]
    (Layout_func.to_alist g)

let test_txt1 () =
  let l = Expr.Txt "aaaaa" in
  let g = Cost.from_expr ~m:4 l in
  Alcotest.(check Helper.alist)
    "s >= m"
    [ (0, { l = Expr.Txt "aaaaa"; s = 5; a = 1; b = 1 }) ]
    (Layout_func.to_alist g)

let test_juxtpos0 () =
  let l0 = Expr.Txt "aa" in
  let l1 = Expr.Txt "bb" in
  let l = Expr.Juxtaposition (l0, l1) in
  let g = Cost.from_expr ~m:5 l in
  Alcotest.(check Helper.alist)
    "aabb"
    [
      ( 0,
        {
          l = Expr.Juxtaposition (Expr.Txt "aa", Expr.Txt "bb");
          s = 4;
          a = 0;
          b = 0;
        } );
      ( 1,
        {
          l = Expr.Juxtaposition (Expr.Txt "aa", Expr.Txt "bb");
          s = 4;
          a = 0;
          b = 1;
        } );
      ( 3,
        {
          l = Expr.Juxtaposition (Expr.Txt "aa", Expr.Txt "bb");
          s = 4;
          a = 2;
          b = 1;
        } );
    ]
    (Layout_func.to_alist g)

let () =
  let open Alcotest in
  run "Ordre"
    [
      ("txt", [ test_case "0" `Quick test_txt0; test_case "1" `Quick test_txt1 ]);
      ("juxtpos", [ test_case "0" `Quick test_juxtpos0 ]);
    ]
