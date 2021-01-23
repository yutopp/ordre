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

let test_stack_0 () =
  let l0 = Expr.Txt "a" in
  let l1 = Expr.Txt "bb" in
  let l = Expr.Stacked (l0, l1) in
  let g = Cost.from_expr ~m:5 l in
  Alcotest.(check Helper.alist)
    "abb"
    [
      ( 0,
        { l = Expr.Stacked (Expr.Txt "a", Expr.Txt "bb"); s = 2; a = 1; b = 0 }
      );
      (3, { l = Expr.Txt "bb"; s = 2; a = 0; b = 1 });
      (4, { l = Expr.Txt "a"; s = 1; a = 0; b = 1 });
    ]
    (Layout_func.to_alist g)

let test_stack_1 () =
  let l0 = Expr.Txt "bb" in
  let l1 = Expr.Txt "a" in
  let l = Expr.Stacked (l0, l1) in
  let g = Cost.from_expr ~m:5 l in
  Alcotest.(check Helper.alist)
    "bba"
    [
      ( 0,
        { l = Expr.Stacked (Expr.Txt "bb", Expr.Txt "a"); s = 2; a = 1; b = 0 }
      );
      (3, { l = Expr.Txt "bb"; s = 2; a = 0; b = 1 });
      (4, { l = Expr.Txt "a"; s = 1; a = 0; b = 1 });
    ]
    (Layout_func.to_alist g)

let test_choise_0 () =
  let l0 = Expr.Txt "a" in
  let l1 = Expr.Txt "bb" in
  let l = Expr.Choice (l0, l1) in

  let g = Cost.from_expr ~m:5 l in
  Alcotest.(check Helper.alist)
    "m = 5"
    [
      (0, { l = Expr.Txt "a"; s = 1; a = 0; b = 0 });
      (3, { l = Expr.Txt "a"; s = 1; a = 0; b = 0 });
      (4, { l = Expr.Txt "a"; s = 1; a = 0; b = 1 });
    ]
    (Layout_func.to_alist g);

  let g = Cost.from_expr ~m:1 l in
  Alcotest.(check Helper.alist)
    "m = 1"
    [ (0, { l = Expr.Txt "a"; s = 1; a = 0; b = 1 }) ]
    (Layout_func.to_alist g)

let test_e_0 () =
  let l0 = Expr.Choice (Expr.Txt "a", Expr.Txt "b") in
  let l1 = Expr.Choice (Expr.Txt "c", Expr.Txt "d") in
  let l = Expr.Juxtaposition (l0, l1) in

  let l' = Cost.e l in
  Alcotest.(check Helper.expr)
    "e"
    (Expr.Choice
       ( Expr.Juxtaposition
           ( Expr.Txt "a",
             Expr.Choice
               ( Expr.Juxtaposition (Expr.Txt "c", Expr.Unit),
                 Expr.Juxtaposition (Expr.Txt "d", Expr.Unit) ) ),
         Expr.Juxtaposition
           ( Expr.Txt "b",
             Expr.Choice
               ( Expr.Juxtaposition (Expr.Txt "c", Expr.Unit),
                 Expr.Juxtaposition (Expr.Txt "d", Expr.Unit) ) ) ))
    l'

let test_article_0 () =
  let l0 = Expr.Choice (Expr.Txt "a", Expr.Txt "bb") in
  let l1 = Expr.Choice (Expr.Txt "c", Expr.Txt "dd") in
  let l2 = Expr.Choice (Expr.Txt "e", Expr.Txt "ff") in
  let l = Expr.Juxtaposition (l0, Expr.Juxtaposition (l1, l2)) in

  let l' = Cost.e l in
  Alcotest.(check Helper.expr)
    "e"
    (Expr.Choice
       ( Expr.Juxtaposition
           ( Expr.Txt "a",
             Expr.Choice
               ( Expr.Juxtaposition
                   ( Expr.Txt "c",
                     Expr.Choice
                       ( Expr.Juxtaposition (Expr.Txt "e", Expr.Unit),
                         Expr.Juxtaposition (Expr.Txt "ff", Expr.Unit) ) ),
                 Expr.Juxtaposition
                   ( Expr.Txt "dd",
                     Expr.Choice
                       ( Expr.Juxtaposition (Expr.Txt "e", Expr.Unit),
                         Expr.Juxtaposition (Expr.Txt "ff", Expr.Unit) ) ) ) ),
         Expr.Juxtaposition
           ( Expr.Txt "bb",
             Expr.Choice
               ( Expr.Juxtaposition
                   ( Expr.Txt "c",
                     Expr.Choice
                       ( Expr.Juxtaposition (Expr.Txt "e", Expr.Unit),
                         Expr.Juxtaposition (Expr.Txt "ff", Expr.Unit) ) ),
                 Expr.Juxtaposition
                   ( Expr.Txt "dd",
                     Expr.Choice
                       ( Expr.Juxtaposition (Expr.Txt "e", Expr.Unit),
                         Expr.Juxtaposition (Expr.Txt "ff", Expr.Unit) ) ) ) )
       ))
    l';

  let g = Cost.from_expr ~m:10 l in
  Alcotest.(check Helper.alist)
    "m = 10"
    [
      ( 0,
        {
          l =
            Expr.Juxtaposition
              (Expr.Txt "a", Expr.Juxtaposition (Expr.Txt "c", Expr.Txt "e"));
          s = 3;
          a = 0;
          b = 0;
        } );
      ( 4,
        {
          l =
            Expr.Juxtaposition
              (Expr.Txt "a", Expr.Juxtaposition (Expr.Txt "c", Expr.Txt "e"));
          s = 3;
          a = 0;
          b = 0;
        } );
      ( 5,
        {
          l =
            Expr.Juxtaposition
              (Expr.Txt "a", Expr.Juxtaposition (Expr.Txt "c", Expr.Txt "e"));
          s = 3;
          a = 0;
          b = 0;
        } );
      ( 6,
        {
          l =
            Expr.Juxtaposition
              (Expr.Txt "a", Expr.Juxtaposition (Expr.Txt "c", Expr.Txt "e"));
          s = 3;
          a = 0;
          b = 0;
        } );
      ( 7,
        {
          l =
            Expr.Juxtaposition
              (Expr.Txt "a", Expr.Juxtaposition (Expr.Txt "c", Expr.Txt "e"));
          s = 3;
          a = 0;
          b = 1;
        } );
      ( 8,
        {
          l =
            Expr.Juxtaposition
              (Expr.Txt "a", Expr.Juxtaposition (Expr.Txt "c", Expr.Txt "e"));
          s = 3;
          a = 1;
          b = 1;
        } );
      ( 9,
        {
          l =
            Expr.Juxtaposition
              (Expr.Txt "a", Expr.Juxtaposition (Expr.Txt "c", Expr.Txt "e"));
          s = 3;
          a = 2;
          b = 1;
        } );
    ]
    (Layout_func.to_alist g)

let test_integ_0 () =
  let l0 = Expr.Juxtaposition (Expr.Txt "aaaaa", Expr.Txt " bbbbb") (* 11 *) in
  let l1 = Expr.Stacked (Expr.Txt "aaaaa", Expr.Txt "  bbbbb") (* 7 *) in
  let l = Expr.Choice (l0, l1) in

  let g = Cost.from_expr ~m:20 l in
  Alcotest.(check Helper.alist)
    "m = 20"
    [
      ( 0,
        {
          l = Expr.Juxtaposition (Expr.Txt "aaaaa", Expr.Txt " bbbbb");
          s = 11;
          a = 0;
          b = 0;
        } );
      ( 9,
        {
          l = Expr.Juxtaposition (Expr.Txt "aaaaa", Expr.Txt " bbbbb");
          s = 11;
          a = 0;
          b = 1;
        } );
      ( 10,
        {
          l = Expr.Stacked (Expr.Txt "aaaaa", Expr.Txt "  bbbbb");
          s = 7;
          a = 1;
          b = 0;
        } );
      (13, { l = Expr.Txt "  bbbbb"; s = 7; a = 0; b = 1 });
      (15, { l = Expr.Txt "aaaaa"; s = 5; a = 0; b = 1 });
    ]
    (Layout_func.to_alist g);

  let g = Cost.from_expr ~m:10 l in
  Alcotest.(check Helper.alist)
    "m = 10"
    [
      ( 0,
        {
          l = Expr.Stacked (Expr.Txt "aaaaa", Expr.Txt "  bbbbb");
          s = 7;
          a = 1;
          b = 0;
        } );
      (3, { l = Expr.Txt "  bbbbb"; s = 7; a = 0; b = 1 });
      (5, { l = Expr.Txt "aaaaa"; s = 5; a = 0; b = 1 });
    ]
    (Layout_func.to_alist g)

let () =
  let open Alcotest in
  run "Ordre"
    [
      ("txt", [ test_case "0" `Quick test_txt0; test_case "1" `Quick test_txt1 ]);
      ( "stack",
        [ test_case "0" `Quick test_stack_0; test_case "1" `Quick test_stack_1 ]
      );
      ("juxtpos", [ test_case "0" `Quick test_juxtpos0 ]);
      ("choise", [ test_case "0" `Quick test_choise_0 ]);
      ("e", [ test_case "article" `Quick test_e_0 ]);
      ( "integ",
        [
          test_case "article" `Quick test_article_0;
          test_case "article" `Quick test_integ_0;
        ] );
    ]
