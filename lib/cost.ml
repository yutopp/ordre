(*
 * Copyright yutopp 2021 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

let alpha = 1

let beta = 1

(* <‘txt’> *)
let base txt ~m =
  let s = String.length txt in

  let l = Expr.Txt txt in
  let g = Layout_func.empty () in
  if s < m then
    let g = Layout_func.add g 0 ~l ~s ~a:0 ~b:0 in
    let g = Layout_func.add g (m - s) ~l ~s ~a:0 ~b:beta in
    g
  else
    (* TODO: maybe a is different *)
    let g = Layout_func.add g 0 ~l ~s ~a:(s - m) ~b:beta in
    g

let stacked g1 g2 : Layout_func.t =
  Map.merge_skewed g1 g2 ~combine:(fun ~key:k e1 e2 ->
      let Layout_func.{ l = l1; s = s1; a = a1; b = b1 } = e1 in
      let Layout_func.{ l = l2; s = s2; a = a2; b = b2 } = e2 in
      Layout_func.
        {
          l = Expr.Stacked (l1, l2);
          s = (if s1 > s2 then s1 else s2);
          a =
            Layout_func.calc_v ~a:a1 ~b:b1 ~dist:0
            + Layout_func.calc_v ~a:a2 ~b:b2 ~dist:0
            + alpha;
          b = b1 + b2;
        })

(* TODO: fix implementation. Reduce evaluation cost *)
let juxtaposition ~m g1 g2 : Layout_func.t =
  let k1 = Layout_func.knots g1 in
  let k2 = Layout_func.knots g2 in

  (* K = K1 ∪ {k − t | k ∈ K2 and s1(k − t) = t} *)
  let kt =
    Set.to_list k2
    |> List.map ~f:(fun k ->
           let ts = List.range ~start:`inclusive ~stop:`inclusive 0 k in
           List.filter_map ts ~f:(fun t ->
               try if Layout_func.s g1 (k - t) = t then Some (k - t) else None
               with Not_found_s _ -> None))
    |> List.join
  in
  let k = Set.union k1 (Set.of_list (module Int) kt) in

  let g = Layout_func.empty () in
  let g =
    Set.fold k ~init:g ~f:(fun g k ->
        let s1 = Layout_func.s g1 k in
        let k' = k + s1 in

        let l = Expr.Juxtaposition (Layout_func.l g1 k, Layout_func.l g2 k') in
        let s = Layout_func.s g1 k + Layout_func.s g2 k' in
        let a =
          Layout_func.v g1 k + Layout_func.v g2 k' - (beta * max (k' - m) 0)
        in
        let b =
          Layout_func.b g1 k + Layout_func.b g2 k'
          - (beta * if k' >= m then 1 else 0)
        in
        Layout_func.add g k ~l ~s ~a ~b)
  in
  g

let choice g1 g2 =
  let k1 = Layout_func.knots g1 in
  let k2 = Layout_func.knots g2 in
  let l = Set.union k1 k2 in
  let k_with_kai =
    Set.to_list l
    |> List.map ~f:(fun k ->
           ( k,
             Int.to_float (Layout_func.v g2 k - Layout_func.v g1 k)
             /. Int.to_float (Layout_func.b g1 k - Layout_func.b g2 k) ))
  in
  let k_ =
    List.filter_map k_with_kai ~f:(fun (k, kai) ->
        if Float.is_nan kai || Float.is_inf kai then None
        else
          let c = Float.round_up (Float.of_int k +. kai) |> Int.of_float in
          if c < Layout_func.KnotSet.at_plus l k then Some c else None)
  in
  let k = Set.union l (Set.of_list (module Int) k_) in

  let g = Layout_func.empty () in
  let g =
    Set.fold k ~init:g ~f:(fun g k ->
        let l_mu =
          if
            Layout_func.v g1 k < Layout_func.v g2 k
            || Layout_func.v g1 k = Layout_func.v g2 k
               && Layout_func.b g1 k <= Layout_func.b g2 k
          then g1
          else g2
        in

        let l = Layout_func.l l_mu k in
        let s = Layout_func.s l_mu k in
        let a = Layout_func.a l_mu k in
        let b = Layout_func.b l_mu k in

        Layout_func.add g k ~l ~s ~a ~b)
  in
  g

let rec from_expr ~m expr : Layout_func.t =
  match expr with
  | Expr.Unit -> failwith ""
  | Expr.Juxtaposition (l1, l2) ->
      juxtaposition ~m (from_expr ~m l1) (from_expr ~m l2)
  | Expr.Stacked (l1, l2) -> stacked (from_expr ~m l1) (from_expr ~m l2)
  | Expr.Txt txt -> base ~m txt
  | Expr.Choice (l1, l2) -> choice (from_expr ~m l1) (from_expr ~m l2)
