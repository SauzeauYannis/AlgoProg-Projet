(** Authors 	
	CHEVAIS Benjamin 
	      && 
	SAUZEAU Yannis
**)

(* Uncomment the load of your ocaml version *)

(* OCaml version 4.02.3 *)
(* #directory "libraries/4.02.3/";; *)

(* OCaml version 4.05.0 *)
#directory "libraries/4.05.0/";; 

(* OCaml version 4.08.1 *)
#directory "libraries/4.08.1/";;

(* OCaml version 4.10.0 *)
(* #directory "libraries/4.10.0/";; *)

(* OCaml version 4.11.1 *)
(* #directory "libraries/4.11.1/";; *)

#load "btree.cmo";;
#load "bst.cmo";;
#load "ABR.cmo";;

open Btree;;
open Bst;;
open ABR;;

(*** 2 Arbres AVL ***)

(** 2.1 Implantaion d'un module Av1 **)

(* Question 1 *)

(* Type de l'arbre AVL *)
type 'a avl = (int * 'a) bst;;

(* Déséquilibre d'un arbre AVL *)
let weight_balance(tree : 'a avl) : int =
  if isEmpty(tree)
  then 0
  else let (wb, r) = root(tree) in wb
;;

(* Valeur de la racine d'un arbre AVL *)
let root_val(tree : 'a avl) : int =
  let (wb, r) = root(tree) in
  r
;;

(* Rotation gauche d'un AVL *)
let rg(tree : 'a avl) : 'a avl =
  if not(isEmpty(tree)) && not(isEmpty(rson(tree)))
  then let ((wbp, p), u, s) = (root(tree), lson(tree), rson(tree)) in
       let ((wbq, q), v, w) = (root(s), lson(s), rson(s)) in
       let (nwbp, nwbq) =
         if wbq = 0
         then (-1, 1)
         else (0, 0) in
       rooting((nwbq, q), rooting((nwbp, p), u, v), w)
  else failwith "rotation gauche"
;;

(* Rotation droite d'un AVL *)
let rd(tree : 'a avl) : 'a avl =
  if not(isEmpty(tree)) && not(isEmpty(lson(tree)))
  then let ((wbq, q), s, w) = (root(tree), lson(tree), rson(tree)) in
       let ((wbp, p), u, v) = (root(s), lson(s), rson(s)) in
       let (nwbq, nwbp) =
         if wbp = 0
         then (1, -1)
         else (0, 0) in
       rooting(((nwbp, p), u, rooting((nwbq, q), v, w)))
  else failwith "rotation droite"
;;

(* Rotation gauche droite d'un AVL *)
let rgd(tree : 'a avl) : 'a avl =
  if not(isEmpty(tree)) && not(isEmpty(lson(tree))) && not(isEmpty(rson(lson(tree))))
  then let (r, s, w) = (root_val(tree), lson(tree), rson(tree)) in
       let (p, t, s2) = (root_val(s), lson(s), rson(s)) in
       let ((wbq, q), u, v) = (root(s2), lson(s2), rson(s2)) in
       let (nwbr, nwbp) =
         if wbq = 0
         then (0, 0)
         else
           if wbq > 0
           then (-1, 0)
           else (0, 1)
       in rooting((0, q), rooting((nwbp, p), t, u), rooting((nwbr, r), v, w))
  else failwith "rotation gauche droite"
;;

(* Rotation droite gauche d'un AVL *)
let rdg(tree : 'a avl) : 'a avl =
  if not(isEmpty(tree)) && not(isEmpty(rson(tree))) && not(isEmpty(lson(rson(tree))))
  then let (r, t, s) = (root_val(tree), lson(tree), rson(tree)) in
       let (p, s2, w) = (root_val(s), lson(s), rson(s)) in
       let ((wbq, q), u, v) = (root(s2), lson(s2), rson(s2)) in
       let (nwbr, nwbp) =
         if wbq = 0
         then (0, 0)
         else
           if wbq > 0
           then (0, -1)
           else (1, 0)
       in rooting((0, q), rooting((nwbr, r), t, u), rooting(((nwbp, p), v, w)))
  else failwith "rotation droite gauche"
;;

(* Question 2 *)

(* Rééquilibrage d'un arbre de recherche *)
let rec rebalance(tree : 'a avl) : 'a avl =
  let ((wb, v), g, d) = (root(tree), lson(tree), rson(tree)) in
  if wb = 0 || wb = 1 || wb = -1
  then tree
  else
    let wbg : int = weight_balance(g) in
    if wb = 2
    then
      if wbg = 1 || wbg = 0
      then rd(tree) 
      else
        if wbg = -1
        then rgd(tree) 
        else failwith "1."
    else
      let wbd : int = weight_balance(rson(tree)) in
      if wb = -2
      then
        if wbd = -1 || wbd = 0
        then rg(tree)
        else
          if wbd = 1
          then rdg(tree)
          else failwith "2."
      else failwith "weight_balance(tree) must be in {-2;-1;0;1;2}"
;;

(* Question 3 *)

(* ajout d'un noeud dans un AVL *)
let rec ajt_val(elem, tree : 'a * 'a avl) : 'a avl =
  if isEmpty(tree)
  then rooting((0, elem), empty(), empty())
  else
    let ((wb, v), g, d) = (root(tree), lson(tree), rson(tree)) in
    if elem < v
    then
      let ng = ajt_val(elem, g) in
      let (wbg, wbng) = (weight_balance(g), weight_balance(ng)) in
      if isEmpty(g) || (wbg = 0 && wbng <> 0)
      then rebalance(rooting((wb + 1, v), ng, d))
      else rooting((wb, v), ng, d)
    else
      if elem > v
      then
        let nd = ajt_val(elem, d) in
        let (wbd, wbnd) = (weight_balance(d), weight_balance(nd)) in
        if isEmpty(d) || (wbd = 0 && wbnd <> 0)
        then rebalance(rooting((wb - 1, v), g, nd))
        else rooting((wb, v), g, nd)
      else tree                          
;;

(* max d'un avl *)
let rec avl_max(tree : 'a avl) : 'a =
  if isEmpty(tree)
  then invalid_arg "avl_max : tree must not be empty"
  else
    let ((wb, v), g, d) = (root(tree), lson(tree), rson(tree)) in
    if isEmpty(d)
    then v
    else avl_max(d)
;;

(* avl sans son max *)
let rec avl_dmax(tree : 'a avl) : 'a avl =
  if isEmpty(tree)
  then invalid_arg "avl_dmax : tree must not be empty"
  else
    let ((wb, v), g, d) = (root(tree), lson(tree), rson(tree)) in
    if isEmpty(d)
    then g
    else
      if (weight_balance(d) = 0 || weight_balance(d) = 1) && not(isEmpty(rson(d)))
      then rooting((wb, v), g, avl_dmax(d))
      else rebalance(rooting((wb + 1, v), g, avl_dmax(d)))
;;

(* suppression d'un noeud dans un AVL *)
let rec suppr_val(elem, tree : 'a * 'a avl) : 'a avl =
  if isEmpty(tree)
  then empty()
  else
    let ((wb, v), g, d) = (root(tree), lson(tree), rson(tree)) in
    if elem < v
    then
      if weight_balance(g) = 0 && not(isEmpty(g)) && not(isEmpty(lson(g)))
      then rooting((wb, v), suppr_val(elem, g), d)
      else rebalance(rooting((wb - 1, v), suppr_val(elem, g), d))
    else
      if elem > v
      then
        if weight_balance(d) = 0 && not(isEmpty(d)) && not(isEmpty(rson(d)))
        then rooting((wb, v), g, suppr_val(elem, d))
        else rebalance(rooting((wb + 1, v), g, suppr_val(elem, d)))
      else
        if isEmpty(d)
        then g
        else
          if isEmpty(g)
          then d
          else
            if weight_balance(g) = 0 && not(isEmpty(g)) && not(isEmpty(rson(g)))
            then rebalance(rooting((wb, avl_max(g)), avl_dmax(g), d))
            else rebalance(rooting((wb - 1, avl_max(g)), avl_dmax(g), d))
;;

(* Dessine un avl *)
let show_avl(tree : 'a avl) : unit = show((fun (wb, root) -> (string_of_int wb) ^ " " ^ (string_of_int root)), tree);;

let a4 = ajt_val(10,ajt_val(14,ajt_val(11,ajt_val(9,ajt_val(7,ajt_val(4,ajt_val(5,ajt_val(2,ajt_val(3,ajt_val(12, empty()))))))))));;
show_avl(a4);;
avl_max(a4);;

let a5 = avl_dmax(avl_dmax(avl_dmax(avl_dmax(avl_dmax(avl_dmax(avl_dmax(avl_dmax(avl_dmax(avl_dmax(a4))))))))));;
show_avl(a5);;

let a6 = suppr_val(12,suppr_val(3,suppr_val(2,suppr_val(2,suppr_val(5,suppr_val(4,suppr_val(7,suppr_val(9,suppr_val(11,suppr_val(14,suppr_val(10, a4)))))))))));;
show_avl(a6);;

let a7 = suppr_val(11,suppr_val(4,suppr_val(5,a4)));;
show_avl(a7);;

(* Question 4 *)

let rec avl_seek(t, e : 'a avl * 'a) : bool =
  if isEmpty(t)
  then false
  else
    if e < root_val(t)
    then avl_seek(lson(t), e)
    else
      if e > root_val(t)
      then avl_seek(rson(t), e)
      else true
;;
  
avl_seek(a4, 10);;
avl_seek(a5, 10);;

(** 2.2 Implantaion d'un module Av1 **)

(* Question 1 *)

(* Construit un ABR à partir d'une liste d'élem avec des insertions aux feuilles *)
let rec avl_lbuild(l : 'a list) : 'a avl =
  match l with
  | [] -> empty()
  | v::lt -> ajt_val(v, avl_lbuild(lt))
;;

show_avl(avl_lbuild([10;14;11;9;7;4;5;3;2;12]));;
show_avl(avl_lbuild([8;14;12;6;5;10]));;

(* Crée des arbres avl à partir d'une suite d'entiers *)
let avl_rnd_create(node_number, limit : int * int) : 'a avl =
  Random.self_init();
  let rec list_rnd_create(size, limit : int * int) : int list =
    let rd : int = Random.int limit in
    if size = 0
    then []
    else rd::list_rnd_create(size-1, limit)
  in avl_lbuild(list_rnd_create(node_number, limit))
;;

let a6 = avl_rnd_create(10, 100);;
weight_balance(a6);;
show_avl(a6);;

let a7 = avl_rnd_create(50, 1000);;
weight_balance(a7);;
show_avl(a7);;

let a8 = avl_rnd_create(1000, 10000);;
weight_balance(a8);;

let rec test(num : int) : unit =
  if num = 0
  then ()
  else
    let wb = weight_balance(avl_rnd_create(1000, 10000)) in
    if wb > 1 || wb < -1
    then failwith ""
    else test(num-1)
;;

test(100);;

let cmpl(node_number, limit : int * int) : float * float * float =
  let a = avl_rnd_create(node_number, limit) in
  let t1 = Sys.time() in
  let tmp = ajt_val(5000, a) in
  let t2 = Sys.time() -. t1 in
  let tmp2 = avl_seek(a, 5000) in
  let t3 = Sys.time() -. t1 -. t2 in
  let tmp3 = suppr_val(5000, a) in
  let t4 = Sys.time() -. t1 -. t2 -. t3 in
  (t2, t3, t4)
;;

cmpl(10000, 100000);;

let a9 = avl_rnd_create(10000, 100000);;

let t = Sys.time() in
    let a10 = ajt_val(5000, a9) in
    Printf.printf "Execution time: %f secondsn\n"
      (Sys.time() -. t);
    let t = Sys.time() in
    let res = avl_seek(a10, 5000) in
    Printf.printf "Execution time: %f secondsn\n"
      (Sys.time() -. t);
    let t = Sys.time() in
    let a11 = suppr_val(5000, a10) in
    Printf.printf "Execution time: %f secondsn\n"
      (Sys.time() -. t);
    a11
;;
    

(* Question 2 *)

(* Calcule la moyenne de déséquilibre de plusieurs avl avec des sous-suites ordonnées *)
let average_weight_balance_sl(nb_sl, tree_number, limit, order, func : int * int * int * (int -> int) * (int * int * (int -> int) -> int list)) : float =
  let rec sum(nb_sl, tree_number, limit, order, func) : int =
    if tree_number = 0
    then 0
    else weight_balance(avl_lbuild(func(nb_sl, limit, order))) + sum(nb_sl, tree_number-1, limit, order, func)
  in (float_of_int (sum(nb_sl, tree_number, limit, order, func))) /. (float_of_int tree_number)
;;

average_weight_balance_sl(5, 100, 100, (function x -> x * x), list_rnd_create_rnd_sl);;
average_weight_balance_sl(5, 100, 100, (function x -> x * 2), list_rnd_create_rnd_sl);;
average_weight_balance_sl(5, 100, 100, (function x -> x + 2), list_rnd_create_rnd_sl);;

average_weight_balance_sl(5, 100, 100, (function x -> x * x), list_rnd_create_regular_sl);;
average_weight_balance_sl(5, 100, 100, (function x -> x * 2), list_rnd_create_regular_sl);;
average_weight_balance_sl(5, 100, 100, (function x -> x + 2), list_rnd_create_regular_sl);;

average_weight_balance_sl(5, 100, 100, (function x -> x * x), list_rnd_create_inc_sl);;
average_weight_balance_sl(5, 100, 100, (function x -> x * 2), list_rnd_create_inc_sl);;
average_weight_balance_sl(5, 100, 100, (function x -> x + 2), list_rnd_create_inc_sl);;

average_weight_balance_sl(5, 100, 100, (function x -> x * x), list_rnd_create_dec_sl);;
average_weight_balance_sl(5, 100, 100, (function x -> x * 2), list_rnd_create_dec_sl);;
average_weight_balance_sl(5, 100, 100, (function x -> x + 2), list_rnd_create_dec_sl);;
