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
  let (wb, r) = root(tree) in
  wb
;;

(* Valeur de la racine d'un arbre AVL *)
let root_val(tree : 'a avl) : int =
  let (wb, r) = root(tree) in
  r
;;

(* Rotation gauche d'un arbre binaire *)
let rg(tree : 'a avl) : 'a avl =
  if not(isEmpty(tree)) && not(isEmpty(rson(tree)))
  then let (p, u, s) = (root_val(tree), lson(tree), rson(tree)) in
       let (q, v, w) = (root_val(s), lson(s), rson(s)) in
       rooting((0, q), rooting((0, p), u, v), w)
  else failwith "rotation gauche"
;;

(* Rotation droite d'un arbre binaire *)
let rd(tree : 'a avl) : 'a avl =
  if not(isEmpty(tree)) && not(isEmpty(lson(tree)))
  then let (q, s, w) = (root_val(tree), lson(tree), rson(tree)) in
       let (p, u, v) = (root_val(s), lson(s), rson(s)) in
       rooting((0, p), u, rooting((0, q), v, w))
  else failwith "rotation droite"
;;

(* Rotation gauche droite d'un arbre binaire *)
let rgd(tree : 'a avl) : 'a avl =
  if not(isEmpty(tree)) && not(isEmpty(lson(tree))) && not(isEmpty(rson(lson(tree))))
  then let (r, s, w) = (root_val(tree), lson(tree), rson(tree)) in
       let (p, t, s2) = (root_val(s), lson(s), rson(s)) in
       let (q, u, v) = (root(s2), lson(s2), rson(s2)) in
       rooting(q, rooting((0, p), t, u), rooting((-weight_balance(s2), r), v, w))
  else failwith "rotation gauche droite"
;;

(* Rotation droite gauche d'un arbre binaire *)
let rdg(tree : 'a avl) : 'a avl =
  if not(isEmpty(tree)) && not(isEmpty(rson(tree))) && not(isEmpty(lson(rson(tree))))
  then let (r, t, s) = (root_val(tree), lson(tree), rson(tree)) in
       let (p, s2, w) = (root_val(s), lson(s), rson(s)) in
       let (q, u, v) = (root(s2), lson(s2), rson(s2)) in
       rooting(q, rooting((0, r), t, u), rooting((-weight_balance(s), p), v, w))   
  else failwith "rotation droite gauche"
;;

(* Question 2 *)

(* Rééquilibrage d'un arbre de recherche *)
let rebalance(tree : 'a avl) : 'a avl =
  let wb : int = weight_balance(tree) in
  if wb = 0 || wb = 1 || wb = -1
  then
    if not(isEmpty(lson(tree))) && not(isEmpty(rson(tree)))
    then rooting((weight_balance(lson(tree)) - weight_balance(rson(tree)), root_val(tree)), lson(tree), rson(tree))
    else tree
  else
    let wbl : int = weight_balance(lson(tree)) in
    if wb = 2
    then
      if wbl = 1
      then rd(tree)
      else
        if wbl = -1
        then rgd(tree)
        else rooting((wb-1, root_val(tree)), lson(tree), rson(tree))
    else
      let wbr : int = weight_balance(rson(tree)) in
      if wb = -2
      then
        if wbr = -1
        then rg(tree)
        else
          if wbr = 1
          then rdg(tree)
          else rooting((wb+1, root_val(tree)), lson(tree), rson(tree))
      else failwith "3. weight_balance(tree): value incorrect"
;;

(* Question 3 *)

(* ajout d'un noeud dans un AVL *)
let rec ajt_val(elem, tree : 'a * 'a avl) : 'a avl =
  if isEmpty(tree)
  then rooting((0, elem), empty(), empty())
  else let (v, g, d) = (root_val(tree), lson(tree), rson(tree)) and
           wb : int = weight_balance(tree) in
       if elem < v
       then rebalance(rooting((wb+1, v), ajt_val(elem, g), d))
       else
         if elem > v
         then rebalance(rooting((wb-1, v), g, ajt_val(elem, d)))
         else tree                          
;;

(* avl sans son max *)
let rec avl_dmax(tree : 'a avl) : 'a avl =
  if isEmpty(tree)
  then invalid_arg "max : tree must not be empty"
  else
    let (v, g, d) = (root_val(tree), lson(tree), rson(tree)) and
        wb : int = weight_balance(tree) in
    if isEmpty(d)
    then rooting((wb+1, v), g, empty())
    else rebalance(rooting((wb, v), g,  avl_dmax(d)))
;;

(* ajout d'un noeud dans un AVL *)
let rec suppr_val(elem, tree : 'a * 'a avl) : 'a avl =
  if isEmpty(tree)
  then empty()
  else
    let (v, g, d) = (root_val(tree), lson(tree), rson(tree)) and
        wb : int = weight_balance(tree) in
    if elem < v
    then rebalance(rooting((wb, v), suppr_val(elem, g), d))
    else
      if elem > v
      then rebalance(rooting((wb, v), g, suppr_val(elem, d)))
      else
        if isEmpty(g) && isEmpty(d)
        then rooting((wb+1, v), g, empty())
        else
          if not(isEmpty(d))
          then rooting((wb-1, v), empty(), d)
          else rebalance(rooting(max_v(g), avl_dmax(g), d))
;;

(* Dessine un avl *)
let show_avl(tree : 'a avl) : unit = show((fun (wb, root) -> (string_of_int wb) ^ " " ^ (string_of_int root)), tree);;

let a4 = ajt_val(9,ajt_val(7,ajt_val(4,ajt_val(5,ajt_val(2,ajt_val(3,ajt_val(12, empty())))))))
;;
show_avl(a4);;
let a4 = ajt_val(10,
                 ajt_val(14,
                         ajt_val(11,
                                 ajt_val(9,
                                         ajt_val(7,
                                                 ajt_val(4,
                                                         ajt_val(5,
                                                                 ajt_val(2,
                                                                         ajt_val(3,
                                                                                 ajt_val(12, empty())
                                                                           )
                                                                   )
                                                           )
                                                   )
                                           )
                                   )
                           )
                   )
           )
;;
show_avl(a4);;
weight_balance(a4);;

let a5 = suppr_val(4,
                   suppr_val(7,
                             suppr_val(9,
                                       suppr_val(11,
                                                 suppr_val(10,
                                                           suppr_val(14,a4)
                                                   )
                                         )
                               )
                     )
           )
;;
show_avl(a5);;

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
show_avl(avl_lbuild([1;2;3;4;5;6;7;8;9]));;

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

let a7 = avl_rnd_create(100, 1000);;
weight_balance(a7);;

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
