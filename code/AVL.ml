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
type 'a avl = 'a bst;;

(* Rotation gauche d'un arbre binaire *)
let rg(tree : 'a avl) : 'a avl =
  if isEmpty(tree)
  then empty()
  else 
    if isEmpty(rson(tree))
    then tree
    else let (p, u, s) = (root(tree), lson(tree), rson(tree)) in
         let (q, v, w) = (root(s), lson(s), rson(s)) in
         rooting(q, rooting(p, u, v), w)
;;

let a1 = bst_lbuild([8;6;7;3;5]);;
show_int_btree(a1);;
show_int_btree(rg(a1));;

(* Rotation droite d'un arbre binaire *)
let rd(tree : 'a avl) : 'a avl =
  if isEmpty(tree)
  then empty()
  else 
    if isEmpty(lson(tree))
    then tree
    else let (q, s, w) = (root(tree), lson(tree), rson(tree)) in
         let (p, u, v) = (root(s), lson(s), rson(s)) in
         rooting(p, u, rooting(q, v, w))
;;

show_int_btree(rg(rd(a1)));;

(* Rotation gauche droite d'un arbre binaire *)
let rgd(tree : 'a avl) : 'a avl =
  if isEmpty(tree)
  then empty()
  else 
    if isEmpty(lson(tree)) && isEmpty(lson(tree))
    then tree
    else let (q, s, w) = (root(tree), lson(tree), rson(tree)) in
         rd(rooting(q, rg(s), w))
;;

let a2 = bst_lbuild([6;8;7;3;5;12;10]);;
show_int_btree(a2);;
show_int_btree(rgd(a2));;

(* Rotation droite gauche d'un arbre binaire *)
let rdg(tree : 'a avl) : 'a avl =
  if isEmpty(tree)
  then empty()
  else 
    if isEmpty(lson(tree)) && isEmpty(lson(tree))
    then tree
    else let (q, s, w) = (root(tree), lson(tree), rson(tree)) in
         rg(rooting(q, s, rd(w)))
;;

let a3 = bst_lbuild([6;8;7;12;10;3;5]);;
show_int_btree(a3);;
show_int_btree(rdg(a3));;

(* Question 2 *)

(* Calcul du déséquilibre d'un arbre binaire de recherche *)
let weight_balance(tree : 'a avl) : int =
  let rec height(t : 'a avl) : int =
    if isEmpty(t) || (isEmpty(rson(t)) && isEmpty(lson(t)))
    then 0
    else 1 + max (height(rson(t))) (height(lson(t)))
  in if isEmpty(tree)
     then 0
     else height(lson(tree)) - height(rson(tree))
;;

(* Rééquilibrage d'un arbre de recherche *)
let rebalance(tree : 'a avl) : 'a avl =
  let wb : int = weight_balance(tree) in
  if wb > - 2 && wb < 2
  then tree
  else
    if wb = 2
    then
      if weight_balance(lson(tree)) = 1
      then rd(tree)
      else rgd(tree)
       else
         if weight_balance(rson(tree)) = 1
         then rg(tree)
         else rdg(tree)
;;

(* Question 3 *)

(* ajout d'un noeud dans un AVL *)
let rec ajt_val(elem, tree : 'a * 'a avl) : 'a avl =
  if(isEmpty(tree))
  then rooting(elem, empty(), empty())
  else let (v, g, d) = (root(tree), lson(tree), rson(tree)) in
       if(elem < v)
       then rebalance(rooting(v, ajt_val(elem, g), d))
       else
         if(elem > v)
         then rebalance(rooting(v, g, ajt_val(elem, d)))
         else rebalance(rooting(v, g, d))                           
;;

(* avl sans son max *)
let rec avl_dmax(tree : 'a avl) : 'a avl =
  if isEmpty(tree)
  then invalid_arg "max : tree must not be empty"
  else
    let (v, g, d) = (root(tree), lson(tree), rson(tree)) in
    if isEmpty(d)
    then g
    else rebalance(rooting(v, g,  avl_dmax(d)))
;;

(* ajout d'un noeud dans un AVL *)
let rec suppr_val(elem, tree : 'a * 'a avl) : 'a avl =
  if isEmpty(tree)
  then empty()
  else
    let (v, g, d) = (root(tree), lson(tree), rson(tree)) in
    if elem < v
    then rebalance(rooting(v, suppr_val(elem, g), d))
    else
      if elem > v
      then rebalance(rooting(v, g, suppr_val(elem, d)))
      else
        if isEmpty(g) && isEmpty(d)
        then g
        else
          if not(isEmpty(d))
          then d
          else rebalance(rooting(max_v(g), avl_dmax(g), d))
;;

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
show_int_btree(a4);;
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
show_int_btree(a5);;

(* Question 4 *)

bst_seek(a4, 10);;
bst_seek(a5, 10);;

(** 2.2 Implantaion d'un module Av1 **)

(* Question 1 *)

(* Construit un ABR à partir d'une liste d'élem avec des insertions aux feuilles *)
let rec avl_lbuild(l : 'a list) : 'a avl =
  match l with
  | [] -> empty()
  | v::lt -> ajt_val(v, bst_lbuild(lt))
;;

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
show_int_btree(a6);;
weight_balance(a6);;

(* Question 2 *)
