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

open Btree;;
open Bst;;

(*** 2 Arbres AVL ***)

(** 2.1 Implantaion d'un module Av1 **)

(* Question 1 *)

(* Rotation gauche d'un arbre binaire *)
let rg(tree : 'a bst) : 'a bst =
  if(isEmpty(tree))
  then empty()
  else 
    if(isEmpty(rson(tree)))
    then tree
    else let (p, u, s) = (root(tree), lson(tree), rson(tree)) in
         let (q, v, w) = (root(s), lson(s), rson(s)) in
         rooting(q, rooting(p, u, v), w)
;;

let a1 = bst_lbuild([8;6;7;3;5]);;
show_int_btree(a1);;
show_int_btree(rg(a1));;

(* Rotation droite d'un arbre binaire *)
let rd(tree : 'a bst) : 'a bst =
  if(isEmpty(tree))
  then empty()
  else 
    if(isEmpty(lson(tree)))
    then tree
    else let (q, s, w) = (root(tree), lson(tree), rson(tree)) in
         let (p, u, v) = (root(s), lson(s), rson(s)) in
         rooting(p, u, rooting(q, v, w))
;;

show_int_btree(rg(rd(a1)));;

(* Rotation gauche droite d'un arbre binaire *)
let rgd(tree : 'a bst) : 'a bst =
  if(isEmpty(tree))
  then empty()
  else 
    if(isEmpty(lson(tree)) && isEmpty(lson(tree)))
    then tree
    else let (q, s, w) = (root(tree), lson(tree), rson(tree)) in
         rd(rooting(q, rg(s), w))
;;

let a2 = bst_lbuild([6;8;7;3;5;12;10]);;
show_int_btree(a2);;
show_int_btree(rgd(a2));;

(* Rotation droite gauche d'un arbre binaire *)
let rdg(tree : 'a bst) : 'a bst =
  if(isEmpty(tree))
  then empty()
  else 
    if(isEmpty(lson(tree)) && isEmpty(lson(tree)))
    then tree
    else let (q, s, w) = (root(tree), lson(tree), rson(tree)) in
         rg(rooting(q, s, rd(w)))
;;

let a3 = bst_lbuild([6;8;7;12;10;3;5]);;
show_int_btree(a3);;
show_int_btree(rdg(a3));;

(* Question 2 *)

(* type somme t_btree + int (déséquilibre) à définir *)
type avl = ;;

(* Calcul du déséquilibre d'un noeud *)
let imbalance(tree : 'a bst) : int =

;;

(* Rééquilibrage d'un arbre de recherche *)
let rebalance(tree : avl) : avl =

;;

(* Question 3 *)

(* Question 4 *)

(** 2.2 Implantaion d'un module Av1 **)

(* Question 1 *)

(* Question 2 *)
