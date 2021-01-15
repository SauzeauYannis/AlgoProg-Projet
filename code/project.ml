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
open Random;;

(*** 1 Arbres Binaires de recherche ***)

(* Question 1 *)

let rec list_rnd_create(node_number, limit : int * int) : int list =
  Random.self_init();
  let rd : int = Random.int limit in
  if(node_number = 0)
  then []
  else rd::list_rnd_create(node_number-1, limit)
;;

list_rnd_create(10, 100);;

let rec bst_rnd_create(node_number, limit : int * int) : 'a t_btree =
  bst_lbuild(list_rnd_create(node_number, limit))
;;

let b1 = bst_rnd_create(10, 100);;
show_int_btree(b1);;

(* Question 2 *)

(* Calcule le hauteur d'un arbre *)
let rec height(tree : 'a t_btree) : int =
  if isEmpty(tree) || (isEmpty(rson(tree)) && isEmpty(lson(tree)))
  then 0
  else (
    if height(rson(tree)) > height(lson(tree))
    then 1 + height(rson(tree))
    else 1 + height(lson(tree))
  )
;;

(* Calcule le déséquilibre d'un arbre *)
let rec weight_balance(tree : int t_btree) : int =
  if isEmpty(tree)
  then 0
  else weight_balance(lson(tree)) + weight_balance(rson(tree)) +
       (height(lson(tree)) - height(rson(tree)))
;;

weight_balance(b1);;

let average_weight_balance(node_number, tree_number, limit : int * int * int) : float;;

(* Question 3 *)

(* Question 4 *)

(*** 2 Arbres AVL ***)

(** 2.1 Implantaion d'un module Av1 **)

(* Question 1 *)

(* Question 2 *)

(* Question 3 *)

(* Question 4 *)

(** 2.2 Implantaion d'un module Av1 **)

(* Question 1 *)

(* Question 2 *)
