(** Authors 	
	CHEVAIS Benjamin 
	      && 
	SAUZEAU YANNIS
**)

(* Uncomment the load of your ocaml version *)

(* OCaml version 4.02.3 *)
(* #directory "libraries/4.02.3/";; *)

(* OCaml version 4.05.0 *)
(* #directory "libraries/4.05.0/";; *)

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

(*** 1 Arbres Binaires de recherche ***)

(* Question 1 *)

let rec list_rnd_create(n : int) : int list =
  
;;

let bst_rnd_create(n : int) : int t_btree =
  bst_lbuild(list_rnd_create(n))
;;

(* Question 2 *)

(* Calcule le déséquilibre d'un arbre *)
let weight_balance(t : int t_btree) : int;;

let average_weight_balance(node_number, tree_number : int, int) : float;;

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
