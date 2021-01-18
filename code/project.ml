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
open List;;

(*** 1 Arbres Binaires de recherche ***)

(* Question 1 *)

(** Créé une liste d'entiers aléatoires **)
(* size : taille de la liste *)
(* limit : nombre aléatoire maximal *)
let rec list_rnd_create(size, limit : int * int) : int list =
  Random.self_init();
  let rd : int = Random.int limit in
  if(size = 0)
  then []
  else rd::list_rnd_create(size-1, limit)
;;

list_rnd_create(10, 100);;

(** Créé un arbre binaire de recherche d'entiers aléatoires **)
(* node_number : nombre de noeuds *)
(* limit : nombre aléatoire maximal *)
let rec bst_rnd_create(node_number, limit : int * int) : 'a bst =
  bst_lbuild(list_rnd_create(node_number, limit))
;;

let b1 = bst_rnd_create(10, 100);;
show_int_btree(b1);;

(* Question 2 *)

(** Retourne le maximum entre 2 nombres **)
(* n1 : nombre 1 *)
(* n2 : nombre 2 *)
let max(n1, n2 : int * int) : int =
  if n1 > n2
  then n1
  else n2
;;

(** Calcule le hauteur d'un arbre binaire de recherche **)
(* tree : arbre binaire de recherche *)
let rec height(tree : 'a bst) : int =
  if isEmpty(tree) || (isEmpty(rson(tree)) && isEmpty(lson(tree)))
  then 0
  else 1 + max(height(rson(tree)), height(lson(tree)))
;;

height(b1);;

(** Calcule le déséquilibre d'un arbre binaire de recherche **)
(* tree : arbre binaire de recherche *)
let rec weight_balance(tree : int bst) : int =
  if isEmpty(tree)
  then 0
  else weight_balance(lson(tree)) + weight_balance(rson(tree)) +
       (height(lson(tree)) - height(rson(tree)))
;;

weight_balance(b1);;

(*

Ces deux fonctions peuvent être regroupé en 1 seule (cf sum_weight_balance)

let rec create_weight_balance_list(node_number, tree_number, limit : int * int * int) : int list =
  if(tree_number = 0)
  then []
  else let tree = bst_rnd_create(node_number, limit) in
       weight_balance(tree)::create_weight_balance_list(node_number, tree_number-1, limit)
;;

let rec sum_list(list : int list) : int =
  if(length(list) = 0)
  then 0
  else hd(list) + sum_list(tl(list))
;;

*)

(** Calcule la somme de déséquilibre de plusieurs arbres binaires de recherche **)
(* node_number : nombre de noeuds par arbre *)
(* tree_number : nombre d'arbres binaire de recherche à générer *)
(* limit : nombre aléatoire maximal *)
let rec sum_weight_balance(node_number, tree_number, limit : int * int * int) : float =
  if (tree_number = 0)
  then 0.
  else let weight_balance : float = float_of_int(weight_balance(bst_rnd_create(node_number, limit))) in
       weight_balance +. sum_weight_balance(node_number, tree_number-1, limit)
;;

(* 

Ca raccourci aussi cette fonction

let average_weight_balance(node_number, tree_number, limit : int * int * int) : float =
  let weight_balance_list = create_weight_balance_list(node_number, tree_number, limit) in
  let sum_weight_balance_list = sum_list(weight_balance_list) in
  float_of_int(sum_weight_balance_list) /. float_of_int(tree_number)
;;
 *)

(** Calcule la moyenne de déséquilibre de plusieurs arbres binaires de recherche **)
(* node_number : nombre de noeuds par arbre *)
(* tree_number : nombre d'arbres binaire de recherche à générer *)
(* limit : nombre aléatoire maximal *)
let average_weight_balance(node_number, tree_number, limit : int * int * int) : float =
  sum_weight_balance(node_number, tree_number, limit) /. float_of_int(tree_number)
;;

average_weight_balance(10, 20, 50);;

(* Question 3 *)

(** Créé une liste d'entiers aléatoires avec des sous-suites ordonnées de longueurs aléatoire **)
(* size : taille de la liste *)
(* limit : nombre aléatoire maximal *)
let rec list_rnd_create_rnd_sublist(size, limit : int * int) : int list =
;;

(** Créé une liste d'entiers aléatoires avec des sous-suites ordonnées de longueurs fixe **)
(* size : taille de la liste *)
(* limit : nombre aléatoire maximal *)
let rec list_rnd_create_regular_sublist(size, limit : int * int) : int list =
;;

(** Créé une liste d'entiers aléatoires avec des sous-suites ordonnées de longueurs croissantes **)
(* size : taille de la liste *)
(* limit : nombre aléatoire maximal *)
let rec list_rnd_create_inc_sublist(size, limit : int * int) : int list =
;;

(** Créé une liste d'entiers aléatoires avec des sous-suites ordonnées de longueurs décroissantes **)
(* size : taille de la liste *)
(* limit : nombre aléatoire maximal *)
let rec list_rnd_create_dec_sublist(size, limit : int * int) : int list =
;;

(** Créé un arbre binaire de recherche d'entiers aléatoires avec des sous-suites ordonnées **)
(* node_number : nombre de noeuds *)
(* limit : nombre aléatoire maximal *)
(* func : fonction de création de sous-suites ordonnées *)
let rec bst_sublist_rnd_create(node_number, limit, func : int * int * (int * int -> int list)) : 'a bst =
    bst_lbuild(func(node_number, limit))
;;

(** Calcule la somme de déséquilibre de plusieurs arbres binaires de recherche avec des sous-suites ordonnées **)
(* node_number : nombre de noeuds par arbre *)
(* tree_number : nombre d'arbres binaire de recherche à générer *)
(* limit : nombre aléatoire maximal *)
(* func : fonction de création de sous-suites ordonnées *)
let rec sum_weight_balance_bis(node_number, tree_number, limit, func : int * int * int * (int * int -> int list)) : float =
  0.
;;

(** Calcule la moyenne de déséquilibre de plusieurs arbres binaires de recherche avec des sous-suites ordonnées **)
(* node_number : nombre de noeuds par arbre *)
(* tree_number : nombre d'arbres binaire de recherche à générer *)
(* limit : nombre aléatoire maximal *)
(* func : fonction de création de sous-suites ordonnées *)
let average_weight_balance_bis(node_number, tree_number, limit, func : int * int * int * (int * int -> int list)) : float =
  0.
;;

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
