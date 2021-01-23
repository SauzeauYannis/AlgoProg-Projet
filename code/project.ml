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

(** Calcule le déséquilibre d'un arbre binaire de recherche **)
(* tree : arbre binaire de recherche *)
let rec weight_balance(tree : int bst) : int =
  let max(n1, n2 : int * int) : int =
    if n1 > n2
    then n1
    else n2
  in let rec height(t : int bst) : int =
       if isEmpty(t) || (isEmpty(rson(t)) && isEmpty(lson(t)))
       then 0
       else 1 + max(height(rson(t)), height(lson(t)))
     in if isEmpty(tree)
        then 0
        else weight_balance(lson(tree)) + weight_balance(rson(tree)) +
               (height(lson(tree)) - height(rson(tree)))
;;

weight_balance(b1);;

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

(** Calcule la moyenne de déséquilibre de plusieurs arbres binaires de recherche **)
(* node_number : nombre de noeuds par arbre *)
(* tree_number : nombre d'arbres binaire de recherche à générer *)
(* limit : nombre aléatoire maximal *)
let average_weight_balance(node_number, tree_number, limit : int * int * int) : float =
  sum_weight_balance(node_number, tree_number, limit) /. float_of_int(tree_number)
;;

average_weight_balance(10, 20, 100);;

(* Question 3 *)

(** Créé une liste d'entiers aléatoires avec des sous-suites ordonnées de longueurs aléatoire **)
(* size : taille de la liste *)
(* limit : nombre aléatoire maximal *)
(* order : fonctions d'ordre sur les sous-suites *)
let rec list_rnd_create_rnd_sl(size, limit, order : int * int * (int -> int)) : int list =
;;

(** Créé une liste d'entiers aléatoires avec des sous-suites ordonnées de longueurs fixe **)
(* size : taille de la liste *)
(* sl_size : taille des sous-suites *)
(* order : fonctions d'ordre sur les sous-suites *)
let list_rnd_create_regular_sl(size, limit, order : int * int * (int -> int)) : int list =
  let rec found_multiple(n, m : int * int) : int =
    if m = 1
    then -1
    else
      if (n mod m) = 0
      then m
      else found_multiple(n, m-1)
  in let mult : int = found_multiple(size, size-1)
     in if mult = -1
        then invalid_arg "size can't be a prime number"
        else (
          Random.self_init();
          let rec list_create(size, limit, order, tmp : int * int * (int -> int) * int) : int list =
            if size = 0
            then []
            else
              if (size mod mult) == 0
              then (
                let rd : int = Random.int limit in
                rd::list_create(size-1, limit, order, rd))
              else (
                let n : int = order(tmp) in
                n::list_create(size-1, limit, order, n))
          in list_create(size, limit, order, 0))
;;

list_rnd_create_regular_sl(9, 100, function x -> x * 2);;
  
(** Créé une liste d'entiers aléatoires avec des sous-suites ordonnées de longueurs croissantes **)
(* size : taille de la liste *)
(* limit : nombre aléatoire maximal *)
(* order : fonctions d'ordre sur les sous-suites *)
let rec list_rnd_create_inc_sl(size, limit, order : int * int * (int -> int)) : int list =
;;

(** Créé une liste d'entiers aléatoires avec des sous-suites ordonnées de longueurs décroissantes **)
(* size : taille de la liste *)
(* limit : nombre aléatoire maximal *)
(* order : fonctions d'ordre sur les sous-suites *)
let rec list_rnd_create_dec_sl(size, limit, order : int * int * (int -> int)) : int list =
;;

(** Créé un arbre binaire de recherche d'entiers aléatoires avec des sous-suites ordonnées **)
(* node_number : nombre de noeuds *)
(* limit : nombre aléatoire maximal *)
(* order : fonctions d'ordre sur les sous-suites *)
(* func : fonction de création de sous-suites ordonnées *)
let rec bst_sl_rnd_create(node_number, limit, order, func : int * int * (int -> int) * (int * int * (int -> int) -> int list)) : int bst =
    bst_lbuild(func(node_number, limit, order))
;;

let b2 = bst_sl_rnd_create(9, 100, (function x -> x * 2), list_rnd_create_regular_sl);;
show_int_btree(b2);;
let b3 = bst_sl_rnd_create(9, 100, (function x -> x + 2), list_rnd_create_regular_sl);;
show_int_btree(b3);;

(** Calcule la somme de déséquilibre de plusieurs arbres binaires de recherche avec des sous-suites ordonnées **)
(* node_number : nombre de noeuds par arbre *)
(* tree_number : nombre d'arbres binaire de recherche à générer *)
(* limit : nombre aléatoire maximal *)
(* order : fonctions d'ordre sur les sous-suites *)
(* func : fonction de création de sous-suites ordonnées *)
let rec sum_weight_balance_bis(node_number, tree_number, limit, order, func : int * int * int * (int -> int) * (int * int * (int -> int) -> int list)) : float =
  if (tree_number = 0)
  then 0.
  else let weight_balance : float = float_of_int(weight_balance(bst_sl_rnd_create(node_number, limit, order, func))) in
       weight_balance +. sum_weight_balance_bis(node_number, tree_number-1, limit, order, func)
;;

(** Calcule la moyenne de déséquilibre de plusieurs arbres binaires de recherche avec des sous-suites ordonnées **)
(* node_number : nombre de noeuds par arbre *)
(* tree_number : nombre d'arbres binaire de recherche à générer *)
(* limit : nombre aléatoire maximal *)
(* order : fonctions d'ordre sur les sous-suites *)
(* func : fonction de création de sous-suites ordonnées *)
let average_weight_balance_bis(node_number, tree_number, limit, order, func : int * int * int * (int -> int) * (int * int * (int -> int) -> int list)) : float =
  sum_weight_balance_bis(node_number, tree_number, limit, order, func) /. float_of_int(tree_number)
;;

average_weight_balance_bis(9, 20, 100, (function x -> x * x), list_rnd_create_regular_sl);;
average_weight_balance_bis(9, 20, 100, (function x -> x * 2), list_rnd_create_regular_sl);;
average_weight_balance_bis(9, 20, 100, (function x -> x + 2), list_rnd_create_regular_sl);;

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
