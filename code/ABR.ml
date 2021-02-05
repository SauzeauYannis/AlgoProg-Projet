(** Authors 	
	CHEVAIS Benjamin 
	      && 
	SAUZEAU Yannis
**)

(* Décommenter la commande '#directory ...' selon votre version d'OCaml *)

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

open Btree;;  (* Module pour les arbres binaires *)
open Bst;;    (* Module pour les arbres binaires de recherche *)

(*** 1 Arbres Binaires de recherche ***)

(* Question 1 *)

(* Créé un arbre binaire de recherche d'entiers aléatoires *)
let bst_rnd_create(node_number, limit : int * int) : 'a bst =
  Random.self_init();
  let rec list_rnd_create(size, limit : int * int) : int list =
    let rd : int = Random.int limit in
    if size = 0
    then []
    else rd::list_rnd_create(size - 1, limit)
  in bst_lbuild(list_rnd_create(node_number, limit))
;;

(* Arbre à 10 noeuds de valeurs entre 0 et 99 *)
let b1 = bst_rnd_create(10, 100);;
show_int_btree(b1);;

(* Question 2 *)

(* Calcule le déséquilibre d'un arbre binaire de recherche *)
let rec weight_balance(tree : 'a bst) : int =
  let rec height(t : 'a bst) : int =
    if isEmpty(t) || (isEmpty(rson(t)) && isEmpty(lson(t)))
    then 0
    else 1 + max (height(rson(t))) (height(lson(t)))
  in if isEmpty(tree)
     then 0
     else height(lson(tree)) - height(rson(tree))
;;

(* Déséquilibre de l'arbre b1 *)
weight_balance(b1);;

(* Calcule la moyenne de déséquilibre de plusieurs arbres binaires de recherche *)
let average_weight_balance(node_number, tree_number, limit : int * int * int) : float =
  let rec sum(node_number, tree_number, limit : int * int * int) : int =
    if tree_number = 0
    then 0
    else weight_balance(bst_rnd_create(node_number, limit)) + sum(node_number, tree_number-1, limit)
  in (float_of_int (sum(node_number, tree_number, limit))) /. (float_of_int tree_number)
;;

(* Moyenne de déséquilibre de 100 arbres à 10 noeuds de valeurs entre 0 et 99 *)
average_weight_balance(10, 100, 100);;

(* Question 3 *)

(* Créé une liste d'entiers aléatoires avec des sous-suites ordonnées de longueurs aléatoires entre 1 et 5 *)
let rec list_rnd_create_rnd_sl(nb_sl, limit, order : int * int * (int -> int)) : int list =
  if nb_sl = 0
  then []
  else (
    Random.self_init();
    let size_sl = (Random.int 5) + 1 in
    let rec create_list(size_sl, tmp : int * int) : int list =
      if size_sl = 0
      then []
      else let rd = order(tmp) in
           rd::create_list(size_sl - 1, rd)
    in let rdval = Random.int limit in
       create_list(size_sl, rdval) @ list_rnd_create_rnd_sl(nb_sl - 1, limit, order)
  )
;;

(* Liste avec 5 sous-suites de longueurs aléatoires entre 1 et 5 ordonnée par incrémentation *)
list_rnd_create_rnd_sl(5, 100, function x -> x + 1);;

(* Créé une liste d'entiers aléatoires avec des sous-suites ordonnées de longueurs 3 *)
let list_rnd_create_regular_sl(nb_sl, limit, order : int * int * (int -> int)) : int list =
  Random.self_init();
  let rec list_create(nb_sl, size, limit, order, tmp : int * int * int * (int -> int) * int) : int list =
    if size = 0
    then []
    else let n = if (size mod 3) == 0
                 then Random.int limit
                 else order(tmp)
         in n::list_create(nb_sl, size - 1, limit, order, n)
  in list_create(nb_sl, nb_sl * 3, limit, order, 0)
;;

(* Liste avec 5 sous-suites de longueurs 3 ordonnée par incrémentation *)
list_rnd_create_regular_sl(5, 100, function x -> x + 1);;
  
(* Créé une liste d'entiers aléatoires avec des sous-suites ordonnées de longueurs croissantes *)
let rec list_rnd_create_inc_sl(nb_sl, limit, order : int * int * (int -> int)) : int list =
  Random.self_init();
  let rec list_create(nb_sl, size, m, limit, order, tmp : int * int * int * int * (int -> int) * int) : int list =
    if nb_sl = size && m = size
    then []
    else
      if m = size
      then let n : int = Random.int limit
           in n::list_create(nb_sl, size + 1, 1, limit, order, n)
      else let n : int = order(tmp)
           in n::list_create(nb_sl, size, m + 1, limit, order, n)
  in list_create(nb_sl, 0, 0, limit, order, 0)
;;

(* Liste avec 5 sous-suites de longueurs croissantes ordonnée par incrémentation *)
list_rnd_create_inc_sl(5, 100, function x -> x + 1);;

(* Créé une liste d'entiers aléatoires avec des sous-suites ordonnées de longueurs décroissantes *)
let rec list_rnd_create_dec_sl(nb_sl, limit, order : int * int * (int -> int)) : int list =
  Random.self_init();
  let rec list_create(nb_sl, m, limit, order, tmp : int * int * int * (int -> int) * int) : int list =
    if nb_sl = 0
    then []
    else
      if m = nb_sl
      then let n : int = Random.int limit
           in if m = 1
              then n::list_create(nb_sl - 1, m - 1, limit, order, n)
              else n::list_create(nb_sl, m - 1, limit, order, n)
      else
        let n : int = order(tmp)
        in if m = 1
           then n::list_create(nb_sl - 1, nb_sl - 1, limit, order, n)
           else n::list_create(nb_sl, m - 1, limit, order, n)
  in list_create(nb_sl, nb_sl, limit, order, 0)
;;

(* Liste avec 5 sous-suites de longueurs décroissantes ordonnée par incrémentation *)
list_rnd_create_dec_sl(5, 100, function x -> x + 1);;

(* Créé un arbre binaire de recherche d'entiers aléatoires avec des sous-suites ordonnées *)
let bst_sl_rnd_create(nb_sl, limit, order, func : int * int * (int -> int) * (int * int * (int -> int) -> int list)) : int bst =
    bst_lbuild(func(nb_sl, limit, order))
;;

(* Arbre binaire créé à partir de 5 sous-suites de longueurs 3 ordonnée par incrémentation *)
let b2 = bst_sl_rnd_create(5, 100, (function x -> x + 1), list_rnd_create_regular_sl);;
show_int_btree(b2);;

(* Calcule la moyenne de déséquilibre de plusieurs arbres binaires de recherche avec des sous-suites ordonnées *)
let average_weight_balance_sl(nb_sl, tree_number, limit, order, func : int * int * int * (int -> int) * (int * int * (int -> int) -> int list)) : float =
  let rec sum(nb_sl, tree_number, limit, order, func) : int =
    if tree_number = 0
    then 0
    else weight_balance(bst_sl_rnd_create(nb_sl, limit, order, func)) + sum(nb_sl, tree_number-1, limit, order, func)
  in (float_of_int (sum(nb_sl, tree_number, limit, order, func))) /. (float_of_int tree_number)
;;

(* Moyenne du déséquilibre pour 100 arbres créé à partir de 5 sous-suites de longueurs aléatoires entre 1 et 5 *)
average_weight_balance_sl(5, 100, 100, (function x -> x * x), list_rnd_create_rnd_sl);;
average_weight_balance_sl(5, 100, 100, (function x -> x * 2), list_rnd_create_rnd_sl);;
average_weight_balance_sl(5, 100, 100, (function x -> x + 2), list_rnd_create_rnd_sl);;

(* Moyenne du déséquilibre pour 100 arbres créé à partir de 5 sous-suites de longueurs 3 *)
average_weight_balance_sl(5, 100, 100, (function x -> x * x), list_rnd_create_regular_sl);;
average_weight_balance_sl(5, 100, 100, (function x -> x * 2), list_rnd_create_regular_sl);;
average_weight_balance_sl(5, 100, 100, (function x -> x + 2), list_rnd_create_regular_sl);;

(* Moyenne du déséquilibre pour 100 arbres créé à partir de 5 sous-suites de longueurs croissantes *)
average_weight_balance_sl(5, 100, 100, (function x -> x * x), list_rnd_create_inc_sl);;
average_weight_balance_sl(5, 100, 100, (function x -> x * 2), list_rnd_create_inc_sl);;
average_weight_balance_sl(5, 100, 100, (function x -> x + 2), list_rnd_create_inc_sl);;

(* Moyenne du déséquilibre pour 100 arbres créé à partir de 5 sous-suites de longueurs décroissantes *)
average_weight_balance_sl(5, 100, 100, (function x -> x * x), list_rnd_create_dec_sl);;
average_weight_balance_sl(5, 100, 100, (function x -> x * 2), list_rnd_create_dec_sl);;
average_weight_balance_sl(5, 100, 100, (function x -> x + 2), list_rnd_create_dec_sl);;
