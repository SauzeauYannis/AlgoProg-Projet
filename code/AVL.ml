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
#load "ABR.cmo";;

open Btree;;
open Bst;;
open ABR;;

(*** 2 Arbres AVL ***)

(** 2.1 Implantaion d'un module AvL **)

(* Question 1 *)

(* Type de l'arbre AVL *)
type 'a avl = (int * 'a) bst;;

(* Déséquilibre d'un arbre AVL *)
let weight_balance(tree : 'a avl) : int =
  if isEmpty(tree)
  then 0
  else let (wb, _) = root(tree) in wb
;;

(* Valeur de la racine d'un arbre AVL *)
let root_val(tree : 'a avl) : int =
  let (_, r) = root(tree) in
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

(* Rééquilibrage d'un avl *)
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
        else failwith "weight_balance(tree) incorrect value"
    else
      let wbd : int = weight_balance(rson(tree)) in
      if wb = -2
      then
        if wbd = -1 || wbd = 0
        then rg(tree)
        else
          if wbd = 1
          then rdg(tree)
          else failwith "weight_balance(tree) incorrect value"
      else failwith "weight_balance(tree) must be in {-2;-1;0;1;2}"
;;

(* Question 3 *)

(* Ajout d'une valeur dans un AVL *)
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

(* Valeur maximale d'un AVL *)
let rec avl_max(tree : 'a avl) : 'a =
  if isEmpty(tree)
  then invalid_arg "avl_max : tree must not be empty"
  else
    let ((wb, v), g, d) = (root(tree), lson(tree), rson(tree)) in
    if isEmpty(d)
    then v
    else avl_max(d)
;;

(* AVL sans son maximum *)
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

(* Suppression d'une valeur dans un AVL *)
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
            let amax = avl_dmax(g) in
            if ((weight_balance(g) = 0 && not(isEmpty(g))) || (weight_balance(amax) = weight_balance(g))) && not(isEmpty(lson(g)))
            then rebalance(rooting((wb, avl_max(g)), amax, d))
            else rebalance(rooting((wb - 1, avl_max(g)), amax, d))
;;

(* Dessine un avl *)
let show_avl(tree : 'a avl) : unit = show((fun (wb, root) -> (string_of_int wb) ^ " " ^ (string_of_int root)), tree);;

(* Création de l'AVL présent dans le cours diapo 12 *)
let a4 = ajt_val(10,ajt_val(14,ajt_val(11,ajt_val(9,ajt_val(7,ajt_val(4,ajt_val(5,ajt_val(2,ajt_val(3,ajt_val(12, empty()))))))))));;
show_avl(a4);;
avl_max(a4);;

(* Suppression successive de la valeur maximale de a4 jusqu'à un arbre vide *)
let a5 = avl_dmax(avl_dmax(avl_dmax(avl_dmax(avl_dmax(avl_dmax(avl_dmax(avl_dmax(avl_dmax(avl_dmax(a4))))))))));;
show_avl(a5);;

(* Suppression des valeurs de a4 dans l'ordre où elles ont étés ajoutés *)
let a6 = suppr_val(12,suppr_val(3,suppr_val(2,suppr_val(2,suppr_val(5,suppr_val(4,suppr_val(7,suppr_val(9,suppr_val(11,suppr_val(14,suppr_val(10, a4)))))))))));;
show_avl(a6);;

(* Question 4 *)

(* Recherche d'une valeur dans un AVL *)
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

(* Recherche de la valeur 10 dans a4 et a5 *)
avl_seek(a4, 10);;
avl_seek(a5, 10);;

(** 2.2 Implantaion d'un module Avl **)

(* Question 1 *)

(* Construit un avl à partir d'une liste d'élem *)
let rec avl_lbuild(l : 'a list) : 'a avl =
  match l with
  | [] -> empty()
  | v::lt -> ajt_val(v, avl_lbuild(lt))
;;

(* Création d'AVL avec une liste *)
show_avl(avl_lbuild([10;14;11;9;7;4;5;3;2;12]));;
show_avl(avl_lbuild([8;14;12;6;5;10]));;

(* Crée des arbres avl à partir d'une suite d'entiers aléatoire *)
let avl_rnd_create(node_number, limit : int * int) : 'a avl =
  Random.self_init();
  let rec list_rnd_create(size, limit : int * int) : int list =
    let rd : int = Random.int limit in
    if size = 0
    then []
    else rd::list_rnd_create(size-1, limit)
  in avl_lbuild(list_rnd_create(node_number, limit))
;;

(* Créé un AVL de 10 noeuds de valeurs aléatoires de 0 à 99 *)
let a6 = avl_rnd_create(10, 100);;
weight_balance(a6);;
show_avl(a6);;

(* Créé un AVL de 50 noeuds de valeurs aléatoires de 0 à 999 *)
let a7 = avl_rnd_create(50, 1000);;
weight_balance(a7);;
show_avl(a7);;

(* Créé un AVL de 1 000 noeuds de valeurs aléatoires de 0 à 9 999 *)
let a8 = avl_rnd_create(1000, 10000);;
weight_balance(a8);;

(* Calcul le temps d'éxécution des valeur d'ajout de suppression et de recherche pour un AVL de 10 000 noeuds de valeurs aléatoires de 0 à 99 999 *)
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

(* Rééquilibrage d'un avl en comptant le nombre de rotations *)
let rec rebalance_bis(tree : 'a avl) : ('a avl * int) =
  let ((wb, v), g, d) = (root(tree), lson(tree), rson(tree)) in
  if wb = 0 || wb = 1 || wb = -1
  then (tree, 0)
  else
    let wbg : int = weight_balance(g) in
    if wb = 2
    then
      if wbg = 1 || wbg = 0
      then (rd(tree), 1)
      else
        if wbg = -1
        then (rgd(tree), 1) 
        else failwith "weight_balance(tree) incorrect value"
    else
      let wbd : int = weight_balance(rson(tree)) in
      if wb = -2
      then
        if wbd = -1 || wbd = 0
        then (rg(tree), 1)
        else
          if wbd = 1
          then (rdg(tree), 1)
          else failwith "weight_balance(tree) incorrect value"
      else failwith "weight_balance(tree) must be in {-2;-1;0;1;2}"
;;

(* ajout d'un noeud dans un avl en comptant le nombre de rotations *)
let rec ajt_val_bis(elem, tree: 'a * 'a avl) : ('a avl * int) =
  if isEmpty(tree)
  then (rooting((0, elem), empty(), empty()), 0)
  else
    let ((wb, v), g, d) = (root(tree), lson(tree), rson(tree)) in
    if elem < v
    then
      let (ng, cg) = ajt_val_bis(elem, g) in
      let (wbg, wbng) = (weight_balance(g), weight_balance(ng)) in
      if isEmpty(g) || (wbg = 0 && wbng <> 0)
      then rebalance_bis(rooting((wb + 1, v), ng, d))
      else (rooting((wb, v), ng, d), cg)
    else
      if elem > v
      then
        let (nd, cd) = ajt_val_bis(elem, d) in
        let (wbd, wbnd) = (weight_balance(d), weight_balance(nd)) in
        if isEmpty(d) || (wbd = 0 && wbnd <> 0)
        then rebalance_bis(rooting((wb - 1, v), g, nd))
        else (rooting((wb, v), g, nd), cd)
      else (tree, 0)                         
;;

(* Construit un avl à partir d'une liste d'élem  en comptant le nombre de rotations *)
let rec avl_lbuild_bis(l : 'a list) : ('a avl * int) =
  match l with
  | [] -> (empty(), 0)
  | v::lt -> let (tree, cpt) = avl_lbuild_bis(lt) in
             let (t, c) = ajt_val_bis(v, tree) in
             (t, c + cpt)
;;

(* Calcule le nombre moyen de rotation de plusieurs avl avec des sous-suites ordonnées *)
let average_rotation_sl(nb_sl, tree_number, limit, order, func : int * int * int * (int -> int) * (int * int * (int -> int) -> int list)) : float =
  let rec sum(nb_sl, tree_number, limit, order, func) : int =
    if tree_number = 0
    then 0
    else
      let (tree, cpt) = avl_lbuild_bis(func(nb_sl, limit, order)) in
      cpt + sum(nb_sl, tree_number-1, limit, order, func)
  in (float_of_int (sum(nb_sl, tree_number, limit, order, func))) /. (float_of_int tree_number)
;;

(* Moyenne du nombre de rotations pour 100 AVL créé à partir de 5 sous-suites de longueurs aléatoires entre 1 et 5 *)
average_rotation_sl(5, 100, 100, (function x -> x * x), list_rnd_create_rnd_sl);;
average_rotation_sl(5, 100, 100, (function x -> x * 2), list_rnd_create_rnd_sl);;
average_rotation_sl(5, 100, 100, (function x -> x + 2), list_rnd_create_rnd_sl);;

(* Moyenne du nombre de rotations pour 100 AVL créé à partir de 5 sous-suites de longueurs aléatoires entre 1 et 5 *)
average_rotation_sl(5, 100, 100, (function x -> x * x), list_rnd_create_regular_sl);;
average_rotation_sl(5, 100, 100, (function x -> x * 2), list_rnd_create_regular_sl);;
average_rotation_sl(5, 100, 100, (function x -> x + 2), list_rnd_create_regular_sl);;

(* Moyenne du nombre de rotations pour 100 AVL créé à partir de 5 sous-suites de longueurs aléatoires entre 1 et 5 *)
average_rotation_sl(5, 100, 100, (function x -> x * x), list_rnd_create_inc_sl);;
average_rotation_sl(5, 100, 100, (function x -> x * 2), list_rnd_create_inc_sl);;
average_rotation_sl(5, 100, 100, (function x -> x + 2), list_rnd_create_inc_sl);;

(* Moyenne du nombre de rotations pour 100 AVL créé à partir de 5 sous-suites de longueurs aléatoires entre 1 et 5 *)
average_rotation_sl(5, 100, 100, (function x -> x * x), list_rnd_create_dec_sl);;
average_rotation_sl(5, 100, 100, (function x -> x * 2), list_rnd_create_dec_sl);;
average_rotation_sl(5, 100, 100, (function x -> x + 2), list_rnd_create_dec_sl);;
