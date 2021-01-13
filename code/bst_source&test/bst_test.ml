(* Uncomment the load of your ocaml version *)

(* OCaml version 4.02.3 *)
(* #directory "libraries/4.02.3/";; *)

(* OCaml version 4.05.0 *)
(* #directory "libraries/4.05.0/";; *)

(* OCaml version 4.08.1 *)
#directory "../libraries/4.08.1/";;

(* OCaml version 4.10.0 *)
(* #directory "libraries/4.10.0/";; *)

(* OCaml version 4.11.1 *)
(* #directory "libraries/4.11.1/";; *)

#load "btree.cmo";;
#load "bst.cmo";;

open Bst;;

(* Construit un ABR à partir d'une liste d'élem avec des insertions aux feuilles *)

let t1 : int bst = bst_lbuild([4;2;3;1;7;9]);;
show_int_btree(t1);;
let t2 : int bst = bst_lbuild([15;4;6;94;125]);;
show_int_btree(t2);;

(* Insère aux feuilles un élément *)

let t3 = bst_linsert(t2, 7);;
show_int_btree(t3);;
let t4 = bst_linsert(t3, 5);;
show_int_btree(t4);;

(* Insère un élément à la racine *)

let t5 = bst_rinsert(t4, 112);;
show_int_btree(t5);;
let t6 = bst_rinsert(t5, 122);;
show_int_btree(t6);;

(* Recherche d'un élement dans un abr *)

bst_seek(t1, 1);;
bst_seek(t2, 7);;

(* Retourne le plus grand élément *)

max(t1);;
max(t6);;

(* Retourne l'arbre sans son plus grand élément *)

let t7 = dmax(t6);;
show_int_btree(t7);;
let t8 = dmax(t1);;
show_int_btree(t8);;

(* Supprime un élement dans un ABR *)

let t9 = bst_delete(122, t6);;
show_int_btree(t9);;
let t10 = bst_delete(1, t1);;
show_int_btree(t10);;

(* Réalise la coupe d'un arbre selon un élément de l'arbre  *)

let (t11, t12) = bst_cut(t6, 6);;
show_int_btree(t11);;
show_int_btree(t12);;
