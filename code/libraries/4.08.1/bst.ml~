open Btree;;
open List;;

type 'a bst = 'a t_btree;;

(* Recherche d'un élement dans un abr *)

let rec bst_seek(t, e : 'a bst * 'a) : bool =
  if isEmpty(t)
  then false
  else
    if e < root(t)
    then bst_seek(lson(t), e)
    else
      if e > root(t)
      then bst_seek(rson(t), e)
      else true
;;

(* Insère aux feuilles un élément *)

let rec bst_linsert(t, e : 'a bst * 'a) : 'a bst =
  if isEmpty(t)
  then rooting(e, empty(), empty())
  else
    if e = root(t)
    then t
    else
      if e < root(t)
      then rooting(root(t), bst_linsert(lson(t), e), rson(t))
      else rooting(root(t), lson(t), bst_linsert(rson(t), e))
;;

(* Construit un ABR à partir d'une liste d'élem avec des insertions aux feuilles *)

let rec bst_lbuild(l : 'a list) : 'a bst =
  match l with
  | [] -> empty()
  | v::lt -> bst_linsert(bst_lbuild(lt), v)
;;

(* Retourne le plus grand élément *)

let rec max(t : 'a bst) : 'a =
  if isEmpty(t)
  then invalid_arg "max : tree must not be empty"
  else
    if isEmpty(rson(t))
    then root(t)
    else max(rson(t))
;;

(* Retourne l'arbre sans son plus grand élément *)

let rec dmax(t : 'a bst) : 'a bst =
  if isEmpty(t)
  then invalid_arg "max : tree must not be empty"
  else
    if isEmpty(rson(t))
    then lson(t)
    else rooting(root(t), lson(t), dmax(rson(t)))
;;

(* Supprime un élement dans un ABR *)

let rec bst_delete(e, t : 'a * 'a bst) : 'a bst =
  if isEmpty(t)
  then empty()
  else
    let (v, g, d) = (root(t), lson(t), rson(t)) in
    if e < v
    then rooting(v, bst_delete(e, g), d)
    else
      if e > v
      then rooting(v, g, bst_delete(e, d))
      else
        if isEmpty(d)
        then g
        else
          if isEmpty(g)
          then d
          else rooting(max(g), dmax(g), d)
;;


(* Réalise la coupe d'un arbre selon un élément de l'arbre  *)

let rec bst_cut(t, e : 'a bst * 'a) : 'a bst * 'a bst =
  if isEmpty(t)
  then (empty(), empty())
  else
    if e < root(t)
    then
      let (g, d) = bst_cut(lson(t), e) in
      (g, rooting(root(t), d, rson(t)))
    else
      let (g, d) = bst_cut(rson(t), e) in
      (rooting(root(t), lson(t), g), d)
;;

(* Insère un élément à la racine *)

let bst_rinsert(t, e : 'a bst * 'a) : 'a bst =
  let (g, d) = bst_cut(t, e) in
  rooting(e, g, d)
;;
