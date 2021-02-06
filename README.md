# AlgoProg-Projet

##### Table of Contents
* [Français](#fr)
  * [Présentation](#fr_pr)
  * [Utilisation](#fr_ut)
  * [Compétences acquises](#fr_cp)
  * [Résultat](#fr_rs)
* [English](#en)
  * [Presentation](#en_pr)
  * [Use](#en_u)
  * [Skills acquired](#en_sk)
  * [Result](#en_rs)

<a name="fr"/>

## Français

<a name="fr_pr"/>

### Présentation

Ce projet a été effectué en troisième année du [CMI Informatique](http://formations.univ-poitiers.fr/fr/index/autre-diplome-niveau-master-AM/autre-diplome-niveau-master-AM/cmi-informatique-JD2XQGVY.html) à l'[UFR SFA Université de Poitiers](https://sfa.univ-poitiers.fr/) dans le cadre de l'enseignement [Algorithmique et programmation](http://formations.univ-poitiers.fr/fr/index/autre-diplome-niveau-master-AM/autre-diplome-niveau-master-AM/cmi-informatique-JD2XQGVY/specialite-s5-JD2XSMB7/algorithmique-et-programmation-3-JB1YGKR9.html).

Ce projet a été développé en binôme sous Ubuntu avec [GNU Emacs](https://www.gnu.org/software/emacs/) et le mode [Tuareg](https://github.com/ocaml/tuareg).

<a name="fr_ut"/>

### Utilisation

Pour lancer le projet, il faut installer [opam](https://opam.ocaml.org/doc/Install.html).

Une fois opam installé il faudra télécharger une des versions d'OCaml présent dans le dossier "[code/libraries](https://github.com/SauzeauYannis/AlgoProg-Projet/tree/main/code/libraries). 
Pour cela, lancez les commandes (sous linux) :

```shell
$ opam install 4.11.1

$ opam switch 4.11.1

$ eval $(opam env)
```

Une fois ces commandes exéxutées vous avez la version 4.11.1 d'OCaml et vous pourez tester les programmes "[code/ABR.ml](https://github.com/SauzeauYannis/AlgoProg-Projet/blob/main/code/ABR.ml)" et "[code/AVL.ml](https://github.com/SauzeauYannis/AlgoProg-Projet/blob/main/code/AVL.ml)" en prenant soins de charger la commande :

```ocaml
#directory "libraries/4.11.1/";;
```

L'exemple ci-dessus est valable pour la version 4.11.1 mais vous pouvez le répéter avec une des autres versions disponible avec le mêmes commandes an changeant juste "4.11.1" par la version que vous souhaitez.

<a name="fr_cp"/>

### Compétences acquises

* Types sommes et structures arborescentes
  * Arbre binaire de recherche
  * Arbre AVL
* Algorithme récursif sur les arbres
  * Construction
  * Parcours
  * Ajout/suppresion
  * Recherche
* Optimisation des structures et des algorithmes
  * Compléxité en O(log(n)) pour les l'ajout/suppression/recherche dans un arbre AVL
  * Stockage du déséquillibre dans chaque noeuds d'un arbre AVL

<a name="fr_rs"/>

### Résultat

Nous avons obtenu la note de ?/20. (Résultat en mars)

<a name="en"/>

## English

<a name="en_pr"/>

### Presentation

This project was carried out in the third year of the [CMI Informatique](http://formations.univ-poitiers.fr/fr/index/autre-diplome-niveau-master-AM/autre-diplome-niveau-master-AM/cmi-informatique-JD2XQGVY.html) at the [University of Poitiers](https://www.univ-poitiers.fr/en/) as part of the [Algorithms and programming](http://formations.univ-poitiers.fr/fr/index/autre-diplome-niveau-master-AM/autre-diplome-niveau-master-AM/cmi-informatique-JD2XQGVY/specialite-s5-JD2XSMB7/algorithmique-et-programmation-3-JB1YGKR9.html) teaching programme.

This project was developed in pairs under Ubuntu with [GNU Emacs](https://www.gnu.org/software/emacs/) and [Tuareg](https://github.com/ocaml/tuareg) mode.

<a name="en_u"/>

### Use

To launch the project, [opam](https://opam.ocaml.org/doc/Install.html) must be installed.

Once opam is installed you will need to download one of the versions of OCaml present in the folder "[code/libraries](https://github.com/SauzeauYannis/AlgoProg-Projet/tree/main/code/libraries). 
For that, launch the commands (under linux):

```shell
opam install 4.11.1

opam switch 4.11.1

eval $(opam env)
```

Once these commands are executed you have OCaml version 4.11.1 and you can test the programs "[code/ABR.ml](https://github.com/SauzeauYannis/AlgoProg-Projet/blob/main/code/ABR.ml)" and "[code/AVL.ml](https://github.com/SauzeauYannis/AlgoProg-Projet/blob/main/code/AVL.ml)" by taking care to load the command :


```ocaml
#directory "libraries/4.11.1/";;
```

The above example is valid for version 4.11.1 but you can repeat it with one of the other versions available with the same commands by just changing "4.11.1" to the version you want.

<a name="en_sk"/>

### Skills acquired

* Tagged union and tree structures
  * Binary search tree
  * AVL tree
* Recursive algorithm on trees
  * Construction
  * Tree traversal
  * Addition/deletion
  * Search
* Optimisation of structures and algorithms
  * Complexity in O(log(n)) for add/delete/search in an AVL tree
  * Storage of the balance factors in each node of an AVL tree
  
<a name="en_rs"/>

### Result

We obtained a score of ?/20. (Result in March)
