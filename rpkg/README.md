# modelDyn : documentation R pour fct_data.R / fct_graph.R

Ce dossier contient un package R minimal (`modelDyn`) dont le seul but est de
donner accès, dans R, à la documentation des fonctions de
`script/fonctions/fct_data.R` et `script/fonctions/fct_graph.R` via l'aide
standard (`?nom_fonction`), comme si ces fonctions venaient d'un package
installé.

## Ça ne change rien à l'existant

- `script/fonctions/fct_data.R` et `fct_graph.R` restent les fichiers de
  référence, utilisés tel quel par tous les autres scripts via `source(...)`.
- Les blocs de commentaires `#'` (roxygen2) ajoutés juste au-dessus de chaque
  fonction dans ces deux fichiers sont de simples commentaires : ils
  n'affectent en rien le comportement de `source()`.
- `rpkg/modelDyn/R/fct_data.R` et `fct_graph.R` sont des **copies**
  (régénérées automatiquement, ne pas éditer directement) utilisées
  uniquement pour extraire la documentation.

## Installation (à faire une fois, dans VOTRE session R/RStudio)

```r
source(here::here("script/fonctions/build_docs_package.R"))
```

**Important** : lancez cette commande depuis la session R que vous utilisez
au quotidien (RStudio). Le package s'installe dans la bibliothèque R de la
session qui exécute la commande (`.libPaths()[1]`) - si vous la lancez depuis
un autre outil/terminal utilisant une autre installation de R, le package
sera invisible depuis votre RStudio habituel (`library(modelDyn)` échouera).
Le script affiche le dossier d'installation utilisé si vous voulez vérifier.

**Ne pas faire** `install.packages("modelDyn")` seul : cette commande cherche
le package sur le CRAN (dépôt public), où il n'existe pas puisque c'est un
package local. Il faut passer par `build_docs_package.R` ci-dessus, qui
installe depuis le dossier local `rpkg/modelDyn` (`repos = NULL, type =
"source"`).

## Utilisation au quotidien

Une fois le package installé :

```r
library(modelDyn)
?compute_juv_rmax
?draw_niveau_pop
?keep_good_surf
```

## Mettre à jour la documentation après une modification

Toute modification d'une fonction ou d'un bloc `#'` dans `fct_data.R` /
`fct_graph.R` ne se reflète pas automatiquement dans `?nom_fonction`. Après
une modification, relancer (toujours depuis votre session R habituelle) :

```r
source(here::here("script/fonctions/build_docs_package.R"))
```

Ce script :

1. copie `fct_data.R` et `fct_graph.R` dans `rpkg/modelDyn/R/` ;
2. régénère `NAMESPACE` et les pages d'aide (`man/*.Rd`) via roxygen2 ;
3. réinstalle le package dans la bibliothèque R de la session courante.

## Ajouter une nouvelle fonction

Ajoutez-la normalement dans `fct_data.R` ou `fct_graph.R`, avec un bloc de
documentation juste au-dessus, par exemple :

```r
#' Titre court de la fonction
#'
#' Description plus longue si utile.
#'
#' @param x Description du paramètre x.
#' @param y Description du paramètre y. Défaut : 10.
#' @return Description de ce que la fonction renvoie.
#' @export
ma_fonction <- function(x, y = 10) {
  ...
}
```

Puis relancez `build_docs_package.R` comme ci-dessus.

## Pré-requis

Le package `roxygen2` doit être installé (`install.packages("roxygen2")`).
Aucune compilation n'est nécessaire (fonctions R pures).

## En cas de problème : "library(modelDyn)" ne trouve pas le package

Cela signifie que `build_docs_package.R` a été exécuté depuis une session R
différente de celle où vous tapez `library(modelDyn)`. Vérifiez dans les deux
sessions le résultat de `.libPaths()[1]` - s'ils diffèrent, relancez
`build_docs_package.R` directement depuis la session (RStudio) où vous voulez
utiliser `?nom_fonction`.
