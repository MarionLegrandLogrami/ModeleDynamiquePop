# logramiSaumon : documentation R pour fct_data.R / fct_graph.R

Ce dossier contient un package R minimal (`logramiSaumon`) dont le seul but
est de donner accès, dans R, à la documentation des fonctions de
`script/fonctions/fct_data.R` et `script/fonctions/fct_graph.R` via l'aide
standard (`?nom_fonction`), comme si ces fonctions venaient d'un package
installé.

## Ça ne change rien à l'existant

- `script/fonctions/fct_data.R` et `fct_graph.R` restent les fichiers de
  référence, utilisés tel quel par tous les autres scripts via `source(...)`.
- Les blocs de commentaires `#'` (roxygen2) ajoutés juste au-dessus de chaque
  fonction dans ces deux fichiers sont de simples commentaires : ils
  n'affectent en rien le comportement de `source()`.
- `rpkg/logramiSaumon/R/fct_data.R` et `fct_graph.R` sont des **copies**
  (régénérées automatiquement, ne pas éditer directement) utilisées
  uniquement pour extraire la documentation.

## Utilisation au quotidien

Une fois le package installé (voir plus bas), dans n'importe quelle
session R :

```r
library(logramiSaumon)
?compute_juv_rmax
?draw_niveau_pop
?keep_good_surf
```

## Mettre à jour la documentation après une modification

Toute modification d'une fonction ou d'un bloc `#'` dans `fct_data.R` /
`fct_graph.R` ne se reflète pas automatiquement dans `?nom_fonction`. Après
une modification, relancer :

```r
source(here::here("script/fonctions/build_docs_package.R"))
```

Ce script :

1. copie `fct_data.R` et `fct_graph.R` dans `rpkg/logramiSaumon/R/` ;
2. régénère `NAMESPACE` et les pages d'aide (`man/*.Rd`) via roxygen2 ;
3. réinstalle le package dans votre bibliothèque R.

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
