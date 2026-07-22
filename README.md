# ModeleDynamiquePop

Modèle dynamique de population du saumon atlantique de l'Allier (Projet INRAe - LOGRAMI) : reconstitution rétrospective de la population, indicateurs PLAGEPOMI et scénarios de projection, à partir d'un modèle bayésien ajusté sous OpenBUGS.

## Structure du dépôt

- `script/analyse_retro/` : reconstitution rétrospective de la population (juvéniles, adultes, taux de renouvellement...). Contient notamment `Indicateurs Plagepomi.Rmd`, qui calcule et met en forme les 4 indicateurs PLAGEPOMI (niveau de population, taux de renouvellement de la population sauvage, diagnostic de conservation, part des juvéniles sauvages).
- `script/projections/` : scénarios de projection (arrêt des déversements, ouverture de Poutès, amélioration de la survie, mortalité en dévalaison, continuité écologique...).
- `script/fonctions/` : fonctions R communes au projet - calculs (`fct_data.R`) et graphiques (`fct_graph.R`) - documentées via roxygen2 et exposées comme package R local pour la consultation de l'aide (voir `rpkg/`).
- `script/lateX/` : rapports LaTeX/Sweave (rapports de transfert, rapport indicateurs 2021, comparaisons de versions du modèle...).
- `script/integration_q_model/` : intégration des débits (scénarios climatiques DRIAS) dans le modèle.
- `script/update/` : script pour la mise à jour du modèle.
- `rpkg/modelDyn/` : package R local donnant accès à la documentation (`?nom_fonction`) des fonctions de `fct_data.R` / `fct_graph.R`, sans dupliquer le code source. Voir `rpkg/README.md` pour l'installation et la mise à jour.

## Données

Les données volumineuses (chaînes CODA issues du modèle bayésien, figures générées) ne sont pas versionnées (`data/` et `img/` sont dans `.gitignore`) : elles sont stockées dans un dossier externe au dépôt.


## Documentation des fonctions R

Les fonctions de `script/fonctions/fct_data.R` et `fct_graph.R` sont documentées (roxygen2) et consultables via `?nom_fonction` une fois le package local installé - voir `rpkg/README.md`.
