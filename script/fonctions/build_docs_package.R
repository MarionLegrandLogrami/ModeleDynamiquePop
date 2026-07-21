######################################################################################
# Reconstruit le package local "logramiSaumon", qui sert uniquement à donner ###
# accès à la documentation R de fct_data.R / fct_graph.R via ?nom_fonction.       ###
#                                                                                  ###
# A relancer après toute modification de fct_data.R ou fct_graph.R (ajout d'une   ###
# fonction, modification d'un bloc roxygen #' au-dessus d'une fonction, etc.) :   ###
# la doc affichée par ?nom_fonction ne se met PAS à jour toute seule.            ###
#                                                                                  ###
# Les fichiers sources de référence restent script/fonctions/fct_data.R et       ###
# fct_graph.R (utilisés par source() dans tous les autres scripts) : ce script    ###
# se contente d'en copier une version dans rpkg/logramiSaumon/R/ pour que        ###
# roxygen2 puisse en extraire la documentation, sans jamais modifier les          ###
# fichiers sources.                                                               ###
######################################################################################

if (!requireNamespace("roxygen2", quietly = TRUE)) {
  stop("Le package roxygen2 est nécessaire (install.packages('roxygen2')).")
}

pkg_dir <- here::here("rpkg/logramiSaumon")

# 1. Copie des fichiers sources dans le dossier R/ du package (source de vérité
#    = script/fonctions/, jamais l'inverse)
file.copy(
  here::here("script/fonctions/fct_data.R"),
  file.path(pkg_dir, "R", "fct_data.R"),
  overwrite = TRUE
)
file.copy(
  here::here("script/fonctions/fct_graph.R"),
  file.path(pkg_dir, "R", "fct_graph.R"),
  overwrite = TRUE
)

# 2. Génère NAMESPACE + man/*.Rd à partir des blocs roxygen2 #'
roxygen2::roxygenise(pkg_dir)

# roxygenise() charge le package (via pkgload) pour l'introspecter, ce qui
# empêche install.packages() de le réinstaller juste après ("package en cours
# d'utilisation") : on le décharge avant de continuer.
if ("logramiSaumon" %in% loadedNamespaces()) {
  try(unloadNamespace("logramiSaumon"), silent = TRUE)
}

# 3. Installe le package dans la bibliothèque R de l'utilisateur : après ça,
#    library(logramiSaumon) + ?nom_fonction fonctionnent comme pour n'importe
#    quel package installé.
install.packages(pkg_dir, repos = NULL, type = "source")

message("Package logramiSaumon reconstruit et installé. Utilisez library(logramiSaumon) puis ?nom_fonction.")
