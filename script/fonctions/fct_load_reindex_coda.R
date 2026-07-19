###################################################
# Fonction load et réindex CODAs issus du modèle  #
# pour que tous les paramètres soient sur la même #
# plage de temps (de 1 à T)                       #
# à appeler à la place de read.coda               #
###################################################

### Trop pénible de gérer à chaque fois les pb d'indice de temps des différentes codas
### exemple : Poutès qui commence à l'année 12 ou 16
### on créée une fonction de chargement des codas qui charge et réindice si nécessaire 
### les paramètres sur année 1 à T en remplissant avec NA

#dans la fonction load_reindex_coda :
# chain_file correspond au chemin vers le "CODAchain1.txt", 
# index_file correspond au chemin vers le "CODAindex.txt",
library(coda)
library(stringr)
library(crayon)

#on fait une fonction pour choisir la couleur des messages renvoyés par la fonction
#éviter qu'ils sortent en rouge ce qui donne l'impression qd ça défile vite qu'il y a un soucis
info_msg <- function(text, color = "green") {
  col_fun <- switch(color,
                    green = green,
                    blue = blue,
                    cyan = cyan,
                    magenta = magenta,
                    yellow = yellow,
                    red = red,
                    bold = bold,
                    grey = silver,
                    identity)  # fallback : aucun style
  cat(col_fun(text), "\n")
}

load_reindex_coda <- function(chain_file, index_file, t_start = 1, t_end = NULL, fill = NA) {
  if (is.null(t_end)) {
    t_end <- get("T", envir = .GlobalEnv)  # T défini dans l'environnement global
  }
  
  mcmc_obj <- read.coda(chain_file, index_file, quiet = TRUE)
  col_names <- colnames(mcmc_obj)
  
  # Cas 1 : Non indexé (aucun crochet)
  if (!any(grepl("\\[", col_names))) {
    info_msg("CODA non-indicée t. Aucun réindexage effectué.")
    return(mcmc_obj)
  }
  
  # Extraire le préfixe du paramètre
  param_prefixes <- unique(sub("\\[.*", "", col_names))
  if (length(param_prefixes) > 1) {
    stop("Plusieurs préfixes détectés : ", paste(param_prefixes, collapse = ", "))
  }
  param_prefix <- param_prefixes[1]
  
  # Motifs regex
  single_index_pattern <- paste0("^", param_prefix, "\\[([0-9]+)\\]$")
  double_index_pattern <- paste0("^", param_prefix, "\\[([0-9]+),([0-9]+)\\]$")
  
  single_matches <- regexec(single_index_pattern, col_names)
  double_matches <- regexec(double_index_pattern, col_names)
  
  # Cas 2 : [t,k]
  if (any(sapply(regmatches(col_names, double_matches), length) > 0)) {
    parsed <- regmatches(col_names, double_matches)
    param_df <- do.call(rbind, lapply(parsed, function(x) {
      if (length(x) == 3) c(t = as.integer(x[2]), k = as.integer(x[3])) else c(t = NA, k = NA)
    }))
    param_df <- as.data.frame(param_df)
    param_df$col <- col_names
    
    unique_k <- sort(unique(param_df$k[!is.na(param_df$k)]))
    full_times <- t_start:t_end
    
    target_names <- unlist(lapply(unique_k, function(k) {
      paste0(param_prefix, "[", full_times, ",", k, "]")
    }))
    
    existing_cols <- col_names
    missing_cols <- setdiff(target_names, existing_cols)
    
    if (length(missing_cols) > 0) {
      # Réindexation
      n_iter <- nrow(mcmc_obj)
      new_matrix <- matrix(fill, nrow = n_iter, ncol = length(target_names),
                           dimnames = list(NULL, target_names))
      common_cols <- intersect(colnames(mcmc_obj), target_names)
      new_matrix[, common_cols] <- mcmc_obj[, common_cols]
      info_msg("CODA non indicée sur 1:T. CODA réindexée.")
      return(mcmc(new_matrix,
                  start = attr(mcmc_obj, "mcpar")[1],
                  end   = attr(mcmc_obj, "mcpar")[2],
                  thin  = attr(mcmc_obj, "mcpar")[3]))
    } else {
      info_msg("CODA déjà indicée sur 1:T. Aucun réindexage effectué.")
      return(mcmc_obj)
    }
  }
  
  # Cas 3 : [i]
  if (any(sapply(regmatches(col_names, single_matches), length) > 0)) {
    parsed <- regmatches(col_names, single_matches)
    indices <- sapply(parsed, function(x) if (length(x) == 2) as.integer(x[2]) else NA)
    indices <- indices[!is.na(indices)]
    
    full_indices <- t_start:t_end
    target_names <- paste0(param_prefix, "[", full_indices, "]")
    existing_cols <- col_names
    missing_cols <- setdiff(target_names, existing_cols)
    
    if (length(missing_cols) > 0) {
      n_iter <- nrow(mcmc_obj)
      new_matrix <- matrix(fill, nrow = n_iter, ncol = length(target_names),
                           dimnames = list(NULL, target_names))
      common_cols <- intersect(colnames(mcmc_obj), target_names)
      new_matrix[, common_cols] <- mcmc_obj[, common_cols]
      info_msg("CODA non indicée sur 1:T. CODA réindexée.")
      return(mcmc(new_matrix,
                  start = attr(mcmc_obj, "mcpar")[1],
                  end   = attr(mcmc_obj, "mcpar")[2],
                  thin  = attr(mcmc_obj, "mcpar")[3]))
    } else {
      info_msg("CODA déjà indicée sur 1:T. Aucun réindexage effectué.")
      return(mcmc_obj)
    }
  }
  
  # Cas non reconnu
  warning("Format d'indexation non reconnu. Paramètre retourné sans modification.")
  return(mcmc_obj)
}
