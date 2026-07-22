######################################################################################################################
# Script de création des fonctions servant à générer les graphiques pour les sorties du modèle de dynamique de pop ###
# @uteur : Marion LEGRAND - LOGRAMI                                                                                ###
# date : 2022_04_06                                                                                                ###
#                                                                                                                  ###
# Documentation : chaque fonction a un bloc #' (roxygen2) juste au-dessus. Pour que ?nom_fonction fonctionne dans  ###
# R, lancer script/fonctions/build_docs_package.R (à refaire après toute modification de ce fichier) - voir       ###
# rpkg/README.md.                                                                                                  ###
######################################################################################################################

#' Boxplot par année (base R), pour les sorties du modèle
#'
#' Trace un boxplot personnalisé par année (quantiles 5/25/50/75/95\%
#' attendus en colonnes 1:5 de \code{data}), avec gestion des couleurs par
#' secteur/déversement, points isolés, segments et texte additionnels, ligne
#' de moyenne/médiane générale, etc. Fonction "base R" (pas ggplot),
#' utilisée directement ou via \code{\link{boxplot_years_png}}.
#'
#' @param data Matrice/data.frame des quantiles par année (lignes = années, colonnes 1:5 = quantiles 5/25/50/75/95\%).
#' @param years Vecteur des indices d'années (positions dans \code{data}) à tracer.
#' @param yr_deb Indice (dans \code{years}) à partir duquel commencer l'affichage. Défaut : 1 (depuis le début).
#' @param yr_point Indices (dans \code{years}) à afficher comme point unique plutôt que boxplot.
#' @param main_title Titre du graphique. Défaut : aucun titre.
#' @param ylab_txt Titre de l'axe Y. Défaut : texte localisé selon \code{lang}.
#' @param ylim_range Bornes de l'axe Y. Défaut : \code{c(0, 1)}.
#' @param subplot_label Étiquette affichée en haut à droite du graphique (ex. lettre de sous-figure).
#' @param col_box Couleur de boîte par défaut. Défaut : "coral3".
#' @param color_dev Vecteur indicateur (0/1/NA) par année, pour colorer selon le déversement (cf. \code{sector_color}, \code{col_active}/\code{col_inactive}).
#' @param sector_color Si \code{col_active}/\code{col_inactive} non fournis : 1 = palette corail/bisque, autre = palette vert foncé/clair.
#' @param col_active,col_inactive Couleurs personnalisées pour \code{color_dev == 1} / \code{== 0}.
#' @param add_segments Liste de segments additionnels à tracer (chaque élément : \code{x0, y0, x1, y1, col, lty, lwd}).
#' @param add_text Liste de textes additionnels à afficher (chaque élément : \code{x, y, labels, col, cex, pos, offset}).
#' @param year_origin Année de référence (année 1 du modèle = year_origin + 1). Défaut : 1974.
#' @param y_at Positions personnalisées des graduations de l'axe Y. Défaut : graduations automatiques.
#' @param center_line Vecteur (aligné sur \code{years}) de moyennes arithmétiques à tracer comme trait central, à la place de la médiane (colonne 3 de \code{data}) par défaut.
#' @param points_overlay Points observés à superposer (liste avec \code{x, y, pch, col, cex}), ex. comptages exhaustifs quand une station fonctionne.
#' @param mean_line Valeur y d'une ligne rouge horizontale de moyenne/médiane générale, avec étiquette dans la marge de droite.
#' @param mean_line_range Étendue x de \code{mean_line}. Défaut : \code{c(min(years), max(years))}.
#' @param mean_label_digits Nombre de décimales de l'étiquette de \code{mean_line}. Défaut : 5.
#' @param mean_label_line Position (paramètre \code{line} de \code{mtext}) de l'étiquette de \code{mean_line}. Défaut : 0.
#' @param lang Langue des titres par défaut des axes : "fr" ou "en". Défaut : "fr".
#' @return Rien (effet de bord : dessine sur le device graphique actif).
#' @export
custom_plot <- function(data,
                        years,
                        yr_deb = NULL,
                        yr_point = NULL,
                        main_title = NULL,
                        ylab_txt = NULL,
                        ylim_range = c(0, 1),
                        subplot_label = "",
                        col_box = "coral3",
                        color_dev = NULL,
                        sector_color = 1,
                        col_active = NULL,
                        col_inactive = NULL,
                        add_segments = NULL,
                        add_text = NULL,
                        year_origin = 1974,
                        y_at = NULL,
                        center_line = NULL,
                        points_overlay = NULL,
                        mean_line = NULL,
                        mean_line_range = NULL,
                        mean_label_digits = 5,
                        mean_label_line = 0,
                        lang = "fr") {

  # Opérateur fallback
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  # Textes localisés
  default_titles <- list(
    fr = list(xlab = "Années", ylab = "Valeurs"),
    en = list(xlab = "Years", ylab = "Values")
  )

  labels <- default_titles[[lang]]
  if (is.null(labels)) {
    warning("Langue non prise en charge, utilisation du français par défaut.")
    labels <- default_titles[["fr"]]
  }

  # Pas de titre par défaut : main_title reste NULL si non renseigné (plot() n'affiche alors rien)
  ylab_txt <- ifelse(is.null(ylab_txt), labels$ylab, ylab_txt)
  xlab_txt <- labels$xlab

  # Plot vide
  plot(1, 1, type = "n", axes = FALSE,
       xlim = c(min(years) - 0.5, max(years) + 0.5),
       ylim = ylim_range,
       xlab = xlab_txt,
       ylab = ylab_txt,
       main = main_title,
       cex.main = 2,
       cex.lab = 1.5)

  #Pour ne représenter que toutes les 5 années + 1er et dernière année
  # Conversion en années réelles (par ex. 1974 + 1:50, ou year_origin + years
  # si la série démarre plus tard que l'année 1 du modèle, ex. décalage lié à un délai bio)
  real_years <- year_origin + years

  # Sélectionner les indices où les années sont multiples de 5, + la première et dernière année
  ticks_to_show <- which((real_years %% 5 == 0) |
                           real_years == min(real_years) |
                           real_years == max(real_years))

  if (is.null(y_at)) {
    axis(2, las = 1, cex.axis = 1.4, col = "grey55")
  } else {
    axis(2, at = y_at, labels = y_at, las = 1, cex.axis = 1.4, col = "grey55")
  }
  axis(1, at = years[ticks_to_show], labels = real_years[ticks_to_show], las = 1, cex.axis = 1, col = "grey55")

  text(max(years), ylim_range[2], labels = subplot_label, col = "grey55", cex = 1.5)
  yr_deb <- ifelse(is.null(yr_deb),1,yr_deb)

  # Affichage par année
  for (i in seq_along(years)) {
    #gestion des couleurs
    this_col <- if (!is.null(color_dev) && i <= length(color_dev)) {
      if (is.na(color_dev[i])) {
        col_box
      } else if (!is.null(col_active) && !is.null(col_inactive)) {
        if (color_dev[i] == 1) col_active else col_inactive
      } else if (sector_color == 1) {
        if (color_dev[i] == 1) "coral3" else "bisque"
      } else {
        if (color_dev[i] == 1) "darkolivegreen4" else "olivedrab2"
      }
    } else {
      col_box
    }

    if (i < yr_deb) next
    idx <- years[i]

    if (!is.null(yr_point) && i %in% yr_point) {
      # Affichage point unique
      if (!is.na(data[i, 1])) {
        points(i, data[i, 1], pch = 16, col = col_box, cex = 1.5)
      }
    } else {
      # Affichage boxplot
      if (all(!is.na(data[idx, ]))) {
        # Whiskers
        segments(idx - 0.15, data[idx, 5], idx + 0.15, data[idx, 5])
        segments(idx, data[idx, 4], idx, data[idx, 5])
        segments(idx - 0.15, data[idx, 1], idx + 0.15, data[idx, 1])
        segments(idx, data[idx, 2], idx, data[idx, 1])

        # Boîte
        polygon(c(idx - 0.3, idx + 0.3, idx + 0.3, idx - 0.3),
                c(data[idx, 2], data[idx, 2], data[idx, 4], data[idx, 4]),
                col = this_col, border = NA)

        # Trait central : médiane par défaut (5e/25e/50e/75e/95e centiles attendus en
        # colonnes 1:5), ou moyenne arithmétique si center_line est fourni (vecteur
        # aligné sur years) - les deux usages coexistent selon les sorties du modèle
        center_val <- if (!is.null(center_line) && !is.na(center_line[i])) center_line[i] else data[idx, 3]
        segments(idx - 0.3, center_val, idx + 0.3, center_val)
      }
    }
  }

  # Segments supplémentaires
  if (!is.null(add_segments)) {
    for (seg in add_segments) {
      segments(seg$x0, seg$y0, seg$x1, seg$y1,
               col = seg$col %||% "grey35",
               lty = seg$lty %||% 2,
               lwd = seg$lwd %||% 2)
    }
  }

  # Texte additionnel
  if (!is.null(add_text)) {
    for (txt in add_text) {
      text(x = txt$x,
           y = txt$y,
           labels = txt$labels,
           col = txt$col %||% "black",
           cex = txt$cex %||% 1,
           pos = txt$pos %||% NULL,
           offset = txt$offset %||% 0.5)
    }
  }

  # Points observés superposés (ex. comptages exhaustifs quand une station fonctionne)
  if (!is.null(points_overlay)) {
    points(x = points_overlay$x, y = points_overlay$y,
           pch = points_overlay$pch %||% 16,
           col = points_overlay$col %||% "black",
           cex = points_overlay$cex %||% 1)
  }

  # Ligne rouge de moyenne/médiane générale + étiquette (motif très fréquent des
  # sorties de paramètres : une ligne horizontale rouge sur une sous-période, avec
  # sa valeur affichée dans la marge de droite)
  if (!is.null(mean_line)) {
    rng <- mean_line_range %||% c(min(years), max(years))
    segments(x0 = rng[1], y0 = mean_line, y1 = mean_line, x1 = rng[2], col = "red")
    mtext(round(mean_line, mean_label_digits), side = 4, line = mean_label_line, at = mean_line,
          cex = 0.8, col = "red", las = 1)
  }
}





#' PNG boxplot par année (ouvre/ferme le device)
#'
#' Ouvre un device PNG, appelle \code{\link{custom_plot}}, ferme le device.
#' Évite de répéter le trio \code{png(...)}/\code{custom_plot(...)}/\code{dev.off()} partout.
#'
#' @param imgwd Dossier de destination du fichier PNG (avec "/" final).
#' @param filename Nom du fichier PNG (sans extension).
#' @param width,height Dimensions du PNG en pixels. Défaut : 800 x 800.
#' @param ... Arguments transmis à \code{\link{custom_plot}}.
#' @return Rien (effet de bord : écrit le fichier PNG).
#' @export
boxplot_years_png <- function(imgwd, filename, width = 800, height = 800, ...) {
  png(filename = str_c(imgwd, filename, ".png"), width = width, height = height)
  custom_plot(...)
  dev.off()
}

#' PNG trace/densité (2 panneaux) pour un paramètre coda
#'
#' Écrit un PNG à 2 panneaux (trace en haut, densité en bas) pour un
#' paramètre coda/mcmc, via \code{plot.mcmc}.
#'
#' @param param Objet coda/mcmc à tracer.
#' @param imgwd Dossier de destination du fichier PNG (avec "/" final).
#' @param filename Nom du fichier PNG (sans extension).
#' @param width,height Dimensions du PNG en pixels. Défaut : 800 x 800.
#' @return Rien (effet de bord : écrit le fichier PNG).
#' @export
png_trace_density <- function(param, imgwd, filename, width = 800, height = 800) {
  png(filename = str_c(imgwd, filename, ".png"), width = width, height = height)
  par(mfrow = c(2, 1))
  plot(param, density = FALSE, auto.layout = FALSE)
  plot(param, trace = FALSE, auto.layout = FALSE)
  dev.off()
}

#' Sortie standard (PNG + table) d'un paramètre scalaire du modèle
#'
#' Combine \code{\link{png_trace_density}} (PNG trace/densité) et
#' \code{\link[=write_param_table]{write_param_table}} (table récapitulative
#' .tex) pour un paramètre scalaire. C'est le motif le plus fréquent du
#' rapport SortieOpenbugs_parameters.Rnw.
#'
#' @param param Objet coda scalaire (1 colonne).
#' @param imgwd Dossier de destination du PNG (avec "/" final).
#' @param tabwd Dossier de destination du fichier .tex (avec "/" final).
#' @param filename Nom de base du fichier PNG et .tex (sans extension).
#' @param label Label LaTeX de la table. Défaut : \code{filename}.
#' @param caption Légende de la table. Si NULL, générée automatiquement.
#' @param digits Nombre de décimales de la table (NULL = défaut de xtable).
#' @param width,height Dimensions du PNG en pixels. Défaut : 800 x 800.
#' @return Rien (effet de bord : écrit le PNG et le fichier .tex).
#' @export
report_param <- function(param, imgwd, tabwd, filename, label = filename,
                          caption = NULL, digits = NULL, width = 800, height = 800) {
  png_trace_density(param, imgwd, filename, width, height)
  write_param_table(param, tabwd, filename, label = label, caption = caption, digits = digits)
}

#' PNG de corrélogramme entre plusieurs séries/paramètres
#'
#' Écrit un PNG de corrélogramme (\code{corrplot}) entre les colonnes de \code{data}.
#'
#' @param data Data.frame/matrice des séries à corréler (colonnes = variables).
#' @param imgwd Dossier de destination du fichier PNG (avec "/" final).
#' @param filename Nom du fichier PNG (sans extension).
#' @param width,height Dimensions du PNG en pixels. Défaut : 800 x 800.
#' @param method Méthode d'affichage \code{corrplot} (ex. "circle"). Défaut : "circle".
#' @param addCoef.col Couleur des coefficients affichés sur le corrélogramme. Défaut : "black".
#' @param number.cex Taille du texte des coefficients. Défaut : 0.8.
#' @param order Méthode de réordonnancement \code{corrplot} (ex. "AOE"). Défaut : "AOE".
#' @param type Partie affichée de la matrice ("upper"/"lower"/"full"). Défaut : "upper".
#' @param tl.srt Rotation des labels. Défaut : 45.
#' @param use Méthode de gestion des NA pour \code{cor()}. Défaut : "complete.obs".
#' @param cor_method Méthode de corrélation ("pearson"/"spearman"/"kendall"). Défaut : "pearson".
#' @return Rien (effet de bord : écrit le fichier PNG).
#' @export
corrplot_png <- function(data, imgwd, filename, width = 800, height = 800, method = "circle",
                          addCoef.col = "black", number.cex = 0.8, order = "AOE",
                          type = "upper", tl.srt = 45, use = "complete.obs", cor_method = "pearson") {
  png(filename = str_c(imgwd, filename, ".png"), width = width, height = height)
  corrplot(cor(data, use = use, method = cor_method), method = method,
           addCoef.col = addCoef.col, number.cex = number.cex, order = order,
           type = type, tl.srt = tl.srt)
  dev.off()
}

#' Moyenne mobile centrée
#'
#' Calcule une moyenne mobile centrée (par défaut sur 5 valeurs, soit 2
#' valeurs avant et 2 après). Remplace la fonction \code{running.mean()} du
#' package igraph (pour ne pas dépendre de ce package), avec le même
#' comportement.
#'
#' @param x Vecteur numérique.
#' @param n Taille de la fenêtre de moyenne mobile. Défaut : 5.
#' @return Un vecteur de même longueur que \code{x} (classe \code{ts}), NA en
#'   début/fin de série (\code{(n-1)/2} valeurs de chaque côté).
#' @export
running.mean<- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 2)}

#' Moyenne mobile arrière (sur les n dernières valeurs, dont la courante)
#'
#' Contrairement à \code{\link{running.mean}} (moyenne centrée), la valeur en
#' sortie à la position i est la moyenne de \code{x[(i-n+1):i]} - la
#' convention utilisée pour les indicateurs PLAGEPOMI (cf. rapport
#' d'indicateurs 2021 : "L'année qui apparait sur le graphique est la
#' dernière année de chacune des périodes mobiles de 5 ans"). N'a besoin
#' d'aucune donnée future : contrairement à une moyenne centrée, elle atteint
#' donc toujours la dernière année disponible de \code{x}.
#'
#' @param x Vecteur numérique.
#' @param n Taille de la fenêtre de moyenne mobile. Défaut : 5.
#' @return Un vecteur numérique de même longueur que \code{x}, NA pour les
#'   \code{n-1} premières valeurs (fenêtre incomplète).
#' @export
trailing_mean <- function(x, n = 5) {
  as.numeric(stats::filter(x, rep(1 / n, n), sides = 1))
}

#' Graphique ggplot de l'indicateur taux de renouvellement (ligne, moyenne mobile 5 ans)
#'
#' Trace le taux de renouvellement (échelle log naturelle) en moyenne mobile
#' 5 ans, avec un fond rouge/vert selon que le taux est en dessous/au-dessus
#' de 1. Peut optionnellement enregistrer un PNG et annoter une période de
#' simulation (vs rétrospective).
#'
#' @param tx_renouv Matrice du taux de renouvellement en échelle log (itérations x années).
#' @param pngName Nom du fichier PNG à enregistrer (sans extension), si \code{png = TRUE}.
#' @param Tyear Nombre d'années de la période rétrospective. Défaut : la variable globale \code{T}.
#' @param typ_pop Type de population ("sauvage" ou "totale"), utilisé dans le titre du graphique. Défaut : "sauvage".
#' @param simulation Si TRUE, ajoute 20 ans de simulation à \code{Tyear}, une ligne verticale et les annotations "Rétrospectif"/"Simulation". Défaut : FALSE.
#' @param png Si TRUE, enregistre le graphique en PNG dans \code{imgwd} (demandé interactivement si absent). Défaut : FALSE.
#' @param scenario Sous-titre du graphique (nom du scénario). Défaut : aucun.
#' @param pngWidth,pngHeight Dimensions du PNG en pixels si \code{png = TRUE}. Défaut : 1000 x 800.
#' @param annee_debut,annee_fin Bornes d'années affichées sur le graphique. Défaut : 1985 à \code{1974 + Tyear - 6}.
#' @param rect_yStart,rect_yEnd,rect_col Bornes basses/hautes et couleurs des rectangles de fond ("bon"/"mauvais"). Défaut : rouge sous 1, vert au-dessus.
#' @return Une liste de 2 éléments : le graphique ggplot, et \code{mean_data} (moyenne par année, échelle naturelle, avant lissage).
#' @export
draw_indic_tx_ren<-function(tx_renouv,pngName,Tyear=T,typ_pop="sauvage",simulation=FALSE,png=FALSE,scenario=NULL,pngWidth=1000,pngHeight=800,annee_debut=1985,annee_fin=(1974+Tyear-6),rect_yStart=c(0,1),rect_yEnd=c(1,5),rect_col=c("red","green")) {
  #........ Debug ........
    # tx_renouv <- renew_rate_final #renew_rate_w_coef
    # pngName <- "test2"
    # Tyear <- T
    # typ_pop="Totale"  #"sauvage"
    # simulation=TRUE
    # png=TRUE
    # scenario=NULL
    # pngWidth = 1000
    # pngHeight = 800
    # annee_debut=1985
    # annee_fin=(1974+Tyear-6)
    # rect_yStart=c(0,1)
    # rect_yEnd=c(1,5)
    # rect_col=c("red","green")
  #......................
  if(simulation==TRUE){Tyear_def<-Tyear+20}else{Tyear_def<-Tyear}
  data <- stack(as.data.frame(tx_renouv[,7:(Tyear_def-6)]))
  mean_data <- tapply(exp(data[,1]),data[,2],mean)
  x_tx_moyMob <- seq(1,(ncol(tx_renouv[,7:(Tyear_def-6)])-4),1)
  rect_2_col <- data.frame(ystart = rect_yStart, yend = rect_yEnd, col = rect_col)
  #On commence toujours à l'année 1=1985
  yr1<-annee_debut
  yr2<-annee_fin
  #On affiche toujours sur les graphs la 1ere et dernière année et entre on affiche les années finissant par 0 ou 5 sauf si moins de 2 ans d'écarts
  seqx <- data.frame(breaks=seq(1,(Tyear_def-16),1),label=seq(1985,(1974+Tyear_def-6),1))
  seqx_debut <- seqx[1,]
  seqx_fin <- seqx[nrow(seqx),]
  seqx_milieu <- seqx[seqx$label%%5==0,]
  seqx_milieu <- seqx_milieu[seqx_milieu$label-seqx_debut$label>2 & seqx_fin$label-seqx_milieu$label>2,]
  seqx <- rbind(seqx_debut,seqx_milieu,seqx_fin)
  xintercept <- Tyear-16#if(simulation==TRUE){Tyear_def}else{}
  graph<-ggplot()+
          geom_rect(data=rect_2_col, aes(xmin=1, xmax=length(x_tx_moyMob), ymin=ystart,ymax=yend), fill=rect_2_col$col, alpha =0.5) +
          geom_line(aes(x_tx_moyMob,running.mean(mean_data,5)[!is.na(running.mean(mean_data,5))]),size=1) +
          scale_y_continuous(trans="log10",limits=c(0.2,5)) +
          annotation_logticks(sides="l") +
          scale_x_continuous(breaks=seqx$breaks, labels=seqx$label) +
          xlab(iconv("Année de reproduction","UTF8")) +
          ylab("Tx de renouvellement") +
          ggtitle(str_c("Tx de renouvellement de la population ",tolower(typ_pop)," \n(moyenne mobile 5 ans)"),
                  subtitle = scenario) +
          labs(fill = "") +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),legend.position="bottom",text = element_text(size = 20))

  if(simulation==TRUE) {
    graph<-graph +
            geom_vline(aes(xintercept=xintercept), linetype="dotted",size=0.8) +
            annotate(geom="text", x=xintercept/2, y=max(4.77,max(running.mean(mean_data,5)[!is.na(running.mean(mean_data,5))])+0.5), label=iconv("Rétrospectif","UTF8"),fontface =2,size=6) +
            annotate(geom="text", x=xintercept+(seqx_fin$breaks-xintercept)/2, y=max(4.77,max(running.mean(mean_data,5)[!is.na(running.mean(mean_data,5))])+0.5), label="Simulation",fontface =2,size=6)
  } else{graph<-graph}

  if(png==TRUE) {
    if(!exists("imgwd")) {
      print(message(iconv("Entrez dans la console R le chemin du dossier où enregistrer la figure","UTF8")))
      imgwd <- readline()
      pngFilename<-str_c(imgwd,pngName,".png")
    }else{}

    pngFilename<-str_c(imgwd,pngName,".png")
    png(filename=pngFilename,width=pngWidth,height=pngHeight)
    print(graph)
    dev.off()


  } else {}
  output<-list(graph,mean_data)
  return(output)
}

#' Graphique ggplot de l'indicateur taux de renouvellement (variante boxplot)
#'
#' Variante boxplot (par année) de \code{\link{draw_indic_tx_ren}}.
#'
#' @section Attention:
#' Cette fonction contient une erreur de code préexistante (un
#' \code{ggplot() + geom_boxplot} sans parenthèses ni chaînage vers les
#' couches suivantes, aux lignes qui suivent) : elle échoue actuellement à
#' l'exécution. Documentée telle quelle en l'état ; non utilisée par
#' Indicateurs Plagepomi.Rmd (qui utilise \code{\link{draw_indic_tx_ren}} pour
#' l'indicateur "taux de renouvellement de la population sauvage").
#'
#' @param tx_renouv Matrice du taux de renouvellement en échelle log (itérations x années).
#' @param pngName Nom du fichier PNG à enregistrer (sans extension), si \code{png = TRUE}.
#' @param Tyear Nombre d'années de la période rétrospective. Défaut : la variable globale \code{T}.
#' @param typ_pop Type de population ("sauvage" ou "totale"), utilisé dans le titre du graphique. Défaut : "sauvage".
#' @param simulation Si TRUE, ajoute 20 ans de simulation à \code{Tyear} et les annotations "Rétrospectif"/"Simulation". Défaut : FALSE.
#' @param png Si TRUE, enregistre le graphique en PNG dans \code{imgwd} (demandé interactivement si absent). Défaut : FALSE.
#' @param scenario Sous-titre du graphique (nom du scénario). Défaut : aucun.
#' @param pngWidth,pngHeight Dimensions du PNG en pixels si \code{png = TRUE}. Défaut : 1000 x 800.
#' @param annee_debut,annee_fin Bornes d'années affichées sur le graphique. Défaut : 1981 à \code{1974 + Tyear - 6}.
#' @return Une liste de 2 éléments : le graphique ggplot, et \code{mean_data} (moyenne par année).
#' @export
draw_indic_tx_ren_boxplot<-function(tx_renouv,pngName,Tyear=T,typ_pop="sauvage",simulation=FALSE,png=FALSE,scenario=NULL,pngWidth=1000,pngHeight=800,annee_debut=1981,annee_fin=(1974+Tyear-6)) {
  #........ Debug ........
  #tx_renouv <- renew_rate_w_coef_q
  #pngName <- "test2"
  # Tyear <- T
  # typ_pop="Sauvage"  #"sauvage"
  # simulation=TRUE
  # png=TRUE
  # scenario="CE"
  # pngWidth = 1000
  # pngHeight = 800
  # annee_debut=1981
  # annee_fin=(1974+Tyear-6)
  #......................

  if(simulation==TRUE){Tyear_def<-Tyear+20}else{Tyear_def<-Tyear}
  graph<-ggplot()+
    geom_boxplot


    geom_rect(data=rect_2_col, aes(xmin=1, xmax=length(x_tx_moyMob), ymin=ystart,ymax=yend), fill=rect_2_col$col, alpha =0.5) +
    geom_line(aes(x_tx_moyMob,running.mean(mean_data,5)[!is.na(running.mean(mean_data,5))]),size=1) +
    scale_y_continuous(trans="log10",limits=c(0.2,5)) +
    annotation_logticks(sides="l") +
    scale_x_continuous(breaks=seqx$breaks, labels=seqx$label) +
    xlab(iconv("Année de reproduction","UTF8")) +
    ylab("Tx de renouvellement") +
    ggtitle(str_c("Tx de renouvellement de la population ",tolower(typ_pop)," \n(moyenne mobile 5 ans)"),
            subtitle = scenario) +
    labs(fill = "") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),legend.position="bottom",text = element_text(size = 20))

  if(simulation==TRUE) {
    graph<-graph +
      geom_vline(aes(xintercept=xintercept), linetype="dotted",size=0.8) +
      annotate(geom="text", x=xintercept/2, y=max(4.77,max(running.mean(mean_data,5)[!is.na(running.mean(mean_data,5))])+0.5), label=iconv("Rétrospectif","UTF8"),fontface =2,size=6) +
      annotate(geom="text", x=xintercept+(seqx_fin$breaks-xintercept)/2, y=max(4.77,max(running.mean(mean_data,5)[!is.na(running.mean(mean_data,5))])+0.5), label="Simulation",fontface =2,size=6)
  } else{graph<-graph}

  if(png==TRUE) {
    if(!exists("imgwd")) {
      print(message(iconv("Entrez dans la console R le chemin du dossier où enregistrer la figure","UTF8")))
      imgwd <- readline()
      pngFilename<-str_c(imgwd,pngName,".png")
    }else{}

    pngFilename<-str_c(imgwd,pngName,".png")
    png(filename=pngFilename,width=pngWidth,height=pngHeight)
    print(graph)
    dev.off()


  } else {}
  output<-list(graph,mean_data)
  return(output)
}
#
# png(filename=here::here("img/Simulation/2021_09/TauxRenouvellementPopSauvage_ScenarioContinuiteEcologique_boxplot_2022_04_05.png"),width=1000,height=800)
# #png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2019_12_IndicateursSat_model_data2016/TauxRenouvellementPopSauvage_ScenarioContinuiteEcologique_boxplot.png",width=1000,height=800)
# #png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/TauxRenouvellementPopSauvage_ScenarioContinuiteEcologique.png",width=1000,height=800)
# #png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/TauxRenouvellementPopSauvage_coef.png",width=1000,height=800)
# par("mar"=c(7.1, 5.1, 4.1, 2.1))
# plot(1,1,type="n",axes=FALSE,xlim=c((T+0.5-5),(T+20.5-5)),xlab="Years",ylim=c(-2,2),ylab="Taux renouvellement",main="Taux de renouvellement de la population sauvage",cex.lab=1.5)
#
# # trace l'axe des ordonnées
# axis(2,at = seq(-2,2,0.5),labels=seq(-2,2,0.5),cex.axis = 1.2,las = 1,lwd=2,col = "black")
# # trace l'axe des abscisses
# axis(1,at = c((T+1-5),(T+10-5),(T+20-5)),
# 		labels=c((1975+T-5),(1975+T+9-5),(1975+T+19-5)),
# 		cex.axis = 1.2,las = 1,lwd=2,col = "black")
#
# for(i in (T+1-5):(T+20-5)){
# 	#whiskers
# 	#95%
# 	segments(i-0.15,renew_rate_q[i,5],i+0.15,renew_rate_q[i,5])
# 	segments(i,renew_rate_q[i,4],i,renew_rate_q[i,5])
# 	#5%
# 	segments(i-0.15,renew_rate_q[i,1],i+0.15,renew_rate_q[i,1])
# 	segments(i,renew_rate_q[i,2],i,renew_rate_q[i,1])
# 	#boxplot
# 	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(renew_rate_q[i,2],renew_rate_q[i,2],renew_rate_q[i,4],renew_rate_q[i,4]),col="light grey")
# 	#median
# 	segments(i-0.3,renew_rate_q[i,3],i+0.3,renew_rate_q[i,3])
# }
# abline(h=0,col="red")
# mtext(1,text=iconv("Scenario : Projection sans déversement à 20 ans et amélioration de la continuité écologique (montaison + dévalaison)","UTF8"),line=5)
# dev.off()

#===================================================================================
# Fonctions graphiques pour les 4 indicateurs PLAGEPOMI (script/analyse_retro/
# Indicateurs Plagepomi.Rmd), portées depuis Indicateurs PLAGEPOMI.Rnw.
#===================================================================================

#' Palette de couleurs des 4 indicateurs et fiches PLAGEPOMI
#'
#' Couleurs sobres et douces (fonds pastel, ligne principale gris foncé
#' neutre, seuils gris foncé) pour les graphiques et les fiches indicateurs :
#' \code{bg_*} = fonds de zone favorable/défavorable (à utiliser avec
#' \code{alpha} ~0.35-0.6, ce sont des fonds de lecture, pas des éléments
#' graphiques dominants) ; \code{line_main} = couleur de la série principale ;
#' \code{grid_major}/\code{axis_text} = grille/texte des axes ; \code{threshold}
#' = couleur des lignes de seuil ; \code{badge_*} = couleurs (plus soutenues)
#' des cartouches de diagnostic dans les fiches.
#'
#' @format Une liste nommée de codes couleur hexadécimaux.
#' @export
plagepomi_palette <- list(
  bg_red = "#F4C7C3",
  bg_orange = "#F5D0A6",
  bg_yellow = "#F3E7A6",
  bg_green = "#D5E8D1",
  bg_white = "#FFFFFF",
  bg_offwhite = "#FAFAFA",
  line_main = "#20262E",
  grid_major = "#D9DEE3",
  axis_text = "#30343B",
  threshold = "#4B5563",
  badge_red = "#C74343",
  badge_green = "#4F8A4A",
  badge_orange = "#C98024",
  badge_gray = "#6B7280"
)

#' Axe des années harmonisé pour les indicateurs PLAGEPOMI
#'
#' Construit une échelle x en années civiles, avec ticks majeurs (labellisés)
#' tous les 10 ans + grille mineure tous les 5 ans, plus la dernière année
#' réellement disponible si elle ne tombe pas sur une décennie ronde.
#'
#' @param year_first Première année civile de la série tracée.
#' @param year_last Dernière année civile de la série tracée.
#' @param to_position Fonction convertissant une année civile en position x du
#'   graphique (chaque indicateur a son propre décalage position <-> année
#'   selon la façon dont sa moyenne mobile a été construite).
#' @return Un objet \code{scale_x_continuous} ggplot2, à ajouter au graphique avec \code{+}.
#' @export
scale_x_years_plagepomi <- function(year_first, year_last, to_position) {
  major_years <- seq(ceiling(year_first / 10) * 10, floor(year_last / 10) * 10, by = 10)
  if (!(year_last %in% major_years)) major_years <- sort(c(major_years, year_last))
  minor_years <- seq(floor(year_first / 5) * 5, ceiling(year_last / 5) * 5, by = 5)
  scale_x_continuous(breaks = to_position(major_years), labels = major_years,
                      minor_breaks = to_position(minor_years))
}

#' Thème harmonisé pour les indicateurs PLAGEPOMI
#'
#' Thème ggplot2 harmonisé (taille de police, marges des titres) pour les 4
#' graphiques d'indicateurs PLAGEPOMI. Ne fixe pas \code{legend.position} pour
#' ne pas écraser le choix propre à chaque graphique (absent/bas selon qu'il y
#' a une légende ou non).
#'
#' @param base_size Taille de police de base, en points. Défaut : 13 (utiliser
#'   une valeur plus petite, ex. 9, pour un assemblage de plusieurs graphiques
#'   sur une même figure - cf. la figure de synthèse des 4 indicateurs).
#' @param title_size Taille du titre, en points, indépendamment de
#'   \code{base_size} (par défaut, \code{theme_bw()} donne au titre 1.2x
#'   \code{base_size}, soit 15.6 pour \code{base_size = 13}). Défaut : 13
#'   (même taille que le reste du texte).
#' @param palette Palette de couleurs à utiliser pour la grille et le texte des
#'   axes. Défaut : \code{\link{plagepomi_palette}}.
#' @return Un objet thème ggplot2, à ajouter au graphique avec \code{+}.
#' @export
theme_plagepomi <- function(base_size = 13, title_size = 13, palette = plagepomi_palette) {
  theme_bw(base_size = base_size) +
    theme(
      plot.title = element_text(size = title_size, hjust = 0.5, margin = margin(b = 10), face = "bold"),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.title.y = element_text(margin = margin(r = 8)),
      axis.text = element_text(colour = palette$axis_text),
      axis.title = element_text(colour = palette$axis_text),
      panel.grid.major = element_line(colour = palette$grid_major, linewidth = 0.3),
      panel.grid.minor = element_line(colour = palette$grid_major, linewidth = 0.15),
      panel.border = element_blank(),
      axis.line = element_line(colour = "grey60", linewidth = 0.3)
    )
}

#' Indicateur PLAGEPOMI "niveau de population"
#'
#' Trace le nombre d'adultes observés/estimés à Vichy (moyenne mobile arrière
#' sur 5 ans, cf. \code{\link{trailing_mean}}), comparé à la cible (nombre
#' d'adultes que produirait la quantité de juvéniles visée dans les
#' conditions de survie actuelles), avec un polygone rouge/vert indiquant si
#' l'observé est en dessous/au-dessus de la cible.
#'
#' @details
#' La cible est lissée avec exactement la même fenêtre mobile arrière que
#' l'observé (5 ans, cf. \code{\link{trailing_mean}}). C'est nécessaire pour
#' une comparaison juste : l'observé sur une fenêtre comme 2000-2004 mélange
#' des retours produits sous l'ancien ET le nouveau régime d'habitat dès
#' qu'une ouverture survient dans la fenêtre (ex. Alagnon en 2004) - une
#' cible non lissée (100% "nouveau régime" dès l'année de l'ouverture)
#' surestimerait alors artificiellement l'écart avec l'observé. Ce choix
#' délisse en contrepartie la lecture des paliers nets de la cible (Tableau 2
#' du rapport d'indicateurs 2021) : chaque ouverture d'habitat apparaît comme
#' une rampe sur ~4 ans plutôt qu'un saut nette à sa date exacte - la
#' cohérence de la comparaison prime sur la lisibilité des dates.
#'
#' @param mean_target Vecteur de longueur T : cible (nombre d'adultes visé), par année.
#' @param observed Vecteur de longueur T : adultes observés/estimés à Vichy, par année (cf. \code{\link{reconstitute_data_vichy}}).
#' @param T Nombre d'années.
#' @param split_year Année civile à partir de laquelle la série observée
#'   passe de "estimée" (tirets) à "réelle" (trait plein) - la première année
#'   où la fenêtre de 5 ans de la moyenne mobile ne contient que des comptages
#'   réels à Vichy.
#' @param title Titre du graphique. NULL pour ne pas en afficher (utile dans
#'   une fiche indicateur où le titre est déjà affiché ailleurs).
#' @param ylab_txt Titre de l'axe Y.
#' @param year_origin Année de référence (année 1 du modèle = year_origin + 1). Défaut : 1974.
#' @param palette Palette de couleurs. Défaut : \code{\link{plagepomi_palette}}.
#' @return Un objet ggplot.
#' @export
draw_niveau_pop <- function(mean_target, observed, T, split_year, title, ylab_txt,
                             year_origin = 1974, palette = plagepomi_palette) {
  target_ma <- trailing_mean(mean_target, 5)
  keep <- !is.na(target_ma)
  target_ma <- target_ma[keep]
  observed_ma <- trailing_mean(observed, 5)[keep]
  years <- (year_origin + seq_along(mean_target))[keep]
  n <- length(years)
  split_at <- which(years >= split_year)[1]

  ids <- factor(c(str_c("1.", seq(1, (n - 1), 1)), str_c("2.", seq(1, (n - 1), 1))))
  values <- data.frame(id = ids, values = c(rep(palette$bg_red, (n - 1)), rep(palette$bg_green, (n - 1))))
  positions <- data.frame(
    id = rep(ids, each = 4),
    x = rep(c(rbind(years[2:n], years[1:(n - 1)], years[1:(n - 1)], years[2:n])), 2),
    y = c(
      c(rbind(0, 0, target_ma[1:(n - 1)], target_ma[2:n])),
      c(rbind(target_ma[2:n], target_ma[1:(n - 1)],
              rep(max(observed_ma, na.rm = TRUE), (n - 1)), rep(max(observed_ma, na.rm = TRUE), (n - 1))))
    )
  )
  datapoly <- plyr::join(values, positions)

  p <- ggplot() +
    geom_polygon(data = datapoly, aes(x = x, y = y, group = id), fill = datapoly$values, alpha = 0.55) +
    geom_line(aes(years[1:split_at], observed_ma[1:split_at]), colour = palette$line_main, linewidth = 0.9, linetype = "dashed") +
    geom_line(aes(years[split_at:n], observed_ma[split_at:n]), colour = palette$line_main, linewidth = 0.9) +
    xlab("Année") +
    ylab(ylab_txt) +
    scale_x_years_plagepomi(min(years), max(years), identity) +
    theme(legend.position = "none") +
    theme_plagepomi()
  if (!is.null(title)) p <- p + ggtitle(title)
  p
}

#' Indicateur PLAGEPOMI "diagnostic de conservation"
#'
#' Trace, sur une fenêtre glissante de \code{window} ans, la probabilité
#' d'assurer la conservation pour 3 niveaux de risque toléré (10/20/30\%), pour
#' un seuil cible de \code{target_pct}\% de Rmax.
#'
#' @param mean_risk_10,mean_risk_20,mean_risk_30 Vecteurs de longueur T :
#'   probabilité de conservation pour un risque toléré de 10/20/30\%, tels que
#'   retournés par \code{\link{compute_diagnostic_risk}}.
#' @param T Nombre d'années.
#' @param target_pct Seuil de diagnostic, en pourcentage de Rmax (affiché dans le titre).
#' @param window Taille de la fenêtre glissante, en années. Défaut : 10.
#' @param year_origin Année de référence (année 1 du modèle = year_origin + 1). Défaut : 1974.
#' @param show_title Si FALSE, n'affiche pas de titre (utile quand le titre est
#'   déjà affiché ailleurs, ex. bandeau d'une fiche indicateur). Défaut : TRUE.
#' @param palette Palette de couleurs. Défaut : \code{\link{plagepomi_palette}}.
#' @return Un objet ggplot.
#' @export
draw_diagnostic_conservation <- function(mean_risk_10, mean_risk_20, mean_risk_30, T,
                                          target_pct, window = 10, year_origin = 1974,
                                          show_title = TRUE, palette = plagepomi_palette) {
  x_diag <- seq(window + 1, T, 1)
  rects <- data.frame(ystart = seq(0, 0.75, 0.25), yend = seq(0.25, 1, 0.25),
                       col = c(palette$bg_red, palette$bg_orange, palette$bg_yellow, palette$bg_green))

  p <- ggplot() +
    geom_rect(data = rects, aes(xmin = (window + 1), xmax = T, ymin = ystart, ymax = yend), fill = rects$col, alpha = 0.55) +
    geom_line(aes(x_diag, mean_risk_10[x_diag], colour = "1", linetype = "1"), linewidth = 0.9) +
    geom_line(aes(x_diag, mean_risk_20[x_diag], colour = "2", linetype = "2"), linewidth = 0.8) +
    geom_line(aes(x_diag, mean_risk_30[x_diag], colour = "3", linetype = "3"), linewidth = 0.8) +
    geom_hline(yintercept = 0.5, colour = palette$threshold, linetype = "solid", linewidth = 0.9) +
    geom_hline(yintercept = 0.75, colour = palette$threshold, linetype = "dashed", linewidth = 0.4) +
    geom_hline(yintercept = 0.25, colour = palette$threshold, linetype = "dashed", linewidth = 0.4) +
    annotate("text", x = window + 1, y = 0.5, label = "seuil 50%", hjust = 0, vjust = -0.5,
             size = 3, colour = palette$threshold) +
    xlab("Année") +
    ylab("Probabilité d'assurer la conservation \nselon le diagnostic choisi") +
    scale_x_years_plagepomi(year_origin + window + 1, year_origin + T, function(year) year - year_origin) +
    scale_y_continuous(breaks = seq(0, 1, 0.2), labels = seq(0, 1, 0.2)) +
    scale_colour_manual("", breaks = c("1", "2", "3"), values = c(palette$line_main, "#6B7280", "#AEB4BC"),
                        labels = c("Risque 10%", "Risque 20%", "Risque 30%")) +
    scale_linetype_manual("", breaks = c("1", "2", "3"), values = c("solid", "dashed", "dotted"),
                          labels = c("Risque 10%", "Risque 20%", "Risque 30%")) +
    theme_plagepomi() +
    theme(legend.position = "bottom")
  if (show_title) {
    p <- p + ggtitle(str_c("Diagnostic de conservation de la population sauvage pour ", target_pct, "% Rmax \n(fenêtre glissante sur ", window, " ans)"))
  }
  p
}

#' Indicateur PLAGEPOMI "part des juvéniles sauvages"
#'
#' Trace le ratio juvéniles sauvages / juvéniles totaux (moyenne mobile
#' arrière sur 5 ans, cf. \code{\link{trailing_mean}}), avec un fond
#' rouge/vert selon que le ratio est en dessous/au-dessus de 50\%.
#'
#' @param mean_ratio Vecteur de longueur T : ratio juvéniles sauvages / totaux, tel que retourné par \code{\link{compute_ratio_juv_wild}}.
#' @param T Nombre d'années.
#' @param year_origin Année de référence (année 1 du modèle = year_origin + 1). Défaut : 1974.
#' @param show_title Si FALSE, n'affiche pas de titre (utile quand le titre est
#'   déjà affiché ailleurs, ex. bandeau d'une fiche indicateur). Défaut : TRUE.
#' @param palette Palette de couleurs. Défaut : \code{\link{plagepomi_palette}}.
#' @return Un objet ggplot.
#' @export
draw_part_juv_wild <- function(mean_ratio, T, year_origin = 1974, show_title = TRUE,
                                palette = plagepomi_palette) {
  ratio_ma <- trailing_mean(mean_ratio, 5)
  keep <- !is.na(ratio_ma)
  ratio_ma <- ratio_ma[keep]
  years <- (year_origin + seq_along(mean_ratio))[keep]

  rect_2_col <- data.frame(ystart = c(0, 0.5), yend = c(0.5, 1), col = c(palette$bg_red, palette$bg_green))

  p <- ggplot() +
    geom_rect(data = rect_2_col, aes(xmin = min(years), xmax = max(years), ymin = ystart, ymax = yend), fill = rect_2_col$col, alpha = 0.55) +
    geom_line(aes(years, ratio_ma), colour = palette$line_main, linewidth = 0.9) +
    geom_hline(yintercept = 0.5, colour = palette$threshold, linetype = "solid", linewidth = 0.9) +
    geom_hline(yintercept = 0.75, colour = palette$threshold, linetype = "dashed", linewidth = 0.4) +
    geom_hline(yintercept = 0.25, colour = palette$threshold, linetype = "dashed", linewidth = 0.4) +
    annotate("text", x = max(years), y = 0.5, label = "objectif 50%", hjust = 1, vjust = -0.5,
             size = 3, colour = palette$threshold) +
    annotate("text", x = min(years), y = 0.08, label = "Objectif non atteint", hjust = 0, size = 3, colour = "grey35") +
    annotate("text", x = min(years), y = 0.92, label = "Objectif atteint", hjust = 0, size = 3, colour = "grey35") +
    xlab("Année") +
    ylab("Ratio juvénile sauvage \nsur juvénile total") +
    scale_x_years_plagepomi(min(years), max(years), identity) +
    theme(legend.position = "none") +
    theme_plagepomi()
  if (show_title) {
    p <- p + ggtitle("Part de juvéniles sauvages dans l'ensemble des juvéniles \n(moyenne mobile 5 ans)")
  }
  p
}

#' Indicateur PLAGEPOMI "taux de renouvellement de la population sauvage"
#'
#' Variante de \code{\link{draw_indic_tx_ren}} dédiée aux fiches/graphiques
#' PLAGEPOMI harmonisés (palette \code{\link{plagepomi_palette}}, thème
#' \code{\link{theme_plagepomi}}, axe des années cohérent avec les 3 autres
#' indicateurs). La fonction \code{draw_indic_tx_ren} d'origine, partagée avec
#' d'autres scripts du projet, reste inchangée. Contrairement à
#' \code{draw_indic_tx_ren} (limite basse de l'axe Y fixée à 0.2), la limite
#' basse est calculée dynamiquement pour ne jamais couper un point de la
#' série. Utilise \code{\link{trailing_mean}} (moyenne mobile arrière) et non
#' \code{\link{running.mean}} (centrée) : conforme au rapport d'indicateurs
#' 2021 ("l'année qui apparait sur le graphique est la dernière année de
#' chacune des périodes mobiles de 5 ans"), ce qui permet aussi d'atteindre la
#' dernière année réellement calculable (une moyenne centrée aurait toujours
#' besoin de 2 années de données futures qui n'existent pas encore).
#'
#' @param taux_renouv_annuel Vecteur de longueur T : taux de renouvellement
#'   annuel en échelle naturelle (ex. \code{exp(colMeans(renew_rate_w_coef))}),
#'   NA hors de la plage calculable.
#' @param T Nombre d'années.
#' @param year_origin Année de référence (année 1 du modèle = year_origin + 1). Défaut : 1974.
#' @param show_title Si FALSE, n'affiche pas de titre. Défaut : TRUE.
#' @param palette Palette de couleurs. Défaut : \code{\link{plagepomi_palette}}.
#' @return Un objet ggplot.
#' @export
draw_tx_renouv_sauvage <- function(taux_renouv_annuel, T, year_origin = 1974,
                                    show_title = TRUE, palette = plagepomi_palette) {
  ma <- trailing_mean(taux_renouv_annuel, 5)
  keep <- !is.na(ma)
  ma <- ma[keep]
  years <- (year_origin + seq_along(taux_renouv_annuel))[keep]

  y_min <- min(0.2, min(ma, na.rm = TRUE) * 0.9)
  rect_2_col <- data.frame(ystart = c(y_min, 1), yend = c(1, 5),
                           col = c(palette$bg_red, palette$bg_green))

  p <- ggplot() +
    geom_rect(data = rect_2_col, aes(xmin = min(years), xmax = max(years), ymin = ystart, ymax = yend),
              fill = rect_2_col$col, alpha = 0.55) +
    geom_line(aes(years, ma), colour = palette$line_main, linewidth = 0.9) +
    geom_hline(yintercept = 1, colour = palette$threshold, linewidth = 0.9) +
    annotate("text", x = max(years), y = 1, label = "seuil = 1", hjust = 1, vjust = -0.6,
             size = 3, colour = palette$threshold) +
    scale_y_continuous(trans = "log10", limits = c(y_min, 5)) +
    annotation_logticks(sides = "l") +
    scale_x_years_plagepomi(min(years), max(years), identity) +
    xlab("Année de reproduction") +
    ylab("Tx de renouvellement") +
    theme_plagepomi() +
    theme(legend.position = "none")
  if (show_title) {
    p <- p + ggtitle("Taux de renouvellement de la population sauvage \n(moyenne mobile 5 ans)")
  }
  p
}

#' Construit une fiche indicateur PLAGEPOMI (patchwork)
#'
#' Assemble un graphique d'indicateur (sans titre interne, cf. paramètre
#' \code{show_title}/\code{title} des \code{draw_*}) avec un bandeau de titre
#' (titre, période évaluée, cartouche de diagnostic coloré), une colonne de
#' commentaire (À retenir / Repère-objectif / Interprétation) et un bandeau
#' méthodologique, en une fiche autonome au format patchwork - empilable avec
#' \code{/} pour construire une page (cf. \code{page_1 <- fiche1 / fiche2}).
#'
#' @param graphique Objet ggplot de l'indicateur (idéalement sans titre interne).
#' @param titre Titre court de l'indicateur, affiché en gras dans le bandeau supérieur.
#' @param periode Période évaluée (ex. "2021-2025"), affichée en étiquette discrète.
#' @param diagnostic Texte du diagnostic (ex. "Sous le seuil"), affiché dans le cartouche coloré.
#' @param statut Statut associé au cartouche : \code{"rouge"}, \code{"vert"},
#'   \code{"orange"} ou toute autre valeur (gris, statut indéterminé).
#' @param a_retenir Texte de la rubrique "À retenir" (commentaire synthétique).
#' @param repere Texte de la rubrique "Repère / objectif" (valeur ou seuil de référence).
#' @param interpretation Texte de la rubrique "Interprétation" (interprétation de l'évolution).
#' @param methode Texte du bandeau "Méthode et précautions" (méthode de calcul, période de référence, précautions).
#' @param palette Palette de couleurs. Défaut : \code{\link{plagepomi_palette}}.
#' @return Un objet patchwork (composable avec \code{/} et \code{+}).
#' @export
creer_fiche_indicateur <- function(graphique, titre, periode, diagnostic, statut,
                                    a_retenir, repere, interpretation, methode,
                                    palette = plagepomi_palette) {
  badge_col <- switch(statut,
    rouge = palette$badge_red,
    vert = palette$badge_green,
    orange = palette$badge_orange,
    palette$badge_gray
  )

  bandeau_titre <- ggplot() +
    xlim(0, 1) + ylim(0, 1) +
    annotate("text", x = 0.01, y = 0.5, label = titre, hjust = 0, vjust = 0.5,
             fontface = "bold", size = 5, colour = "#1F2328") +
    ggtext::geom_richtext(aes(x = 0.72, y = 0.5, label = periode),
                          hjust = 0.5, vjust = 0.5, size = 3.3,
                          fill = "#EEF0F2", label.colour = NA, colour = "#4B5563",
                          label.r = unit(4, "pt"), label.padding = unit(c(3, 7, 3, 7), "pt")) +
    ggtext::geom_richtext(aes(x = 0.99, y = 0.5, label = diagnostic),
                          hjust = 1, vjust = 0.5, size = 3.5, fontface = "bold",
                          fill = badge_col, label.colour = NA, colour = "white",
                          label.r = unit(4, "pt"), label.padding = unit(c(4, 9, 4, 9), "pt")) +
    theme_void() +
    theme(plot.margin = margin(4, 10, 4, 10),
          plot.background = element_rect(fill = "#F7F8F9", colour = NA))

  colonne_commentaire <- ggplot() +
    xlim(0, 1) + ylim(0, 1) +
    annotate("text", x = 0, y = 0.97, label = "À RETENIR", hjust = 0, vjust = 1,
             fontface = "bold", size = 3.2, colour = "#1F2328") +
    ggtext::geom_textbox(aes(x = 0, y = 0.90, label = a_retenir), hjust = 0, vjust = 1,
                          width = unit(0.95, "npc"), box.colour = NA, fill = NA,
                          size = 3, colour = "grey45", fontface = "italic") +
    annotate("text", x = 0, y = 0.62, label = "REPÈRE / OBJECTIF", hjust = 0, vjust = 1,
             fontface = "bold", size = 3.2, colour = "#1F2328") +
    ggtext::geom_textbox(aes(x = 0, y = 0.55, label = repere), hjust = 0, vjust = 1,
                          width = unit(0.95, "npc"), box.colour = NA, fill = NA,
                          size = 3, colour = "grey45", fontface = "italic") +
    annotate("text", x = 0, y = 0.30, label = "INTERPRÉTATION", hjust = 0, vjust = 1,
             fontface = "bold", size = 3.2, colour = "#1F2328") +
    ggtext::geom_textbox(aes(x = 0, y = 0.23, label = interpretation), hjust = 0, vjust = 1,
                          width = unit(0.95, "npc"), box.colour = NA, fill = NA,
                          size = 3, colour = "grey45", fontface = "italic") +
    theme_void() +
    theme(plot.margin = margin(6, 6, 6, 12),
          plot.background = element_rect(fill = palette$bg_offwhite, colour = NA))

  corps <- graphique + colonne_commentaire + patchwork::plot_layout(widths = c(0.63, 0.37))

  bandeau_methode <- ggplot() +
    xlim(0, 1) + ylim(0, 1) +
    annotate("text", x = 0, y = 0.8, label = "MÉTHODE ET PRÉCAUTIONS", hjust = 0, vjust = 1,
             fontface = "bold", size = 3, colour = "#1F2328") +
    ggtext::geom_textbox(aes(x = 0, y = 0.5, label = methode), hjust = 0, vjust = 1,
                          width = unit(0.98, "npc"), box.colour = NA, fill = NA,
                          size = 2.9, colour = "grey40") +
    theme_void() +
    theme(plot.margin = margin(4, 10, 6, 10),
          plot.background = element_rect(fill = "#F3F4F6", colour = NA))

  bandeau_titre / corps / bandeau_methode +
    patchwork::plot_layout(heights = c(0.11, 0.71, 0.18))
}
