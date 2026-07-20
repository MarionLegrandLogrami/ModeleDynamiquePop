######################################################################################################################
# Script de création des fonctions servant à générer les graphiques pour les sorties du modèle de dynamique de pop ###
# @uteur : Marion LEGRAND - LOGRAMI                                                                                ###
# date : 2022_04_06                                                                                                ###
######################################################################################################################

#===================================================================================
# Graph pour sortir les boxplot en passant par une fonction pour simplifier le code
#===================================================================================

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






#===================================================================================
# PNG boxplot par année : ouvre le device, appelle custom_plot(), ferme le device.
# Evite de répéter le trio png(...)/custom_plot(...)/dev.off() partout.
#===================================================================================
boxplot_years_png <- function(imgwd, filename, width = 800, height = 800, ...) {
  png(filename = str_c(imgwd, filename, ".png"), width = width, height = height)
  custom_plot(...)
  dev.off()
}

#===================================================================================
# PNG standard 2 panneaux (trace / densité) pour un paramètre coda/mcmc
#===================================================================================
png_trace_density <- function(param, imgwd, filename, width = 800, height = 800) {
  png(filename = str_c(imgwd, filename, ".png"), width = width, height = height)
  par(mfrow = c(2, 1))
  plot(param, density = FALSE, auto.layout = FALSE)
  plot(param, trace = FALSE, auto.layout = FALSE)
  dev.off()
}

#===================================================================================
# Sortie complète "standard" d'un paramètre scalaire du modèle : PNG trace/densité
# + table récapitulative en .tex (xtable). C'est le motif le plus fréquent du
# rapport SortieOpenbugs_parameters.Rnw (voir write_param_table dans fct_data.R).
#===================================================================================
report_param <- function(param, imgwd, tabwd, filename, label = filename,
                          caption = NULL, digits = NULL, width = 800, height = 800) {
  png_trace_density(param, imgwd, filename, width, height)
  write_param_table(param, tabwd, filename, label = label, caption = caption, digits = digits)
}

#===================================================================================
# PNG de corrélogramme (corrplot) entre plusieurs séries/paramètres
#===================================================================================
corrplot_png <- function(data, imgwd, filename, width = 800, height = 800, method = "circle",
                          addCoef.col = "black", number.cex = 0.8, order = "AOE",
                          type = "upper", tl.srt = 45, use = "complete.obs", cor_method = "pearson") {
  png(filename = str_c(imgwd, filename, ".png"), width = width, height = height)
  corrplot(cor(data, use = use, method = cor_method), method = method,
           addCoef.col = addCoef.col, number.cex = number.cex, order = order,
           type = type, tl.srt = tl.srt)
  dev.off()
}

#===============================================================================
# GGPLOT graph indicateur Tx renouvellement bon/mauvais en moyenne mobile 5 ans
#===============================================================================
running.mean<- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 2)}

#--- Description des paramètres utilisés dans la fonction
#--- tx_renouv = matrice contenant les tx de renouvellement calculés pour chaque année et pour chaque itération
#--- Tyear = nombre d'année à considérer.
#--- typ_pop = type de population sur laquelle l'analyse tourne : sauvage ou totale. Met en cohérence le titre du graphique et passe l'argument en minuscule au passage.
#--- simulation = faut-il plotter les 20 ans de simulation du tx de renouvellement. si TRUE ajoute des annotations Rétrospectif/Simulation
#                 et une barre de séparation pointillée au graph
#--- png = veut-on enregistrer le graph en png.
#--- scenario = Sous-titre sur le graphique avec le scénario utilisé
#--- annee_debut = Date de début à afficher sur le graphique
#--- annee_fin = Date de fin à afficher sur le graphique
#--- rect_yStart = Donne les y de début pour construire les 2 rectangles de couleur sur le graph
#--- rect_yEnd = Donne les y de fin pour construire les 2 rectangles de couleur sur le graph
#--- rect_col = Donne les couleur à utiliser pour les rectangles de couleur du graph pour l'indicateur bon et mauvais

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

#====================================================
# GGPLOT graph indicateur Tx renouvellement BOXPLOT
#====================================================
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
# png(filename="D:/Documents/Workspace_eclipse/ModeleDynamiquePop/img/Simulation/2021_09/TauxRenouvellementPopSauvage_ScenarioContinuiteEcologique_boxplot_2022_04_05.png",width=1000,height=800)
# #png(filename="C:/Users/marion.legrand/workspace/ModeleDynamiquePop/img/Simulation/2019_12_IndicateursSat_model_data2016/TauxRenouvellementPopSauvage_ScenarioContinuiteEcologique_boxplot.png",width=1000,height=800)
# #png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/TauxRenouvellementPopSauvage_ScenarioContinuiteEcologique.png",width=1000,height=800)
# #png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/TauxRenouvellementPopSauvage_coef.png",width=1000,height=800)
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

  