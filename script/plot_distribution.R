# TODO: Add comment
# Script pour visualiser les formes des distributions
# Author: Marion LEGRAND
###############################################################################


# Définir les paramètres de la distribution Beta
alpha <- 19*1.8
beta <- 1*1.8

n <- 10000
sample <- rbeta(n, shape1 = alpha, shape2 = beta)

# Tracer un histogramme des valeurs tirées
hist(sample, breaks = 30, probability = TRUE, col = "lightblue",
		main = "Histogramme des valeurs tirées d'une distribution Beta(19,1)",
		xlab = "Valeurs", ylab = "Densité")
quantile(sample)
length(sample[sample>0.9])/10000

# Ajouter la courbe de la densité théorique
x <- seq(0, 1, length.out = 1000)
y <- dbeta(x, shape1 = alpha, shape2 = beta)
lines(x, y, col = "blue", lwd = 2)


# Définir les paramètres de la distribution Beta
alpha <- 38
beta <- 2

n <- 1000
sample <- rbeta(n, shape1 = alpha, shape2 = beta)

# Tracer un histogramme des valeurs tirées
hist(sample, breaks = 30, probability = TRUE, col = "lightblue",
		main = "Histogramme des valeurs tirées d'une distribution Beta(38,2)",
		xlab = "Valeurs", ylab = "Densité")

# Ajouter la courbe de la densité théorique
x <- seq(0, 1, length.out = 1000)
y <- dbeta(x, shape1 = alpha, shape2 = beta)
lines(x, y, col = "blue", lwd = 2)

# Définir les paramètres de la distribution Beta
alpha <- 57
beta <- 3

n <- 1000
sample <- rbeta(n, shape1 = alpha, shape2 = beta)

# Tracer un histogramme des valeurs tirées
hist(sample, breaks = 30, probability = TRUE, col = "lightblue",
		main = "Histogramme des valeurs tirées d'une distribution Beta(57,3)",
		xlab = "Valeurs", ylab = "Densité")

# Ajouter la courbe de la densité théorique
x <- seq(0, 1, length.out = 1000)
y <- dbeta(x, shape1 = alpha, shape2 = beta)
lines(x, y, col = "blue", lwd = 2)
