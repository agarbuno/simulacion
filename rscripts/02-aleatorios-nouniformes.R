## Setup --------------------------------------------
library(tidyverse)
library(patchwork)
library(scales)
## Cambia el default del tamaño de fuente 
theme_set(theme_linedraw(base_size = 25))

## Cambia el número de decimales para mostrar
options(digits = 4)

sin_lineas <- theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())
color.itam  <- c("#00362b","#004a3b", "#00503f", "#006953", "#008367", "#009c7b", "#00b68f", NA)

sin_lineas <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
sin_leyenda <- theme(legend.position = "none")
sin_ejes <- theme(axis.ticks = element_blank(), axis.text = element_blank())

g1 <- tibble(x = rexp(1000, 1)) |>
  ggplot(aes(x)) +
  geom_histogram() + sin_lineas +
  xlim(0, 8) + ggtitle("Exponencial R")

g2 <- tibble(u = runif(1000),
             x = -log(u)) |>
  ggplot(aes(x)) +
  geom_histogram() + sin_lineas +
  xlim(0, 8) + ggtitle("Exponencial = f(Uniforme)")

g1 + g2

set.seed(108)
U <- runif(3 * 10^4)      # Genera uniformes
U <- matrix(U, nrow = 3)  # Transforma a matriz
X <- -log(U)              # Transforma a exponenciales
X <- 2 * apply(X, 2, sum) # Suma los tres renglones
summary(X)

set.seed(108)
runif(3 * 10^4) |>        # Genera uniformes 
  matrix(nrow = 3) |>     # Transforma a matriz
  log() |>                # Calcula logaritmos
  apply(2, function(x){-2 * sum(x)} ) |> 
  summary()

set.seed(108)
Sigma <- diag(2); Sigma[1,2] <- .75; Sigma[2,1] <- .75;
L <- chol(Sigma)

Z <- rnorm(2 * 10^4)      # Generamos vectores estandar
Z <- matrix(Z, nrow = 2)  # Reacomodamos en matriz
X <- t(L) %*% Z           # Transformacion lineal
cov(t(X))

k <- 1:10
probs <- pbinom(k, 10, .3)
probs

rbinomial <- function(nsamples, size, theta){
  probs <- pbinom(k, size, theta)
  x <- c()
  for (jj in 1:nsamples){
    u <- runif(1)
    x[jj] <- which(probs > u)[1]
  }
  return(x)
}

set.seed(108)
x <- rbinomial(1000, 10, .3)
tibble(samples = x) |>
  ggplot(aes(samples)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1,
                 color = 'white') +
  geom_line(data = tibble(x_ = 1:8, y_ = dbinom(x_, 10, .3)),
          aes(x_, y_), lwd = 1.5, lty = 2, 
          colour = "salmon") + 
  sin_lineas

k <- 1:24
ppois(k, 7)

rpoisson <- function(nsamples, lambda){
  probs <- ppois(1:30, lambda)
  x <- c()
  for (jj in 1:nsamples){
    u <- runif(1)
    x[jj] <- which(probs > u)[1]
  }
  return(x)
}

set.seed(108)
x <- rpoisson(1000, 7)
tibble(samples = x) |>
  ggplot(aes(samples)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1,
                 color = 'white') +
  geom_line(data = tibble(x_ = 1:30, y_ = dpois(x_, 7)),
          aes(x_, y_), lwd = 1.5, lty = 2, 
          colour = "salmon") + 
  sin_lineas

nsamples <- 10^4
n <- 6; theta <- .3
y <- rgamma(nsamples, n, rate = theta/(1-theta))
x <- rpois(nsamples, y)

tibble(samples = x) |>
ggplot(aes(x)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1, color = "white") +
  geom_line(data = tibble(x_ = 1:60, y_ = dnbinom(x_, n, theta)),
            aes(x_, y_), lwd = 1.5, lty = 2, 
            colour = "salmon") +
  sin_lineas

## Esto es para poner a prueba un pseudo generador 
rpseudo.uniform <- function(nsamples, seed = 108727){
  x0 <- seed; a <- 7**5; m <- (2**31)-1;
  x  <- x0; 
  for (jj in 2:nsamples){
    x[jj] <- (a * x[jj-1]) %% m
  }
  x/m
}

nsamples <- 30;  nbins <- 10;
samples <- data.frame(x = rpseudo.uniform(nsamples, seed = 166136))
samples |>
ggplot(aes(x)) +
  geom_hline(yintercept = nsamples/nbins, color = "darkgray", lty = 2) +
  annotate("rect",
           ymin = qbinom(.95, nsamples, 1/nbins),
           ymax = qbinom(.05, nsamples, 1/nbins),
           xmin = -Inf, xmax = Inf,
           alpha = .4, fill = "gray") + 
  geom_histogram(bins = nbins, color = "white") + sin_lineas +
  ggtitle("Semilla: 166136")

## Esto es para poner a prueba un pseudo generador =============================
rpseudo.uniform <- function(nsamples, seed = 108727){
  x0 <- seed; a <- 7**5; m <- (2**31)-1;
  x  <- x0; 
  for (jj in 2:nsamples){
    x[jj] <- (a * x[jj-1]) %% m
  }
  x/m
}

nsamples <- 30; nbreaks <- 10
samples <- data.frame(x = rpseudo.uniform(nsamples))

Fn <- hist(samples$x, breaks = nbreaks, plot = FALSE)$counts/nsamples
F0 <- 1/nbreaks

X2.obs <- (nsamples*nbreaks)*sum((Fn - F0)**2)

## Esto es para generar datos observados de la distribucion que queremos 
experiment <- function(nsamples){
  nbreaks <- 10
  samples <- data.frame(x = runif(nsamples))
  Fn <- hist(samples$x, breaks = nbreaks, plot = FALSE)$counts/nsamples
  F0 <- 1/nbreaks
  X2 <- (nsamples*nbreaks)*sum((Fn - F0)**2)
  return(X2)
}

X2 <- c()
for (jj in 1:5000){
  X2[jj] <- experiment(nsamples)
}

data.frame(estadistica = X2) |>
  ggplot(aes(estadistica)) +
  geom_histogram(aes(y = ..density..)) +
  geom_vline(xintercept = X2.obs, lty = 2, color = 'red', lwd = 1.5) +
  stat_function(fun = dchisq, args = list(df = nbreaks - 1), color = 'salmon', lwd = 1.5) +
  sin_lineas + xlab(expression(chi^{2}))

print(paste("Estadistico: ", round(X2.obs, 4), ", Probabilidad: ", mean(X2 >= X2.obs), sep =''))

counts.obs <- Fn*nsamples 
chisq.test(counts.obs, p = rep(1, nbreaks)/nbreaks, simulate.p.value = TRUE)

ks.test(samples$x, "punif")
