## Setup ---------------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(scales)

## Cambia el default del tamaño de fuente 
theme_set(theme_linedraw(base_size = 25))

## Cambia el número de decimales para mostrar
options(digits = 4)
## Problemas con mi consola en Emacs
options(pillar.subtle = FALSE)
options(rlang_backtrace_on_error = "none")
options(crayon.enabled = FALSE)
options(width=80)

## Para el tema de ggplot
color.itam  <- c("#00362b","#004a3b", "#00503f", "#006953", "#008367", "#009c7b", "#00b68f", NA)
sin_leyenda <- theme(legend.position = "none")
sin_ejes <- theme(axis.ticks = element_blank(), axis.text = element_blank())
sin_lineas <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

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

## Ejemplo: Chi-cuadrada (6) =================================================

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

## Ejemplo: Vectores Gaussianos ==============================================

set.seed(108)
Sigma <- diag(2); Sigma[1,2] <- .75; Sigma[2,1] <- .75;
L <- chol(Sigma)

Z <- rnorm(2 * 10^4)      # Generamos vectores estandar
Z <- matrix(Z, nrow = 2)  # Reacomodamos en matriz
X <- t(L) %*% Z           # Transformacion lineal
cov(t(X))

## Ejemplo: Variables binomiales =============================================

k <- 1:10
pbinom(k, 10, .3)

rbinomial <- function(nsamples, size, theta){
  probs <- pbinom(1:10, size, theta)
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

## Ejemplo: Poisson (discretas sin cota) =====================================

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

# Mezclas, distribuciones conjuntas y distribuciones marginales ==============

## Ejemplo: Poisson-Gamma ====================================================

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

## Ejemplo: Beta-Binomial ====================================================

nsamples <- 10^4
n <- 20; a <- 5; b <- 2
theta <- rbeta(nsamples, a, b)
x <- rbinom(nsamples, n, theta)

tibble(samples = x) |>
ggplot(aes(x)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1, color = "white") +
  sin_lineas

## Mezcla de Gaussianas ======================================================

nsamples <- 10^5
y <- sample(1:3, size = nsamples, prob = c(.1, .7, .2), replace = TRUE)
x <- rnorm(nsamples,
           mean = ifelse(y==1, 1, ifelse(y==2, 2, 5)),
           sd = ifelse(y==1, 0.1, ifelse(y==2, 0.5, 1)))

tibble(samples = x) |>
ggplot(aes(x)) +
  geom_histogram(aes(y = ..density..), color = "white") +
  geom_line(data = tibble(x_ = seq(0, 8, length = 500),
                          y_ = .1 * dnorm(x_, 1, 0.1) +
                               .7 * dnorm(x_, 2, .5) +
                               .2 * dnorm(x_, 5, 1)),
            aes(x_, y_), lwd = 1.5, lty = 2, 
            colour = "salmon") +    
  sin_lineas

# Verificando distribución con Ji-cuadarada ==================================

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
samples <- tibble(x = rpseudo.uniform(nsamples, seed = 166136))
samples |>
  ggplot(aes(x)) +
  geom_hline(yintercept = nsamples/nbins, color = "darkgray", lty = 2) +
  annotate("rect",
           ymin = qbinom(.95, nsamples, 1/nbins),
           ymax = qbinom(.05, nsamples, 1/nbins),
           xmin = -Inf, xmax = Inf,
           alpha = .4, fill = "gray") + 
  geom_histogram(binwidth = 1/nbins, color = "white", boundary = 1) + sin_lineas +
  coord_cartesian(xlim = c(0, 1)) + 
  ggtitle("Semilla: 166136")

## Usando la noción del abogado del diablo ===================================

set.seed(108727)
## Generamos muestra de nuestro generador (congruencial lineal) 
samples <- tibble(x = rpseudo.uniform(nsamples))

## Calculamos frecuencias relativas y teoricas 
Fn <- hist(samples$x, breaks = nbins, plot = FALSE)$counts/nsamples
F0 <- 1/nbins

## Calculamos nuestra referencia 
X2.obs <- (nsamples*nbins)*sum((Fn - F0)**2)

## La imprimimos en pantalla 
paste("Estadístico: ", round(X2.obs, 3))

## Replicando experimentos =================================================== 
experiment <- function(nsamples, nbreaks = 20){
  samples <- data.frame(x = runif(nsamples))
  Fn <- hist(samples$x, breaks = nbreaks, plot = FALSE)$counts/nsamples
  F0 <- 1/nbreaks
  X2 <- (nsamples*nbreaks)*sum((Fn - F0)**2)
  return(X2)
}

X2 <- c()
for (jj in 1:5000){
  X2[jj] <- experiment(nsamples, nbins)
}

tibble(estadistica = X2) |>
  ggplot(aes(estadistica)) +
  geom_histogram(aes(y = ..density..), bins = 20, color = "white") +
  xlab(expression(chi[nbins-1]^{2})) + 
  geom_vline(xintercept = X2.obs, lty = 2, color = 'red', lwd = 1.5) +
  stat_function(fun = dchisq, args = list(df = nbins - 1), color = 'salmon', lwd = 1.5) +
  sin_lineas

print(paste("Estadistico: ", round(X2.obs, 4), ", Probabilidad: ", mean(X2 >= X2.obs), sep =''))

counts.obs <- Fn*nsamples 
chisq.test(counts.obs, p = rep(1, nbreaks)/nbreaks, simulate.p.value = TRUE)

ks.test(samples$x, "punif")

## Numeros aleatorios normales (aprox. uniforme) =============================

nsamples <- 10^4; k <- 12
U <- runif(k * nsamples)
U <- matrix(U, nrow = k)
X <- apply(U, 2, function(x){ (sum(x) - k/2)/sqrt(k/12) })

g1 <- tibble(samples = X) |>
  ggplot(aes(samples)) +
  geom_histogram() + sin_lineas +
  ggtitle("Utilizando uniformes")

g2 <- tibble(samples = rnorm(nsamples)) |>
  ggplot(aes(samples)) +
  geom_histogram() + sin_lineas +
  ggtitle("Utilizando rnorm")

g1 + g2

## Numeros normales (metodo polar)

rnormal.bm <- function(n){
  r <- sqrt(-2 * log(runif(n)))
  theta <- runif(n, 0, 2 * pi)
  z <- matrix(0, nrow = 2, ncol = n)
  z[1,] <- r * cos(2 * pi * theta)
  z[2,] <- r * sin(2 * pi * theta)
  return(z)
}

set.seed(108)
z <- rnormal.bm(nsamples)
g.joint <- tibble(z1 = z[1,], z2 = z[2,]) |>
  ggplot(aes(z1, z2)) +
  geom_point() + ylab(expression(z[2])) +
  xlab(expression(z[1])) +
  sin_lineas
g.x <- tibble(z1 = z[1,], z2 = z[2,]) |>
  ggplot(aes(z1)) +
  geom_histogram() + xlab(expression(z[1])) + 
  sin_lineas
g.y <- tibble(z1 = z[1,], z2 = z[2,]) |>
  ggplot(aes(z2)) +
  geom_histogram() + xlab(expression(z[2])) + 
  sin_lineas

(g.x / g.y | g.joint)

sessionInfo()
