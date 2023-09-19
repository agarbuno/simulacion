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
options(width=70)

## Para el tema de ggplot
color.itam  <- c("#00362b","#004a3b", "#00503f", "#006953", "#008367", "#009c7b", "#00b68f", NA)
sin_leyenda <- theme(legend.position = "none")
sin_ejes <- theme(axis.ticks = element_blank(), axis.text = element_blank())
sin_lineas <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

## Ejemplo: variables antiteticas ============================================

set.seed(108);
nsamples <- 10^3;
a <- 2; b <- 3;
u <- runif(nsamples, min = a, max = b)
x <- dnorm(u)

c(estimador = mean(x), error.std = sd(x)/sqrt(nsamples), N = length(x))

u_ <- a + (b - u)
x_ <- dnorm(u_)
x  <- (x + x_)/2
ax <- x[1:(nsamples/2)]

c(estimador = mean(ax), error.std = sd(ax)/sqrt(nsamples), N = length(ax))

## Ejemplo: variables control ================================================

set.seed(108)
x <- rnorm(nsamples)

f_x <- x**6/(1 + x**2)
c(estimador = mean(f_x), error.std = sd(f_x)/sqrt(nsamples))

g_x <- 3 - 1 / (1 + x**2)
c(estimador = mean(g_x), error.std = sd(g_x)/sqrt(nsamples) )

set.seed(108)
x <- rnorm(100 * nsamples)
x <- array(x, c(100, nsamples))
f_x <- x**6/(1 + x**2)
estimadores <- apply(f_x, 1, mean)
c(estimador = mean(estimadores), error.std = sd(estimadores))

g_x <- 3 - 1/(1+x**2)
estimadores <- apply(g_x, 1, mean)
c(estimador = mean(estimadores), error.std = sd(estimadores))

## monte carlo condicional ===================================================

set.seed(108); 
theta <- rbeta(nsamples, 2, 5)
y <- rbinom(nsamples, size = 20, theta)
c(estimador = mean(y), error.std = sd(y)/sqrt(nsamples))

m_y <- 20 * theta
c(estimador = mean(m_y), error.std = sd(m_y)/sqrt(nsamples))

(sd(y) - sd(m_y))/sd(y)

### ejemplo: poisson beta ====================================================

set.seed(108)
w <- rpois(nsamples, 10)
y <- rbeta(nsamples, w, w**2 + 1)
c(estimador = mean(y), error.std = sd(y)/sqrt(nsamples))

m_y <- w / (w**2 + w + 1)
c(estimador = mean(m_y), error.std = sd(m_y)/sqrt(nsamples))

(sd(y) - sd(m_y))/sd(y)

### ejemplo: estimacion de densidades ========================================

nsamples <- 5 * 10^3; ngrid <- 1000; k <- 4
rpareto <- function(n, alpha) { 1 / runif(n)^(1/alpha) - 1 }
dpareto <- function(x, alpha) {
  ifelse( x >= 0, (alpha / ((x+1)**(alpha + 1))), 0) }
u <- rpareto( (k-1) * nsamples, alpha = 3/2)
u <- array(u, c(k-1, nsamples))
S <- apply(u, 2, sum)
x <- seq(0.1, 15, length.out = ngrid)

estimador <- array(x, c(ngrid,1)) |>
  apply(1, FUN = function(x_){ dpareto(x_ - S, alpha = 3/2) }) |>
  apply(2, mean)

error.std <- array(x, c(ngrid,1)) |>
  apply(1, FUN = function(x_){ dpareto(x_ - S, alpha = 3/2) }) |>
  apply(2, sd)

k <- 8
u <- rpareto( (k-1) * nsamples, alpha = 3/2)
u <- array(u, c(k-1, nsamples))
S <- apply(u, 2, sum)

estimador.8 <- array(x, c(ngrid,1)) |>
  apply(1, FUN = function(x_){ dpareto(x_ - S, alpha = 3/2) }) |>
  apply(2, mean)

error.std.8 <- array(x, c(ngrid,1)) |>
  apply(1, FUN = function(x_){ dpareto(x_ - S, alpha = 3/2) }) |>
  apply(2, sd)

g1 <- tibble(x, estimador, error.std) |>
ggplot(aes(x, estimador)) +
  geom_ribbon(aes(ymin = estimador - 2 * error.std/sqrt(nsamples),
                  ymax = estimador + 2 * error.std/sqrt(nsamples)),
              fill = 'salmon', alpha = .3) + 
  geom_line() + sin_lineas + ggtitle(expression(k==4))

g2 <- tibble(x, estimador = estimador.8, error.std = error.std.8) |>
ggplot(aes(x, estimador)) +
  geom_ribbon(aes(ymin = estimador - 2 * error.std/sqrt(nsamples),
                  ymax = estimador + 2 * error.std/sqrt(nsamples)),
              fill = 'salmon', alpha = .3) + 
  geom_line() + sin_lineas + ggtitle(expression(k==8))

g1 + g2

### Ejemplo: constuctura =====================================================

K <- 20; nsamples <- 10^4
sigma <- rexp(nsamples, 1/4)
mu    <- rnorm(nsamples, mean = 10, sd = 4)
x     <- rnorm(nsamples, mean = mu, sd = sigma)
costo <- 1000 * ifelse( x <= K, 0, x - K)

c(media = mean(costo), error.std = sd(costo)/sqrt(nsamples))

costo.cond <- 1000 * (sigma * dnorm((K - mu)/sigma) - (K - mu) * pnorm( (mu - K)/sigma ))
c(media = mean(costo.cond), error.std = sd(costo.cond)/sqrt(nsamples))

(sd(costo) - sd(costo.cond))/sd(costo)

## Monte Carlo estratificado =================================================

nsamples <- 5000
h <- function(u) { 4 * sqrt(1 - u**2) }
u <- runif(100 * nsamples)
u <- array(u, c(100, nsamples))
h_u <- h(u)
estimador_MC <- apply(h_u, 1, cummean) |> t()

as_tibble(t(estimador_MC[1:50,])) |>
  mutate(n = 1:nsamples) |>
  pivot_longer(cols = 1:50) |>
  ggplot(aes(n, value, group = name)) +
  geom_line(aes(color = name), alpha = .8) +
  geom_hline(yintercept = pi, lty = 2) + 
  scale_x_continuous(trans='log10', 
                     labels = trans_format("log10", math_format(10^.x))) + 
  ylab('Aproximación') + xlab("Número de muestras") + sin_lineas + sin_leyenda

runif_estrat <- function(u){
  x <- c()
  for (jj in 1:nsamples){
    x[jj] <- (u[jj] + jj - 1)/nsamples 
  }
  return(x)
}
u_strat <- apply(u, 1, runif_estrat) |> t()

h_strat <- h(u_strat)
estimador_sMC <- apply(h_strat, 1, mean)

calcula_antitetic <- function(u){
  x <- c()
  for (jj in 1:nsamples){
    x[jj] <- h((u[jj] + jj - 1)/nsamples) + h((jj - u[jj])/nsamples)
  }
  return(0.5 * x)
}
h_anti <- apply(u, 1, calcula_antitetic) |> t()

estimador_asMC <- apply(h_anti, 1, mean)

options(digits = 7)
tibble(metodo = c('vainilla', 'estratificado', 'anti-estratificado'),
       estimador = c( apply(h_u, 1, mean) |> mean(),
                      apply(h_strat, 1, mean) |> mean(),
                      apply(h_anti, 1, mean) |> mean()),
       error.mc = c( apply(h_u, 1, mean) |> sd(),
                    apply(h_strat, 1, mean) |> sd(),
                    apply(h_anti, 1, mean) |> sd())
       ) |> as.data.frame()

## Ambiente ==================================================================

sessionInfo()
