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

## Para el tema de ggplot
color.itam  <- c("#00362b","#004a3b", "#00503f", "#006953", "#008367", "#009c7b", "#00b68f", NA)
sin_leyenda <- theme(legend.position = "none")
sin_ejes <- theme(axis.ticks = element_blank(), axis.text = element_blank())
sin_lineas <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

x0 <- 3; a <- 3; m <- 5;
x  <- x0; 
for (jj in 2:10){
  x[jj] <- (a * x[jj-1]) %% m
}
x

x0 <- 3; a <- 2; m <- 11;
x  <- x0; 
for (jj in 2:20){
  x[jj] <- (a * x[jj-1]) %% m
}
x

u <- x[1:(m-1)] / m
u

runif(1)

runif(100, min = 7, max = 10)

runif(5)
runif(5)

set.seed(108); runif(5)
set.seed(108); runif(5)

set.seed(10)
samples <- tibble(x = runif(30))
g1 <- samples |>
  ggplot(aes(x)) +
  stat_ecdf(geom = "step") +
  geom_abline(intercept = 0, slope = 1, lty = 2) + 
  sin_lineas +
  ylab("Función de acumulación") + xlab("x") +
  ggtitle("n = 30") + xlim(0,1)
g2 <- tibble(x = runif(500)) |>
  ggplot(aes(x)) +
  stat_ecdf(geom = "step") +
  geom_abline(intercept = 0, slope = 1, lty = 2) + 
  sin_lineas +
  ylab("Función de acumulación") + xlab("x") +
  ggtitle("n = 500")
g1 + g2

Pn <- ecdf(samples$x)
x_ <- seq(0, 1, length = 1000)
Dn <- max(abs(Pn(x_) - punif(x_)))
print(paste("Distancia: ", Dn))

set.seed(10)
experiment <- function(id){
     Fn <- ecdf(runif(30))
     max(abs(Fn(x_) - punif(x_)))
  }
replicas <- tibble(id = 1:5000) |>
  mutate(estadistico = map_dbl(id, experiment))

replicas |>
  ggplot(aes(estadistico)) +
  geom_histogram() + sin_lineas +
  geom_vline(xintercept = Dn, lty = 2, color = 'red', lwd = 1.5)

print(paste("Probabilidad: ", mean(replicas$estadistico >= Dn)))

ks.test(samples$x, "punif")
