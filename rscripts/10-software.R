## Setup --------------------------------------------
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
sin_lineas <- theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())
color.itam  <- c("#00362b","#004a3b", "#00503f", "#006953", "#008367", "#009c7b", "#00b68f", NA)

sin_leyenda <- theme(legend.position = "none")
sin_ejes <- theme(axis.ticks = element_blank(), axis.text = element_blank())

library(LearnBayes)
minmaxpost <- function(theta, data){
  mu <- theta[1]
  sigma <- exp(theta[2])
  dnorm(data$min, mu, sigma, log = TRUE) +
    dnorm(data$max, mu, sigma, log = TRUE) +
    ((data$n - 2) * log(pnorm(data$max, mu, sigma) -
                        pnorm(data$min, mu, sigma)))
}

data <- list(n = 10, min = 52, max = 84)
fit  <- laplace(minmaxpost, c(70, 2), data)
fit

mcmc.fit <- rwmetrop(minmaxpost,
                     list(var = fit$v, scale = 3),
                     c(70, 2),
                     10000,
                     data)

mcmc.fit$accept

mu.samp <- mcmc.fit$par[, 1]
sigma.samp <- exp(mcmc.fit$par[, 2])
tibble(cuantil = mu.samp + 0.674 * sigma.samp) |>
  ggplot(aes(cuantil)) +
  geom_histogram() + sin_lineas

library(cmdstanr)
modelos_files <- "modelos/compilados/software"
ruta <- file.path("modelos/software/minmax.stan")
modelo <- cmdstan_model(ruta, dir = modelos_files)

muestras <- modelo$sample(data = list(N = 10, xmin = 52, xmax = 84),
              chains = 4,
              iter = 1500,
              iter_warmup = 500,
              seed = 108727,
              refresh = 500)

muestras$summary()

muestras$draws(format = "df") |>
  pivot_longer(cols = 2:4, names_to = "parameter") |>
  group_by(parameter) |>
  summarise(media = mean(value), std.dev = sd(value),
            error.mc = std.dev/(n()), samples = n())

modelo$optimize(data = list(N = 10, xmin = 52, xmax = 84),
                refresh = 0)$mle()

modelo$variational(data = list(N = 10, xmin = 52, xmax = 84),
                   refresh = 0, seed = 108727)
