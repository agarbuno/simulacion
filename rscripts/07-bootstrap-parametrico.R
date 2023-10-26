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
options(width=60)

## Para el tema de ggplot
color.itam  <- c("#00362b","#004a3b", "#00503f", "#006953", "#008367", "#009c7b", "#00b68f", NA)
sin_leyenda <- theme(legend.position = "none")
sin_ejes <- theme(axis.ticks = element_blank(), axis.text = element_blank())
sin_lineas <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

set.seed(41852)
muestra <- rnorm(150, mean = 1, sd = 2)

mle.obs <- broom::tidy(MASS::fitdistr(muestra, "normal")) |>
  tibble::column_to_rownames("term")
mle.obs

## paso 1: define el estimador
estimador_mle <- function(datos, modelo = "normal"){
  datos |>
    MASS::fitdistr(modelo) |>
    broom::tidy() |>
    select(-std.error)
}

## paso 2: define el proceso de remuestreo
paramboot_sample <- function(data){
  rnorm(length(data),
        mean = mle.obs["mean", "estimate"],
        sd = mle.obs["sd", "estimate"])
}

## paso 3: define el paso bootstrap
paso_bootstrap <- function(id){
  muestra |>
    paramboot_sample() |>
    estimador_mle()
}

## paso 4: aplica bootstrap parametrico
boot_mle <- map_df(1:5000, paso_bootstrap)

gqq <- boot_mle |>
  ggplot(aes(sample = estimate)) +
  geom_qq() + geom_qq_line(colour = "red") +
  facet_wrap(~term, scales = "free_y") + sin_lineas

ghist <- boot_mle |>
  ggplot(aes(x = estimate)) +
  geom_histogram() + 
  facet_wrap(~term, scales = "free") + sin_lineas

gqq / ghist

set.seed(4182)
muestra <- rnorm(6, mean = 1, sd = 2)
mle.obs <- broom::tidy(MASS::fitdistr(muestra, "normal")) |>
  tibble::column_to_rownames("term")
mle.obs

## paso 4: aplica bootstrap parametrico
boot_mle <- map_df(1:5000, paso_bootstrap)

gqq <- boot_mle |>
  ggplot(aes(sample = estimate)) +
  geom_qq() + geom_qq_line(colour = "red") +
  facet_wrap(~term, scales = "free_y") + sin_lineas

ghist <- boot_mle |>
  ggplot(aes(x = estimate)) +
  geom_histogram() + 
  facet_wrap(~term, scales = "free") + sin_lineas

gqq / ghist

propinas <- read_csv("data/propinas.csv",
                     progress = FALSE,
                     show_col_types = FALSE) |>
  mutate(id = 1:244)
propinas |> head()

## paso 1: define el estimador
estimador <- function(split, ...){
  muestra <- analysis(split) |> group_by(momento)
  muestra |>
    summarise(estimate = mean(cuenta_total), .groups = 'drop') |>
    mutate(term = momento)
}

## paso 2 y 3: remuestrea y calcula estimador
boot_samples <- bootstraps(propinas, strata = momento, 500) |>
  mutate(res_boot = map(splits, estimador))
## paso 4: construye intervalos de confianza
intervalos_noparam <- boot_samples |>
  int_pctl(res_boot, alpha = 0.05) |> 
  mutate(across(where(is.numeric), round, 2))
intervalos_noparam

## paso 1: define estimador
estimador_mle_grupos <- function(muestra, modelo = "normal") {
  muestra |>
    select(momento, cuenta_total) |>
    group_by(momento) |>
    nest(data = cuenta_total) |>
    summarise(mle = map(data, function(x) {
      nobs <- nrow(x)
      unlist(x) |>
        estimador_mle(modelo = modelo) |>
        mutate(n = nobs)
    }))
}

mle.obs <- estimador_mle_grupos(propinas, "normal")
mle.obs |> unnest(mle)

## paso 2: define proceso de remuestreo
param_boot_grupos <- function(estimadores){
  estimadores |>
    group_by(momento) |>
    mutate(simulaciones = map(mle, function(m){
      tibble(cuenta_total = rnorm(m$n[1], m$estimate[1], sd = m$estimate[2]))
    })) |>
    unnest(simulaciones) |>
    select(-mle) |>
    ungroup()
}

## paso 3: paso bootstrap
paso_bootstrap_grupos <- function(id){
  param_boot_grupos(mle.obs) |>
    estimador_mle_grupos()
}

## paso 4: aplica bootstrap y presenta intervalos 
intervalos_param <- tibble(id = 1:500)|>
  mutate(estimadores = map(id, paso_bootstrap_grupos)) |>
  unnest(estimadores) |>
  unnest(mle) |>
  group_by(momento, term) |>
  summarise(.lower = quantile(estimate, 0.025),
            .estimate = mean(estimate),
            .upper = quantile(estimate, 0.975),
            .alpha = .05,
            .method = "percentile (normal)", .groups = "drop") |>
  filter(term == "mean") |> select(-term)
intervalos_param

intervalos_noparam

library(resampledata)
data(Turbine)
Turbine |> tibble()

Turbine |>
  summarise(estimate = quantile(Production, probs = .1))

library(rsample)
## paso 1: define el estimador
calcula_percentil <- function(split, ...) {
  split |>
    analysis() |>
    summarise(estimate = quantile(Production, probs = .1)) |>
    mutate(term = "Percentil")
}

nonparam_boot <- bootstraps(Turbine, 1000) |>
  mutate(resultados = map(splits, calcula_percentil))

gnpw <- nonparam_boot |>
  unnest(resultados) |>
  ggplot(aes(estimate)) +
  geom_histogram() +
  sin_lineas
gnpw

## paso 1: define el estimador
ajusta_weibull <- function(data){
  tibble(data) |>
    filter(Production > 0) |>
    pull(Production) |>
    MASS::fitdistr("weibull") |>
    broom::tidy() |>
    select(-std.error) |>
    tibble::column_to_rownames("term")
}

mle.weibull <- ajusta_weibull(Turbine)
mle.weibull

## paso 2: define el proceso de remuestreo
paramboot_sample <- function(data){
  tibble(Production = rweibull(nrow(data),
                               scale = mle.weibull["scale", "estimate"],
                               shape = mle.weibull["shape", "estimate"])
        )
}

## paso 1.5: complementa el estimador
extrae_cuantil <- function(params){
  qweibull(scale = params["scale", "estimate"],
           shape = params["shape", "estimate"],
           p = .10) %>%
    tibble(estimate = .)
}

## paso 3: define el paso bootstrap
paso_bootstrap <- function(id){
  Turbine |>
    paramboot_sample() |>
    ajusta_weibull() |>
    extrae_cuantil()
}

## paso 4: aplica bootstrap parametrico
param_boot <- map_df(1:1000, paso_bootstrap)

gpw <- param_boot |>
  ggplot(aes(estimate)) +
  geom_histogram() +
  sin_lineas
gnpw + gpw

nonparam_boot |> int_pctl("resultados") |>
  mutate(.method = "percentile (noparam)",
         .length = .upper - .lower)

param_boot |>
  summarise(term = "Percentil",
            .lower = quantile(estimate, .05),
            .estimate = mean(estimate),
            .upper = quantile(estimate, .95),
            .alpha = 0.05,
            .method = "percentile (param)") |>
  mutate(.length = .upper - .lower)

sessionInfo()
