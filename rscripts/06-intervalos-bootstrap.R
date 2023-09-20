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

## Error estandar e intervalos normales --------------------------------------

te <- read_csv("data/tea.csv") |>
  rowid_to_column() |>
  select(rowid, Tea, sugar)

## paso 1: define el estimador
calc_estimador <- function(datos){
  prop_negro <- datos |>
    mutate(negro = ifelse(Tea == "black", 1, 0)) |>
    summarise(prop_negro = mean(negro), n = length(negro)) |>
    pull(prop_negro)
  prop_negro
}

## calcula el estimador
prop_hat <- calc_estimador(te)
prop_hat |> round(4)

prop_negro_tbl <- read_rds("cache/prop_negro_tbl.rds")
g_hist <- ggplot(prop_negro_tbl, aes(x = prop_negro)) + geom_histogram(bins = 15) + sin_lineas
g_qq_normal <- ggplot(prop_negro_tbl, aes(sample = prop_negro)) +
  geom_qq() + geom_qq_line(colour = "red") + sin_lineas
g_hist + g_qq_normal

prop_negro_tbl <- read_rds("cache/prop_negro_tbl.rds")
media_boot <- prop_negro_tbl |> pull(prop_negro) |> mean()
media_boot - prop_hat

ee_boot <- prop_negro_tbl |> pull(prop_negro) |> sd()
ee_boot

intervalo_95 <- c(inf = prop_hat - 2 * ee_boot,
                  centro = prop_hat,
                  sup = prop_hat + 2 * ee_boot)
intervalo_95 |> round(3)

## Ejemplo casas vendidas --------------------------------------------------

## muestra original
set.seed(121)
poblacion_casas <- read_csv("data/casas.csv")
muestra_casas   <- read_rds("cache/casas_muestra.rds")

## paso 1: define el estimador
estimador_lote <- function(split, ...){
  N <- nrow(poblacion_casas)
  muestra <- analysis(split)
  muestra |>
    summarise(estimate = (N / n()) * sum(precio_miles)) |>
    mutate(term = "Valor lote")
}

totales_boot <- bootstraps(muestra_casas,  5000) |>  ## paso 2 y 3
  mutate(res_boot = map(splits, estimador_lote))    ## paso 4

totales_boot

## paso 4: examina la distribución bootstrap
g_hist <- totales_boot |>
  unnest(res_boot) |>
  mutate(total_boot = estimate) |>
  ggplot(aes(x = total_boot)) +
  geom_histogram() + sin_lineas +
  geom_vline(xintercept = quantile(totales_boot$total_boot, 0.975), colour = "gray") +
  geom_vline(xintercept = quantile(totales_boot$total_boot, 0.025), colour = "gray")
g_qq <- totales_boot |>
  unnest(res_boot) |>
  mutate(total_boot = estimate) |>
  ggplot(aes(sample = total_boot)) +
  geom_qq() + geom_qq_line(colour = "red") +
  geom_hline(yintercept = quantile(totales_boot$total_boot, 0.975), colour = "gray") +
  geom_hline(yintercept = quantile(totales_boot$total_boot, 0.025), colour = "gray") +
  sin_lineas
g_hist + g_qq

estimador.obs <- muestra_casas |>
  summarise(estimador = (nrow(poblacion_casas)/n() * sum(precio_miles))) |>
  pull(estimador)
estimador.obs

resumen_boot <- totales_boot |>
  unnest(res_boot) |>
  summarise(media.boot = mean(estimate)) |>
  mutate(sesgo = media.boot - estimador.obs)
resumen_boot

resumen_boot |>
  mutate(sesgo_relativo = sesgo / estimador.obs)

intervalos_normales <- totales_boot |>
  unnest(res_boot) |>
  summarise(media_boot = mean(estimate), ee_boot = sd(estimate)) |>
  mutate(inf = media_boot - 2 * ee_boot, sup = media_boot + 2 * ee_boot)
intervalos_normales

intervalos_normales / 1000

## Cobertura de intervalos --------------------------------------

## Para usar resultados en cache:
sims_intervalos <- read_rds("cache/sims_intervalos.rds")

sims_tbl <- sims_intervalos |>
  bind_rows () |>
  mutate(cubre = inf < total & total < sup)

total <- sum(poblacion_casas$precio_miles)
ggplot(sims_tbl, aes(x = rep)) +
  geom_hline(yintercept = total, colour = "red") +
  geom_linerange(aes(ymin = inf, ymax = sup, colour = cubre)) + sin_lineas

total <- sum(poblacion_casas$precio_miles)
sims_tbl |>
  summarise(cobertura = mean(cubre))

## Intervalos de percentiles  --------------------------------------

g_hist2 <- totales_boot|>
  ggplot(aes(x = total_boot)) +
  geom_histogram(aes(y = ..density..)) + 
  stat_function(fun = dnorm, args = list(mean = total_est, sd = ee_boot),
                color = 'red', lty = 2) +
  sin_lineas

g_hist2 + g_qq

intervalos_normales / 1000

totales_boot |> unnest(res_boot) |> 
  mutate(upper = estimate >= max(intervalos_normales$sup), 
         lower = estimate <= min(intervalos_normales$inf)) |>
  summarise(prop_inf = mean(lower), 
            prop_sup = mean(upper))

intervalo_95 <- totales_boot |> unnest(res_boot) |>
  pull(estimate) |>
  quantile(probs = c(0.025, 0.50, 0.975))
intervalo_95 / 1000

abs(intervalo_95 - estimador.obs)/1000

## en este ejemplo usamos rsample, pero puedes escribir tu propio código
library(rsample)
propinas <- read_csv("data/propinas.csv",
                     progress = FALSE,
                     show_col_types = FALSE) |>
  mutate(id = 1:244)

propinas

## paso 1: define el estimador
estimador <- function(split, ...){
  muestra <- analysis(split) |> group_by(momento)
  muestra |>
    summarise(estimate = mean(cuenta_total), .groups = 'drop') |>
    mutate(term = momento)
}

## paso 2 y 3: remuestrea y calcula estimador
boot_samples <- bootstraps(propinas, strata = momento, 1000) |>
  mutate(res_boot = map(splits, estimador))
## paso 4: construye intervalos de confianza
intervalo_propinas_90 <- boot_samples |>
  int_pctl(res_boot, alpha = 0.10) |> 
  mutate(across(where(is.numeric), round, 2))
intervalo_propinas_90

estimadores <- propinas |>
  group_by(momento) |> 
  rename(term = momento) |> 
  summarise(media = mean(cuenta_total))

ggplot(intervalo_propinas_90, aes(x = term)) +
  geom_linerange(aes(ymin = .lower, ymax = .upper)) +
  geom_point(data = estimadores, aes(y = media), colour = "red", size = 3) +
  xlab("Momento") + ylab("Media de cuenta total (dólares)") +
  labs(subtitle = "Intervalos de 90% para la media") + sin_lineas

## Funciones de computo ------------------------------------------------------

boot_samples

boot_samples$splits[[1]]

analysis(boot_samples$splits[[1]]) |>
  group_by(id)

library(pryr)
c(objeto_boot = object_size(boot_samples),
  original    = object_size(propinas),
  remuestra   = object_size(boot_samples)/nrow(boot_samples), 
  incremento  = object_size(boot_samples)/object_size(propinas))

## Correccion de intervalos --------------------------------------------------

totales_boot <- bootstraps(muestra_casas,  2000, apparent = TRUE) |> 
  mutate(res_boot = map(splits, estimador_lote))
totales_boot |> tail()

totales_boot |>
  int_pctl(res_boot) |>
  select(- .alpha ) |>
  mutate_if(is.numeric, function(x) {x/1000}) |>
  mutate(length = .upper - .lower)

intervalos_bca <- totales_boot |>
  int_bca(res_boot, .fn = estimador_lote)

intervalos_bca |>
  select(- .alpha ) |>
  mutate_if(is.numeric, function(x) {x/1000}) |>
  mutate(length = .upper - .lower)

estimador_razon <- function(split, ...){
  muestra <- analysis(split)
  muestra |>
    summarise(estimate = sum(area_habitable_sup_m2) / sum(area_lote_m2),
              .groups = "drop") |>
    mutate(term = "area del lote construida")
}

razon_boot <- bootstraps(muestra_casas,  2000, apparent = TRUE) |> 
  mutate(res_boot = map(splits, estimador_razon))

razon_boot |>
  int_pctl(res_boot) |>
  select(- .alpha ) |>
  mutate_if(is.numeric, function(x) {x*100}) |>
  mutate(length = .upper - .lower)

intervalos_bca <- razon_boot |>
  int_bca(res_boot, .fn = estimador_razon)

intervalos_bca |>
  select(- .alpha ) |>
  mutate_if(is.numeric, function(x) {x*100}) |>
  mutate(.length = .upper - .lower)

resample_data <- poblacion_casas |>
  mc_cv(prop = 200/1144, 2000) |>
  mutate(results = map(splits, estimador_razon))
resample_data

resample_data |>
  unnest(results) |>
  summarise(inf = quantile(estimate, probs = c(0.025)) * 100,
            sup = quantile(estimate, probs = c(0.975)) * 100) |>
  mutate(length = sup - inf)

sessionInfo()
