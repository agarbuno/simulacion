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

## Idea de bootstrap ---------------------------------------------------------

set.seed(2112)
poblacion_casas <- read_csv("data/casas.csv")
muestra <- sample_n(poblacion_casas, 200, replace = FALSE) |>
  select(id, nombre_zona, area_habitable_sup_m2, precio_miles)

muestra |> print(n = 5)

sprintf("Hay %0.0f casas en total, tomamos muestra de %0.0f",
      nrow(poblacion_casas), nrow(muestra))

mean(muestra$precio_miles)

bind_rows(muestra |> mutate(tipo = "muestra"),
    poblacion_casas |> mutate(tipo = "población")) |>
ggplot(aes(x = precio_miles, group = tipo)) + 
    geom_histogram(aes(y=..density..), binwidth = 50) + 
  facet_wrap(~ tipo) + sin_lineas + sin_leyenda

media <- mean(muestra$precio_miles)
media

## paso 1: define el estimador
calcula_estimador <- function(data){
  data |>
    summarise(media_precio = mean(precio_miles), .groups = "drop")
}

## paso 2: define el proceso de remuestreo
genera_remuestras <- function(data, n = 200){
  data |>
    sample_n(200, replace = TRUE)
}

## paso 3: definimos el paso bootstrap
paso_bootstrap <-function(id){
  muestra |>
  genera_remuestras() |>
    calcula_estimador() |>
    pull(media_precio)
}

## paso 4: aplica el procedimiento bootstrap
media_muestras <- map_dbl(1:1000, paso_bootstrap)
media_muestras[1:9]

bootstrap <- tibble(media = media_muestras)
g_cuantiles <- ggplot(bootstrap, aes(sample = media)) +
  geom_qq(distribution = stats::qunif) +
  ggtitle("QQ-plots de la distribución \nde la media") +
  sin_lineas
g_histograma <- ggplot(bootstrap, aes(x = media)) +
  geom_histogram(binwidth = 2) + sin_lineas +
  ggtitle("Histograma de la distribución \nde la media")
g_cuantiles + g_histograma

limites_ic <- quantile(media_muestras, c(0.05,  0.95)) |> round(4)
limites_ic

ee_boot <- sd(media_muestras)
round(ee_boot, 2)

### Ejemplo: muestreo aleatorio con reemplazo ================================

muestra <- sample(1:20, 5)
muestra

sample(muestra, size = 5, replace = TRUE)

## Datos de te (plugin) ------------------------------------------------------

te <- read_csv("data/tea.csv") |>
  rowid_to_column() |>
  select(rowid, Tea, sugar)

te |>
  mutate(negro = ifelse(Tea == "black", 1, 0)) |>
  summarise(prop_negro = mean(negro), n = length(negro), .groups = "drop")

muestra.obs <- te |>
  mutate(negro = ifelse(Tea == "black", 1, 0)) |>
  summarise(media = mean(negro), n = length(negro), .groups = "drop")

## paso 1: define el estimador
calc_estimador <- function(datos){
  prop_negro <- datos |>
    mutate(negro = ifelse(Tea == "black", 1, 0)) |>
    summarise(prop_negro = mean(negro), n = length(negro), .groups = "drop") |>
    pull(prop_negro)
  prop_negro
}

## paso 2: define el proceso de remuestreo
muestra_boot <- function(datos){
  ## tomar muestra con reemplazo del mismo tamaño
  sample_n(datos, size = nrow(datos), replace = TRUE)
}

## paso 3: definimos el paso bootstrap
paso_bootstrap <- function(id){
  muestra_boot(datos = te) |>
    calc_estimador()
}

## paso 4: aplica el procedimiento bootstrap
prop_negro_tbl <- map_dbl(1:2000, paso_bootstrap ) |>
  as_tibble() |>
  rename( prop_negro = value)

write_rds(prop_negro_tbl, "cache/prop_negro_tbl.rds")

## paso 5: examina la distribución bootstrap
prop_negro_tbl |>
  ggplot(aes(x = prop_negro)) +
  geom_histogram(bins = 15) + sin_lineas

prop_negro_tbl |>
  summarise(
    cuantil_25 = quantile(prop_negro, 0.25),
    cuantil_75 = quantile(prop_negro, 0.75), 
    media = mean(prop_negro),
    ee = sd(prop_negro)/sqrt(muestra.obs$n),
    sesgo = mean(prop_negro) - muestra.obs$media,
    .groups = "drop") |>
  mutate(across(where(is.numeric), round, 4))

## Propiedadesde distirbucion bootstrap --------------------------------------

set.seed(911)
## Generamos 20 conjuntos de datos observados 
muestras <- map(1:16, function(x) {
  muestra <- sample_n(poblacion_casas, 200, replace = F) |>
    mutate(rep = x, tipo = "muestras")
}) |> bind_rows()
## Agregamos las columnas tipo y rep
dat_pob <- poblacion_casas |> mutate(tipo = "población", rep = 1)
## Pegamos las tablas
datos_sim <- bind_rows(dat_pob, muestras)

## paso 1: define el estimador
calc_estimador <- function(datos){
  media_precio <- datos |>
    summarise(media = mean(precio_miles), .groups = "drop") |>
    pull(media)
  media_precio
}

## paso 2: define el proceso de remuestreo
muestra_boot <- function(datos, n = NULL){
  ## tomar muestra con reemplazo del mismo tamaño
  if(is.null(n)){
      m <- sample_n(datos, size = nrow(datos), replace = TRUE)}
  else {
      m <- sample_n(datos, size = n, replace = TRUE)
    }
  m
}

## paso 3: definimos el paso bootstrap
paso_bootstrap <- function(data, n = NULL){
  data |>
    muestra_boot(n) |>
    calc_estimador()
}

## paso 4: define el procedimiento bootstrap
procedimiento_bootstrap <- function(data){
  tibble(precio_miles = rerun(1000, paso_bootstrap(data)))
}

dist_muestreo <- read_rds("cache/sims_muestreo_precios.rds")
dist_boot <- read_rds("cache/sims_boot_precios.rds")
mean_boot <- dist_boot |> summarise(media = mean(precio_miles)) |> filter(rep <= 16)
dist_boot |>
  ungroup() |>
  filter(rep <= 16) |>
  ggplot(aes(x = precio_miles)) +
  geom_histogram(data = dist_muestreo |> ungroup() |> select(precio_miles),
                 fill = "lightblue", alpha = .6,
                 position = "identity", bins = 20) +
  geom_histogram(alpha = .6, fill = "salmon", bins = 20) +
  geom_vline(data = mean_boot, aes(xintercept = media), color = 'black', lty = 2) +
  geom_vline(data = dist_muestreo |>
               ungroup() |>
               select(precio_miles) |>
               summarise(media = mean(precio_miles)),
             aes(xintercept = media), color = 'black') +
  facet_wrap(~rep) + sin_lineas

poblacion_casas |>
  summarise(media = mean(precio_miles), .groups = "drop")

tibble(n = 1:100) |>
  mutate(combinaciones = choose(2 * n - 1, n)) |>
  ggplot(aes(n, combinaciones)) + geom_point() + geom_line() +
  geom_hline(yintercept = choose(11, 6), lty = 2)+
  geom_vline(xintercept = 6, lty = 2) + 
  scale_x_continuous(trans='log10', 
                     labels = trans_format("log10", math_format(10^.x))) +
  scale_y_continuous(trans='log10', 
                     labels = trans_format("log10", math_format(10^.x))) + 
  sin_lineas

## Bootstrap para razon y suavizadores ---------------------------------------

set.seed(250)
casas_muestra <- sample_n(poblacion_casas, 200)

options(width = 70)
casas_muestra |> glimpse()

write_rds(casas_muestra, "cache/casas_muestra.rds")

## paso 1: define el estimador
estimador_razon <- function(split, ...){
  muestra <- analysis(split)
  muestra |>
    summarise(estimate = sum(area_habitable_sup_m2) / sum(area_lote_m2),
              .groups = "drop") |>
    mutate(term = "area del lote construida")
}

estimador <- casas_muestra |>
  summarise(estimate = sum(area_habitable_sup_m2) / sum(area_lote_m2))
estimador

library(rsample)
dist_boot <- bootstraps(casas_muestra,  2000) |>   ## paso 2 y 3
  mutate(res_boot = map(splits, estimador_razon))  ## paso 4

g_1 <- ggplot(dist_boot %>% unnest(res_boot), aes(x = estimate)) +
  geom_histogram(bins = 20) + sin_lineas
g_2 <- ggplot(dist_boot %>% unnest(res_boot), aes(sample = estimate)) +
  geom_qq() + geom_qq_line(colour = 'red') + sin_lineas
g_1 + g_2

dist_boot |> int_pctl(res_boot) |>
  mutate(estimador = estimador$estimate) |>
  rename(media_boot = .estimate) |>
  mutate(sesgo = media_boot - estimador) |>
  select(-.method, -term)

graf_casas <- function(data){
  ggplot(data %>% filter(calidad_gral < 7), 
         aes(x = area_habitable_sup_m2)) + 
    geom_point(aes(y = precio_m2_miles), alpha = 0.75) +
    geom_smooth(aes(y = precio_m2_miles), method = "loess", span = 0.7, 
                se = FALSE, method.args = list(degree = 1, family = "symmetric")) +
    sin_lineas 
}
graf_casas(casas_muestra)

suaviza_boot <- function(x, data){
  ## remuestreo
  muestra_boot <- sample_n(data, nrow(data), replace = T)
  ajuste <- loess(precio_m2_miles ~ area_habitable_sup_m2, data = muestra_boot, 
                  degree = 1, span = 0.7, family = "symmetric")
  datos_grafica <- tibble(area_habitable_sup_m2 = seq(25, 250, 5))
  ajustados <- predict(ajuste, newdata = datos_grafica)
  datos_grafica %>% mutate(ajustados = ajustados) %>% 
    mutate(rep = x)
}
reps <- map(1:10, ~ suaviza_boot(.x, casas_muestra %>% filter(calidad_gral < 7))) %>% 
  bind_rows()

## ojo: la rutina loess no tienen soporte para extrapolación
graf_casas(casas_muestra) + 
  geom_line(data = reps, aes(y = ajustados, group = rep), alpha = 1, colour = "red") +
  coord_cartesian(xlim = c(50, 225))

reps <- map(1:200, ~ suaviza_boot(.x, casas_muestra %>% filter(calidad_gral < 7))) %>% 
  bind_rows()
## ojo: la rutina loess no tienen soporte para extrapolación
graf_casas(casas_muestra) + 
  geom_line(data = reps, aes(y = ajustados, group = rep), alpha = 0.2, colour = "red") +
  coord_cartesian(xlim = c(50, 225))

## Ambiente ==================================================================
options(width = 75)

sessionInfo()
