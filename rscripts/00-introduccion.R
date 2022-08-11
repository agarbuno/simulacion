## Setup --------------------------------------------
library(tibble)
library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(scales)
## Cambia el default del tamaño de fuente 
theme_set(theme_linedraw(base_size = 25))

## Cambia el número de decimales para mostrar
options(digits = 2)

sin_lineas <- theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())
color.itam  <- c("#00362b","#004a3b", "#00503f", "#006953", "#008367", "#009c7b", "#00b68f", NA)

sin_lineas <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
sin_leyenda <- theme(legend.position = "none")
sin_ejes <- theme(axis.ticks = element_blank(), axis.text = element_blank())

genera_circulo <- function(n = 10){
  tibble(angulo = seq(0, 2*pi, length.out = n),
         x = sin(angulo), y = cos(angulo))
}

tibble(n = 2**c(3, 4, 8)) |>
  mutate(datos = map(n, genera_circulo)) |>
  unnest(datos) |>
  ggplot(aes(x, y)) + 
  geom_path(aes(group = n, lty = factor(n))) +
  coord_equal() + xlab(expression(x[1])) + ylab(expression(x[2])) + 
  sin_lineas + sin_leyenda + sin_ejes
