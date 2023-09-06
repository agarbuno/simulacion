## Setup --------------------------------------------------

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

 color.itam  <- c("#00362b","#004a3b", "#00503f", "#006953", "#008367", "#009c7b", "#00b68f", NA)
 sin_lineas <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
 sin_leyenda <- theme(legend.position = "none")
 sin_ejes <- theme(axis.ticks = element_blank(), 
       axis.text = element_blank())

 ## Ejemplo de integracion numerica -----------------------

 grid.n          <- 11                 # Número de celdas 
 grid.size       <- 6/(grid.n+1)       # Tamaño de celdas en el intervalo [-3, 3]
 norm.cuadrature <- tibble(x = seq(-3, 3, by = grid.size), y = dnorm(x) )

 norm.density <- tibble(x = seq(-5, 5, by = .01), 
        y = dnorm(x) )

norm.cuadrature |>
  ggplot(aes(x=x + grid.size/2, y=y)) + 
  geom_area(data = norm.density, aes(x = x, y = y), fill = 'lightblue') + 
  geom_bar(stat="identity", alpha = .3) + 
  geom_bar(aes(x = x + grid.size/2, y = -0.01), fill = 'black', stat="identity") + 
  sin_lineas + xlab('') + ylab("") + 
  annotate('text', label = expression(Delta~u[n]),
           x = .01 + 5 * grid.size/2, y = -.02, size = 12) + 
  annotate('text', label = expression(f(u[n]) ),
           x = .01 + 9 * grid.size/2, y = dnorm(.01 + 4 * grid.size/2), size = 12) + 
  annotate('text', label = expression(f(u[n]) * Delta~u[n]), 
           x = .01 + 5 * grid.size/2, y = dnorm(.01 + 4 * grid.size/2)/2, 
           angle = -90, alpha = .7, size = 12) + sin_ejes

grid.n          <- 101                 # Número de celdas 
grid.size       <- 6/(grid.n+1)       # Tamaño de celdas en el intervalo [-3, 3]
norm.cuadrature <- tibble(x = seq(-3, 3, by = grid.size), y = dnorm(x) )

norm.cuadrature |>
    ggplot(aes(x=x + grid.size/2, y=y)) + 
    geom_area(data = norm.density, aes(x = x, y = y), fill = 'lightblue') + 
    geom_bar(stat="identity", alpha = .3) + 
    geom_bar(aes(x = x + grid.size/2, y = -0.01), fill = 'black', stat="identity") + 
    sin_lineas + xlab('') + ylab("") + 
    annotate('text', label = expression(Delta~u[n]),
             x = .01 + 5 * grid.size/2, y = -.02, size = 12) + 
    annotate('text', label = expression(f(u[n]) ),
             x = .01 + 9 * grid.size/2, y = dnorm(.01 + 4 * grid.size/2), size = 12) + 
    annotate('text', label = expression(f(u[n]) * Delta~u[n]), 
             x = .01 + 5 * grid.size/2, y = dnorm(.01 + 4 * grid.size/2)/2, 
             angle = -90, alpha = .7, size = 12) + sin_ejes

grid.n          <- 11                 # Número de celdas 
grid.size       <- 6/(grid.n+1)       # Tamaño de celdas en el intervalo [-3, 3]
norm.cuadrature <- tibble(x = seq(-5, 0, by = grid.size),
                          y.lo = dnorm(x - grid.size/2), y.hi = dnorm(x + grid.size/2))
norm.density.half <- tibble(x = seq(-5, 0, by = .01), y = dnorm(x - grid.size/2) ) 

g1 <- norm.cuadrature |>
  ggplot(aes(x=x + grid.size/2, y=y.lo)) + 
  geom_area(data = norm.density.half, aes(x = x, y = y), fill = 'lightblue') + 
  geom_bar(stat="identity", alpha = .3) + 
  geom_bar(aes(x = x + grid.size/2, y = -0.005), fill = 'black', stat="identity") + 
  sin_lineas + xlab('') + ylab("") + sin_ejes + xlim(-5,0)

g2 <- norm.cuadrature |>
  ggplot(aes(x=x + grid.size/2, y=y.hi)) + 
  geom_area(data = norm.density.half, aes(x = x, y = y), fill = 'lightblue') + 
  geom_bar(stat="identity", alpha = .3) + 
  geom_bar(aes(x = x + grid.size/2, y = -0.005), fill = 'black', stat="identity") + 
  sin_lineas + xlab('') + ylab("") + sin_ejes + xlim(-5, 0)

g1 + g2

canvas <- ggplot(faithful, aes(x = eruptions, y = waiting)) +
  xlim(0.5, 6) +
  ylim(40, 110)

grid.size <- 10 - 1

mesh <- expand.grid(x = seq(0.5, 6, by = (6-.5)/grid.size),
                    y = seq(40, 110, by = (110-40)/grid.size))

g1 <- canvas +
  geom_density_2d_filled(aes(alpha = ..level..), bins = 8) +
  scale_fill_manual(values = rev(color.itam)) + 
  sin_lineas + theme(legend.position = "none") +
  geom_point(data = mesh, aes(x = x, y = y)) + 
  annotate("rect", xmin = .5 + 5 * (6-.5)/grid.size, 
           xmax = .5 + 6 * (6-.5)/grid.size, 
           ymin = 40 + 3 * (110-40)/grid.size, 
           ymax = 40 + 4 * (110-40)/grid.size,
           linestyle = 'dashed', 
           fill = 'salmon', alpha = .4) + ylab("") + xlab("") + 
  annotate('text', x = .5 + 5.5 * (6-.5)/grid.size, 
           y = 40 + 3.5 * (110-40)/grid.size, 
           label = expression(u[n]), color = 'red', size = 15) +
  theme(axis.ticks = element_blank(), 
        axis.text = element_blank())


g2 <- canvas + 
  stat_bin2d(aes(fill = after_stat(density)), binwidth = c((6-.5)/grid.size, (110-40)/grid.size)) +
  sin_lineas + theme(legend.position = "none") +
  theme(axis.ticks = element_blank(), 
        axis.text = element_blank()) +
  scale_fill_distiller(palette = "Greens", direction = 1) + 
  sin_lineas + theme(legend.position = "none") +
  ylab("") + xlab("")

g3 <- canvas + 
  stat_bin2d(aes(fill = after_stat(density)), binwidth = c((6-.5)/25, (110-40)/25)) +
  sin_lineas + theme(legend.position = "none") +
  theme(axis.ticks = element_blank(), 
        axis.text = element_blank()) +
  scale_fill_distiller(palette = "Greens", direction = 1) + 
  sin_lineas + theme(legend.position = "none") +
  ylab("") + xlab("")

g1 + g2 + g3

## Integración Monte ========================================================= 
genera_dardos <- function(n = 100){
    tibble(x1 = runif(n, min = -1, max = 1), 
           x2 = runif(n, min = -1, max = 1)) %>% 
      mutate(resultado = ifelse(x1**2 + x2**2 <= 1., 1., 0.))
  }

  dardos <- tibble(n = seq(2,5)) %>% 
    mutate(datos = map(10**n, genera_dardos)) %>% 
    unnest() 

  dardos %>% 
    ggplot(aes(x = x1, y = x2)) + 
      geom_point(aes(color = factor(resultado))) + 
      facet_wrap(~n, nrow = 1) +  
    sin_lineas + sin_ejes + sin_leyenda + coord_equal()

set.seed(1087)
genera_dardos(n = 2**16) %>% 
  mutate(n = seq(1, 2**16), 
         approx = cummean(resultado) * 4) %>% 
  ggplot(aes(x = n, y = approx)) + 
    geom_line() + 
    geom_hline(yintercept = pi, linetype = 'dashed') + 
    scale_x_continuous(trans='log10', 
                       labels = trans_format("log10", math_format(10^.x))) + 
  ylab('Aproximación') + xlab("Número de muestras") + sin_lineas

set.seed(108)
nsamples <- 10**4; nexp <- 100
U <- runif(nexp * 2 * nsamples)
U <- array(U, dim = c(nexp, 2, nsamples))

str(U)

U[1:5] |> str()

U[1:5,,] |> str()

apply(U[1:5,,], 1, str)

cuenta_dardos <- function(x){
  ## obtiene la norma 2
  dardos <- apply(x**2, 2, sum)
  ## cuenta si los dardos estan en el circulo unitario
  exitos <- ifelse(dardos <= 1, 1, 0)
  ## obtiene frecuencias relativas
  prop   <- mean(exitos)
  4 * prop
}

resultados <- apply(U, 1, cuenta_dardos)
resultados

tibble(x = resultados) |>
ggplot(aes(x)) +
geom_histogram(bins = 20) + sin_lineas +
geom_vline(xintercept = pi, lty = 2, color = "salmon", size =2)

cuenta_acumulacion_dardos <- function(x){
  ## obtiene la norma 2
  dardos <- apply(x**2, 2, sum)
  ## cuenta si los dardos estan en el circulo unitario
  exitos <- ifelse(dardos <= 1, 1, 0)
  ## obtiene frecuencias relativas mientras avanza N
  prop   <- cummean(exitos)
  4 * prop
}

resultados <- apply(U, 1, cuenta_acumulacion_dardos)
resultados |> str()
## Nota como transpone el resultado

resultados |>
  as_data_frame() |>
  mutate(n = 1:nsamples) |>
  pivot_longer(cols = 1:10) |>
  ggplot(aes(n, value)) +
  geom_line(aes(group = name, color = name)) +
  geom_hline(yintercept = pi, linetype = 'dashed') + 
  scale_x_continuous(trans='log10', 
                     labels = trans_format("log10", math_format(10^.x))) + 
  ylab('Aproximación') + xlab("Número de muestras") + sin_lineas + sin_leyenda +
  ylim(0, 7)

resultados |>
  as_data_frame() |>
  mutate(n = 1:nsamples) |>
  pivot_longer(cols = 1:nexp) |>
  group_by(n) |>
  summarise(promedio = mean(value),
            desv.est = sd(value),
            y.lo = promedio - 2 * desv.est,
            y.hi = promedio + 2 * desv.est) |>
  ggplot(aes(n , promedio)) +
  geom_ribbon(aes(ymin = y.lo, ymax = y.hi), fill = "gray", alpha = .3) +
  geom_ribbon(aes(ymin = promedio - 2 * sqrt(pi * (4 - pi)/(n)),
                  ymax = promedio + 2 * sqrt(pi * (4 - pi)/(n))),
              fill = "salmon", alpha = .1) +
  geom_hline(yintercept = pi, linetype = 'dashed') + 
  geom_line() +
  scale_x_continuous(trans='log10', 
                     labels = trans_format("log10", math_format(10^.x))) + 
  ylab('Aproximación') + xlab("Número de muestras") + sin_lineas + sin_leyenda +
ylim(0, 7)

resultados |>
  as_data_frame() |>
  mutate(n = 1:nsamples) |>
  pivot_longer(cols = 1:nexp) |>
  group_by(n) |>
  summarise(varianza = var(value/4)) |>
  mutate(cramer.rao = pi * (4 - pi)/(16 * n)) |>
  ggplot(aes(n , varianza)) +
  geom_line() +
  geom_line(aes(n, cramer.rao), lty = 2, color = 'red') +
  scale_y_continuous(trans='log10') +
  scale_x_continuous(trans='log10', 
                     labels = trans_format("log10", math_format(10^.x))) + 
  ylab('Varianza') + xlab("Número de muestras") + sin_lineas + sin_leyenda

options(digits = 8)

tibble(N = 1:nsamples, estimado = resultados[,1]) |>
  mutate( diff.abs = abs(estimado - pi)) |>
  filter(N %% 10 == 0 & log10(N) %in% c(1, 2, 3, 4))

set.see(108); nsamples <- 10**4; nexp <- 100
h <- function(x){ exp(-x**2/2) }
u <- runif(nexp * nsamples, min = 2, max = 3)
x <- array(u, c(nexp, nsamples))
h_x <- h(x)

estimador_mc <- apply(h_x, 1, cummean)    # ojo, transpone el resultado
media_mc <- apply(estimador_mc, 1, mean)
error_mc <- apply(estimador_mc, 1, sd)

estimador.uniforme <- estimador_mc
as.tibble(estimador_mc) |>
  mutate(n = 1:nsamples) |>
  pivot_longer(cols = 1:20) |>
  ggplot(aes(n, value, color = name)) +
  geom_line() +
  geom_hline(yintercept = sqrt(2 * pi) * (pnorm(3) - pnorm(2)), lty = 2) + 
  scale_x_continuous(trans='log10', 
                     labels = trans_format("log10", math_format(10^.x))) + 
  ylab('Aproximación') + xlab("Número de muestras") + sin_lineas + sin_leyenda

as.tibble(estimador_mc) |>
  mutate(n = 1:nsamples) |>
  ggplot(aes(n, V1)) +
  geom_ribbon(aes(ymin = V1 - 2 * error_mc,
                  ymax = V1 + 2 * error_mc),
              alpha = .2, fill = 'salmon') + 
  geom_line() +
  geom_hline(yintercept = sqrt(2 * pi) * (pnorm(3) - pnorm(2)), lty = 2) +
  scale_x_continuous(trans='log10', 
                     labels = trans_format("log10", math_format(10^.x))) + 
  ylab('Aproximación') + xlab("Número de muestras") + sin_lineas + sin_leyenda

gunif <- tibble(n = 1:nsamples, media = media_mc, error = error_mc) |>
  ggplot(aes(n, media)) +
  geom_ribbon(aes(ymin = media - 2 * error,
                  ymax = media + 2 * error), alpha = .2, fill = 'salmon') +
  geom_line() +
  geom_hline(yintercept = sqrt(2 * pi) * (pnorm(3) - pnorm(2)), lty = 2) + 
  scale_x_continuous(trans='log10', 
                     labels = trans_format("log10", math_format(10^.x))) + 
  ylab('Aproximación') + xlab("Número de muestras") + sin_lineas + sin_leyenda

gunif

set.seed(108); nsamples <- 10**4; nexp <- 100
f <- function(x){ ifelse(x >= 2 & x <= 3, sqrt(2 * pi), 0) }
u <- rnorm(nexp * nsamples)
x <- array(u, c(nexp, nsamples))
f_x <- f(x)

estimador_mc <- apply(f_x, 1, cummean)    # ojo, transpone el resultado
media_mc <- apply(estimador_mc, 1, mean)
error_mc <- apply(estimador_mc, 1, sd)

estimador.normal <- estimador_mc
as.tibble(estimador_mc) |>
    mutate(n = 1:nsamples) |>
    pivot_longer(cols = 1:20) |>
    ggplot(aes(n, value, color = name)) +
    geom_line() +
    geom_hline(yintercept = sqrt(2 * pi) * (pnorm(3) - pnorm(2)), lty = 2) + 
    scale_x_continuous(trans='log10', 
                       labels = trans_format("log10", math_format(10^.x))) + 
    ylab('Aproximación') + xlab("Número de muestras") + sin_lineas + sin_leyenda

as.tibble(estimador_mc) |>
  mutate(n = 1:nsamples) |>
  ggplot(aes(n, V1)) +
  geom_ribbon(aes(ymin = V1 - 2 * error_mc,
                  ymax = V1 + 2 * error_mc),
              alpha = .2, fill = 'salmon') + 
  geom_line() +
  geom_hline(yintercept = sqrt(2 * pi) * (pnorm(3) - pnorm(2)), lty = 2) +
  scale_x_continuous(trans='log10', 
                     labels = trans_format("log10", math_format(10^.x))) + 
  ylab('Aproximación') + xlab("Número de muestras") + sin_lineas + sin_leyenda

gnormal <- tibble(n = 1:nsamples, media = media_mc, error = error_mc) |>
  ggplot(aes(n, media)) +
  geom_ribbon(aes(ymin = media - 2 * error,
                  ymax = media + 2 * error), alpha = .2, fill = 'salmon') +
  geom_line() +
  geom_hline(yintercept = sqrt(2 * pi) * (pnorm(3) - pnorm(2)), lty = 2) + 
  scale_x_continuous(trans='log10', 
                     labels = trans_format("log10", math_format(10^.x))) + 
  ylab('Aproximación') + xlab("Número de muestras") + sin_lineas + sin_leyenda

gnormal

gunif + gnormal

(gunif + ylim(-.55, .55)) + (gnormal + ylim(-.55, .55))

media_mc <- apply(estimador.normal, 1, mean)
error_mc <- apply(estimador.normal, 1, sd)

truth <- sqrt(2 * pi) * (pnorm(3) - pnorm(2))
as.tibble(estimador.normal) |>
    mutate(n = 1:nsamples, error = error_mc) |>
    pivot_longer(cols = 1:100) |>
    filter(log10(n) %in% c(2, 3, 4)) |>
    mutate(contains =
             factor(ifelse(truth <= value + 2 * error & truth >= value - 2 * error, 1, 0))) |>
    ggplot(aes(value, name)) +
    geom_point(aes(color = contains)) +
    geom_linerange(aes(xmin = value - 2 * error,
                       xmax = value + 2 * error,
                       color = contains), alpha = .4, lwd = 1.2) +
    geom_vline(xintercept = truth, lty = 2) +
    facet_wrap(~n, scales = "free_x") + sin_lineas + sin_leyenda +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
  xlab("") + ylab("")

media_mc <- apply(estimador.uniforme, 1, mean)
error_mc <- apply(estimador.uniforme, 1, sd)

truth <- sqrt(2 * pi) * (pnorm(3) - pnorm(2))
as.tibble(estimador.uniforme) |>
    mutate(n = 1:nsamples, error = error_mc) |>
    pivot_longer(cols = 1:100) |>
    filter(log10(n) %in% c(2, 3, 4)) |>
    mutate(contains =
             factor(ifelse(truth <= value + 2 * error & truth >= value - 2 * error, 1, 0))) |>
    ggplot(aes(value, name)) +
    geom_point(aes(color = contains)) +
    geom_linerange(aes(xmin = value - 2 * error,
                       xmax = value + 2 * error,
                       color = contains), alpha = .4, lwd = 1.2) +
    geom_vline(xintercept = truth, lty = 2) +
    facet_wrap(~n, scales = "free_x") + sin_lineas + sin_leyenda +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
  xlab("") + ylab("")

set.seed(108727); nsamples <- 10**4; nexp <- 100
f <- function(theta){ pi * exp(-0.5 * (1 - theta)**2) }
h <- function(theta){ theta * f(theta) }
theta <- rcauchy(nexp * nsamples)
theta <- array(theta, c(nexp, nsamples))
f_theta <- f(theta)
h_theta <- h(theta)

num_estimador_mc   <- apply(h_theta, 1, cummean) # ojo, transpone el resultado
denom_estimador_mc <- apply(f_theta, 1, cummean) # ojo, transpone el resultado
estimador_mc <- num_estimador_mc / denom_estimador_mc
media_mc <- apply(estimador_mc, 1, mean)
error_mc <- apply(estimador_mc, 1, sd)

estimador.cauchy <- estimador_mc
as.tibble(estimador_mc) |>
    mutate(n = 1:nsamples) |>
    pivot_longer(cols = 1:100) |>
    ggplot(aes(n, value, group = name)) +
    geom_line(alpha = .2, color = "salmon") +
    geom_line(data = tibble(value = media_mc, n = 1:nsamples, name = "baseline"),
              aes(n, value), color = "black", lwd = 1.2) +
    ## geom_hline(yintercept = sqrt(2 * pi) * (pnorm(3) - pnorm(2)), lty = 2) + 
    scale_x_continuous(trans='log10', 
                       labels = trans_format("log10", math_format(10^.x))) +
    coord_cartesian(ylim = c(-.5, 1.5)) + 
    ylab('Aproximación') + xlab("Número de muestras") + sin_lineas + sin_leyenda

tibble(n = 1:nsamples, error = error_mc) |>
ggplot(aes(n, error)) +
geom_line() +
geom_line(aes(n, 5/n), color = 'salmon', lty = 2) + 
scale_x_continuous(trans='log10', 
                   labels = trans_format("log10", math_format(10^.x))) +
scale_y_continuous(trans='log10') + sin_lineas +
xlab("Numero de simulaciones") + ylab("Error estándar")

truth <- media_mc[nsamples]
as.tibble(estimador.cauchy) |>
    mutate(n = 1:nsamples, error = error_mc) |>
    pivot_longer(cols = 1:100) |>
    filter(log10(n) %in% c(2, 3, 4)) |>
    mutate(contains =
             factor(ifelse(truth <= value + 2 * error & truth >= value - 2 * error, 1, 0))) |>
    ggplot(aes(value, name)) +
    geom_point(aes(color = contains)) +
    geom_linerange(aes(xmin = value - 2 * error,
                       xmax = value + 2 * error,
                       color = contains), alpha = .4, lwd = 1.2) +
    geom_vline(xintercept = truth, lty = 2) +
    facet_wrap(~n, scale = "free_x") + sin_lineas + sin_leyenda +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
  xlab("") + ylab("")

sessionInfo()
