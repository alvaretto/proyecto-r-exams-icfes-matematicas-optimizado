#!/usr/bin/env Rscript
library(ggplot2)
set.seed(42)
g <- 9.8; v0 <- 11.2; n_puntos <- 99
angulos <- c(runif(15, 0.05, 0.25), runif(25, 0.25, 0.55), runif(30, 0.55, 1.0), runif(20, 1.0, 1.35), runif(9, 1.35, 1.55))
alcance_teorico <- (v0^2 * sin(2 * angulos)) / g
ruido_factor <- 0.38 * sqrt(pmax(0.3, alcance_teorico / max(alcance_teorico))) * sqrt(alcance_teorico)
ruido <- rnorm(n_puntos, 0, ruido_factor)
alcance <- pmax(0.3, pmin(13.5, alcance_teorico + ruido))
datos <- data.frame(angulo = angulos, alcance = alcance)
p <- ggplot(datos, aes(x = angulo, y = alcance)) +
  geom_point(color = "#00CED1", shape = 18, size = 3.2, alpha = 0.9) +
  scale_x_continuous(name = "Ángulo (en radianes)", breaks = seq(0, 1.6, 0.2), limits = c(0, 1.7)) +
  scale_y_continuous(name = "Alcance horizontal (m)", breaks = seq(0, 14, 2), limits = c(0, 15)) +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(color = "gray80"), panel.grid.major.x = element_line(color = "gray90"),
        panel.grid.minor = element_blank(), axis.line = element_line(color = "black"),
        plot.background = element_rect(fill = "white", color = NA), panel.background = element_rect(fill = "white", color = NA))
ggsave("r_output_v3.png", plot = p, width = 9, height = 6, dpi = 150)
cat("R v3 generado\n")
