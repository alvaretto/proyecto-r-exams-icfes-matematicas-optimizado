# Generador R/ggplot2 - barras campeonato baloncesto (autocontenido)
library(ggplot2)

.tema_baloncesto <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 15, hjust = 0.5, lineheight = 1.05),
      axis.title.y = element_text(face = "bold", size = 13),
      axis.title.x = element_blank(),
      axis.text = element_text(colour = "grey15", size = 10.5),
      panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(colour = "grey75", linewidth = 0.3),
      axis.line = element_line(colour = "grey10", linewidth = 0.5),
      legend.title = element_blank(), legend.position = "right",
      legend.text = element_text(size = 11),
      legend.key.size = unit(0.45, "cm"), legend.key.spacing.y = unit(0.25, "cm"),
      plot.background = element_rect(fill = "white", colour = NA),
      plot.margin = margin(8, 12, 8, 8)
    )
}

.pasos_y <- function(ymax) seq(0, ymax, by = if (ymax <= 15) 1 else 2)

barras_simples <- function(valores, categorias, colores, titulo, etiqueta_y, archivo_png) {
  df <- data.frame(cat = factor(categorias, levels = categorias), v = valores)
  ymax <- ceiling(max(valores))
  g <- ggplot(df, aes(cat, v, fill = cat)) +
    geom_col(width = 0.3) +
    scale_fill_manual(values = setNames(colores, categorias), guide = "none") +
    scale_y_continuous(breaks = .pasos_y(ymax), limits = c(0, ymax), expand = expansion(mult = c(0, 0.04))) +
    labs(title = titulo, y = etiqueta_y) +
    .tema_baloncesto() +
    theme(axis.line.y = element_line(colour = "grey10", linewidth = 0.5))
  ggsave(archivo_png, g, width = 6, height = 4.2, dpi = 150, bg = "white")
  invisible(archivo_png)
}

# matriz: filas = grupos (2), columnas = categorias (2)
barras_opcion <- function(matriz, grupos, categorias, colores, modo = "agrupada",
                          titulo, etiqueta_y, archivo_png, ymax = NULL) {
  df <- data.frame(
    grupo = factor(rep(grupos, times = length(categorias)), levels = grupos),
    cat   = factor(rep(categorias, each = length(grupos)), levels = categorias),
    v     = as.vector(matriz))
  if (is.null(ymax))
    ymax <- ceiling(if (modo == "apilada") max(colSums(matriz)) else max(matriz))
  g <- ggplot(df, aes(cat, v, fill = grupo)) +
    { if (modo == "apilada")
        geom_col(position = position_stack(reverse = FALSE), width = 0.4)
      else geom_col(position = position_dodge(width = 0.75), width = 0.7) } +
    scale_fill_manual(values = setNames(colores, grupos),
                      guide = guide_legend(reverse = FALSE)) +
    scale_y_continuous(breaks = .pasos_y(ymax), limits = c(0, ymax), expand = expansion(mult = c(0, 0.04))) +
    labs(title = titulo, y = etiqueta_y) +
    .tema_baloncesto()
  ggsave(archivo_png, g, width = 6, height = 4.2, dpi = 150, bg = "white")
  invisible(archivo_png)
}
