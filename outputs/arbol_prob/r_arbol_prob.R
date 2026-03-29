## ============================================================
## Árbol de Probabilidad — 2 niveles — ggplot2
## Para integrar en ejercicios R-exams (.Rmd)
## ============================================================

library(ggplot2)

## ----------------------------------------------------------
## PARÁMETROS (modificar aquí para parametrizar el ejercicio)
## ----------------------------------------------------------
p_llueve    <- 0.6
p_no_llueve <- 1 - p_llueve

p_traf_dado_llueve    <- 0.7
p_notraf_dado_llueve  <- 1 - p_traf_dado_llueve

p_traf_dado_no_llueve    <- 0.3
p_notraf_dado_no_llueve  <- 1 - p_traf_dado_no_llueve

etiq_raiz      <- "Inicio"
etiq_n1_izq    <- "Llueve"
etiq_n1_der    <- "No llueve"
etiq_n2_ll_izq <- "Tráfico"
etiq_n2_ll_der <- "Sin tráfico"
etiq_n2_no_izq <- "Tráfico"
etiq_n2_no_der <- "Sin tráfico"

titulo <- "Árbol de Probabilidad"

## Función para formato decimal español
es <- function(x) gsub("\\.", ",", format(round(x, 4), nsmall = 1))

## ----------------------------------------------------------
## PROBABILIDADES FINALES (hojas)
## ----------------------------------------------------------
pf_ll_traf   <- p_llueve * p_traf_dado_llueve
pf_ll_notraf <- p_llueve * p_notraf_dado_llueve
pf_no_traf   <- p_no_llueve * p_traf_dado_no_llueve
pf_no_notraf <- p_no_llueve * p_notraf_dado_no_llueve

## ----------------------------------------------------------
## PALETA DE COLORES
## ----------------------------------------------------------
col_inicio_borde  <- "#1971c2"
col_inicio_fondo  <- "#a5d8ff"

col_llueve_borde  <- "#2f9e44"
col_llueve_fondo  <- "#b2f2bb"

col_nolueve_borde <- "#e8590c"
col_nolueve_fondo <- "#ffd8a8"

col_hoja_ll_fondo  <- "#d4edda"
col_hoja_ll_borde  <- "#2f9e44"

col_hoja_no_fondo  <- "#ffe8cc"
col_hoja_no_borde  <- "#e8590c"

col_prob_texto <- "#1971c2"
col_prob_fondo <- "#e8f4fd"
col_prob_borde <- "#1971c2"

## ----------------------------------------------------------
## COORDENADAS DE NODOS (centros)
## El árbol crece hacia ABAJO: cy_r (raíz) > cy_ll/no > cy_l2/n2
## ----------------------------------------------------------
cx_r  <- 0;    cy_r  <- 3.6
cx_ll <- -3.5; cy_ll <- 1.8
cx_no <-  3.5; cy_no <- 1.8

cx_ll_izq <- -5.5; cy_l2 <- 0
cx_ll_der <- -1.5
cx_no_izq <-  1.5; cy_n2 <- 0
cx_no_der <-  5.5

# Half-width / half-height de los rectángulos
hw      <- 0.75;  hh      <- 0.38
hw_hoja <- 0.90;  hh_hoja <- 0.38

## ----------------------------------------------------------
## POSICIÓN DE ETIQUETAS EN RAMAS
## Los segmentos van de borde INFERIOR del padre al borde SUPERIOR del hijo
## ----------------------------------------------------------
mid_y_01  <- (cy_r  - hh + cy_ll + hh) / 2
mid_y_01b <- (cy_r  - hh + cy_no + hh) / 2
mid_y_ll  <- (cy_ll - hh + cy_l2 + hh_hoja) / 2
mid_y_no  <- (cy_no - hh + cy_n2 + hh_hoja) / 2

## ----------------------------------------------------------
## CONSTRUCCIÓN DEL GRÁFICO
## ----------------------------------------------------------
p <- ggplot() +

  ## 1 — SEGMENTOS (ramas) — padre borde INFERIOR → hijo borde SUPERIOR ——
  # Raíz → Nivel 1
  annotate("segment",
    x = cx_r,  xend = cx_ll,
    y = cy_r - hh, yend = cy_ll + hh,
    color = col_inicio_borde, linewidth = 1.5, lineend = "round") +
  annotate("segment",
    x = cx_r,  xend = cx_no,
    y = cy_r - hh, yend = cy_no + hh,
    color = col_inicio_borde, linewidth = 1.5, lineend = "round") +
  # Llueve → Nivel 2
  annotate("segment",
    x = cx_ll, xend = cx_ll_izq,
    y = cy_ll - hh, yend = cy_l2 + hh_hoja,
    color = col_llueve_borde, linewidth = 1.4, lineend = "round") +
  annotate("segment",
    x = cx_ll, xend = cx_ll_der,
    y = cy_ll - hh, yend = cy_l2 + hh_hoja,
    color = col_llueve_borde, linewidth = 1.4, lineend = "round") +
  # No llueve → Nivel 2
  annotate("segment",
    x = cx_no, xend = cx_no_izq,
    y = cy_no - hh, yend = cy_n2 + hh_hoja,
    color = col_nolueve_borde, linewidth = 1.4, lineend = "round") +
  annotate("segment",
    x = cx_no, xend = cx_no_der,
    y = cy_no - hh, yend = cy_n2 + hh_hoja,
    color = col_nolueve_borde, linewidth = 1.4, lineend = "round") +

  ## 2 — RECTÁNGULOS (nodos) ————————————————————————————————
  # Raíz
  annotate("rect",
    xmin = cx_r - hw, xmax = cx_r + hw,
    ymin = cy_r - hh, ymax = cy_r + hh,
    fill = col_inicio_fondo, color = col_inicio_borde, linewidth = 1.4) +
  # Nivel 1 — Llueve
  annotate("rect",
    xmin = cx_ll - hw, xmax = cx_ll + hw,
    ymin = cy_ll - hh, ymax = cy_ll + hh,
    fill = col_llueve_fondo, color = col_llueve_borde, linewidth = 1.3) +
  # Nivel 1 — No llueve
  annotate("rect",
    xmin = cx_no - hw, xmax = cx_no + hw,
    ymin = cy_no - hh, ymax = cy_no + hh,
    fill = col_nolueve_fondo, color = col_nolueve_borde, linewidth = 1.3) +
  # Nivel 2 — hojas de "Llueve"
  annotate("rect",
    xmin = cx_ll_izq - hw_hoja, xmax = cx_ll_izq + hw_hoja,
    ymin = cy_l2 - hh_hoja, ymax = cy_l2 + hh_hoja,
    fill = col_hoja_ll_fondo, color = col_hoja_ll_borde, linewidth = 1.2) +
  annotate("rect",
    xmin = cx_ll_der - hw_hoja, xmax = cx_ll_der + hw_hoja,
    ymin = cy_l2 - hh_hoja, ymax = cy_l2 + hh_hoja,
    fill = col_hoja_ll_fondo, color = col_hoja_ll_borde, linewidth = 1.2) +
  # Nivel 2 — hojas de "No llueve"
  annotate("rect",
    xmin = cx_no_izq - hw_hoja, xmax = cx_no_izq + hw_hoja,
    ymin = cy_n2 - hh_hoja, ymax = cy_n2 + hh_hoja,
    fill = col_hoja_no_fondo, color = col_hoja_no_borde, linewidth = 1.2) +
  annotate("rect",
    xmin = cx_no_der - hw_hoja, xmax = cx_no_der + hw_hoja,
    ymin = cy_n2 - hh_hoja, ymax = cy_n2 + hh_hoja,
    fill = col_hoja_no_fondo, color = col_hoja_no_borde, linewidth = 1.2) +

  ## 3 — TEXTO EN NODOS ————————————————————————————————————
  annotate("text",
    x = cx_r, y = cy_r, label = etiq_raiz,
    color = col_inicio_borde, size = 4.0, fontface = "bold") +
  annotate("text",
    x = cx_ll, y = cy_ll, label = etiq_n1_izq,
    color = col_llueve_borde, size = 3.5, fontface = "bold") +
  annotate("text",
    x = cx_no, y = cy_no, label = etiq_n1_der,
    color = col_nolueve_borde, size = 3.5, fontface = "bold") +
  annotate("text",
    x = cx_ll_izq, y = cy_l2, label = etiq_n2_ll_izq,
    color = col_hoja_ll_borde, size = 3.2, fontface = "bold") +
  annotate("text",
    x = cx_ll_der, y = cy_l2, label = etiq_n2_ll_der,
    color = col_hoja_ll_borde, size = 3.2, fontface = "bold") +
  annotate("text",
    x = cx_no_izq, y = cy_n2, label = etiq_n2_no_izq,
    color = col_hoja_no_borde, size = 3.2, fontface = "bold") +
  annotate("text",
    x = cx_no_der, y = cy_n2, label = etiq_n2_no_der,
    color = col_hoja_no_borde, size = 3.2, fontface = "bold") +

  ## 4 — PROBABILIDADES EN RAMAS ————————————————————————————
  # Raíz → Nivel 1
  annotate("text",
    x = cx_ll - 0.7, y = mid_y_01,
    label = paste0("P = ", es(p_llueve)),
    color = col_llueve_borde, size = 3.0, fontface = "italic") +
  annotate("text",
    x = cx_no + 0.7, y = mid_y_01b,
    label = paste0("P = ", es(p_no_llueve)),
    color = col_nolueve_borde, size = 3.0, fontface = "italic") +
  # Llueve → Nivel 2
  annotate("text",
    x = cx_ll_izq - 0.55, y = mid_y_ll,
    label = paste0("P = ", es(p_traf_dado_llueve)),
    color = col_llueve_borde, size = 2.9, fontface = "italic") +
  annotate("text",
    x = cx_ll_der + 0.75, y = mid_y_ll,
    label = paste0("P = ", es(p_notraf_dado_llueve)),
    color = col_llueve_borde, size = 2.9, fontface = "italic") +
  # No llueve → Nivel 2
  annotate("text",
    x = cx_no_izq - 0.6, y = mid_y_no,
    label = paste0("P = ", es(p_traf_dado_no_llueve)),
    color = col_nolueve_borde, size = 2.9, fontface = "italic") +
  annotate("text",
    x = cx_no_der + 0.75, y = mid_y_no,
    label = paste0("P = ", es(p_notraf_dado_no_llueve)),
    color = col_nolueve_borde, size = 2.9, fontface = "italic") +

  ## 5 — PROBABILIDADES FINALES (cajas DEBAJO de cada hoja) —————
  annotate("label",
    x = cx_ll_izq, y = cy_l2 - hh_hoja - 0.60,
    label = paste0(es(p_llueve), " \u00d7 ", es(p_traf_dado_llueve),
                   " = ", es(pf_ll_traf)),
    fill = col_prob_fondo, color = col_prob_texto,
    label.r = unit(0.18, "lines"),
    size = 2.8, fontface = "bold") +
  annotate("label",
    x = cx_ll_der, y = cy_l2 - hh_hoja - 0.60,
    label = paste0(es(p_llueve), " \u00d7 ", es(p_notraf_dado_llueve),
                   " = ", es(pf_ll_notraf)),
    fill = col_prob_fondo, color = col_prob_texto,
    label.r = unit(0.18, "lines"),
    size = 2.8, fontface = "bold") +
  annotate("label",
    x = cx_no_izq, y = cy_n2 - hh_hoja - 0.60,
    label = paste0(es(p_no_llueve), " \u00d7 ", es(p_traf_dado_no_llueve),
                   " = ", es(pf_no_traf)),
    fill = col_prob_fondo, color = col_prob_texto,
    label.r = unit(0.18, "lines"),
    size = 2.8, fontface = "bold") +
  annotate("label",
    x = cx_no_der, y = cy_n2 - hh_hoja - 0.60,
    label = paste0(es(p_no_llueve), " \u00d7 ", es(p_notraf_dado_no_llueve),
                   " = ", es(pf_no_notraf)),
    fill = col_prob_fondo, color = col_prob_texto,
    label.r = unit(0.18, "lines"),
    size = 2.8, fontface = "bold") +

  ## Título y tema
  labs(title = titulo) +
  theme_void(base_size = 13) +
  theme(
    plot.title  = element_text(
      hjust = 0.5, face = "bold", size = 16,
      color = "#333333", margin = margin(b = 8)
    ),
    plot.margin = margin(t = 14, r = 20, b = 20, l = 20)
  ) +
  ## Fijar proporciones y dejar espacio para cajas de prob. final
  coord_fixed(
    ratio  = 1,
    xlim   = c(-7.2, 7.2),
    ylim   = c(-1.5, 4.5),
    expand = FALSE,
    clip   = "off"
  )

## ----------------------------------------------------------
## GUARDAR
## ----------------------------------------------------------
output_file <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/outputs/arbol_prob/arbol_prob_r.png"

ggsave(
  filename = output_file,
  plot     = p,
  width    = 12,
  height   = 8,
  dpi      = 150,
  bg       = "white"
)

cat("PNG guardado en:", output_file, "\n")
