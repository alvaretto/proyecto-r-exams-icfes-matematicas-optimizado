# Cargar bibliotecas necesarias
library(xtable)

# Función para generar datos aleatorios y crear diagrama de caja
crear_boxplot_aleatorio <- function(base_min = 155, base_max = 172, 
                                  min_n = 7, max_n = 19, 
                                  solo_impares = TRUE,
                                  variacion = 10) {
  
  # Validar parámetros de entrada
  if (!is.numeric(base_min) || !is.numeric(base_max) || 
      !is.numeric(min_n) || !is.numeric(max_n) ||
      !is.logical(solo_impares)) {
    stop("Los parámetros deben ser numéricos y solo_impares debe ser lógico")
  }
  
  # Generar límites aleatorios dentro del rango de variación
  min_valor <- base_min - sample(0:variacion, 1)
  max_valor <- base_max + sample(0:variacion, 1)
  
  # Generar tamaño de la muestra
  n <- if (solo_impares) {
    min_n <- if (min_n %% 2 == 0) min_n + 1 else min_n
    max_n <- if (max_n %% 2 == 0) max_n - 1 else max_n
    posibles_n <- seq(min_n, max_n, by = 2)
    sample(posibles_n, 1)
  } else {
    sample(min_n:max_n, 1)
  }
  
  # Generar datos aleatorios con distribución normal truncada
  datos <- round(runif(n, min_valor, max_valor))
  
  # Asegurar que al menos algunos datos estén cerca de los extremos
  datos[1] <- min_valor + sample(-2:2, 1)
  datos[2] <- max_valor + sample(-2:2, 1)
  
  # Crear una tabla con los datos ordenados y numerados
  datos_ordenados <- sort(datos)
  datos_tabla <- data.frame(
    "Dato N°" = 1:length(datos_ordenados),
    "Valor" = datos_ordenados
  )
  
  # Crear y mostrar la tabla con xtable
  tabla_xtable <- xtable(datos_tabla,
                        caption = "Datos generados ordenados",
                        label = "tabla:datos",
                        align = c("l", "c", "c"))
  
  print(tabla_xtable, 
        type = "html",
        include.rownames = FALSE,
        html.table.attributes = 'border="1" cellpadding="5" cellspacing="0"')
  
  # Calcular estadísticas
  minimo <- min(datos)
  q1 <- as.numeric(quantile(datos, 0.25, type = 7))
  mediana <- median(datos)
  q3 <- as.numeric(quantile(datos, 0.75, type = 7))
  maximo <- max(datos)
  
  # Crear y mostrar tabla de estadísticas
  stats_tabla <- data.frame(
    "Estadística" = c("Mínimo", "Q1", "Mediana (Q2)", "Q3", "Máximo"),
    "Valor" = c(minimo, q1, mediana, q3, maximo)
  )
  
  stats_xtable <- xtable(stats_tabla,
                        caption = "Estadísticas calculadas",
                        label = "tabla:stats",
                        align = c("l", "l", "c"))
  
  print(stats_xtable,
        type = "html",
        include.rownames = FALSE,
        html.table.attributes = 'border="1" cellpadding="5" cellspacing="0"')
  
  # Configurar gráfico
  par(mar = c(2, 4, 3, 2))
  plot(NA, xlim = c(0.5, 1.5), ylim = c(minimo-3, maximo+3), 
       xaxt = "n", yaxt = "n", xlab = "", ylab = "",
       bty = "n", main = "Diagrama de Caja con Datos Aleatorios")
  
  # Dibujar el rectángulo principal (cuerpo del boxplot)
  rect(0.75, q1, 1.25, q3, border = "black", col = "white")
  
  # Dibujar la línea de la mediana
  segments(0.75, mediana, 1.25, mediana, lwd = 1)
  
  # Dibujar los bigotes (whiskers)
  segments(1, q1, 1, minimo, lwd = 1)  # Bigote inferior
  segments(1, q3, 1, maximo, lwd = 1)  # Bigote superior
  
  # Dibujar las líneas horizontales en los extremos de los bigotes
  segments(0.85, minimo, 1.15, minimo, lwd = 1)  # Línea horizontal en mínimo
  segments(0.85, maximo, 1.15, maximo, lwd = 1)  # Línea horizontal en máximo
  
  # Agregar el eje Y con valores relevantes
  secuencia_y <- seq(floor(minimo - 1), ceiling(maximo + 1), by = 1)
  axis(2, at = secuencia_y, las = 1, cex.axis = 0.7)
  
  # Agregar etiquetas para los cuartiles, mínimo y máximo
  text(1.35, q1, "Q1", cex = 0.8)
  text(1.35, mediana, "Q2", cex = 0.8)
  text(1.35, q3, "Q3", cex = 0.8)
  text(1.35, minimo, "Mín.", cex = 0.8)
  text(1.35, maximo, "Máx.", cex = 0.8)
  
  # Dibujar el borde del gráfico
  box()
  
  # Devolver los datos generados para su posible uso posterior
  invisible(datos)
}

# Ejemplo de uso:
#set.seed(123)  # Para reproducibilidad
datos1 <- crear_boxplot_aleatorio()




