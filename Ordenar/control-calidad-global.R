# =============================================================================
# SISTEMA DE CONTROL DE CALIDAD GLOBAL PARA ARCHIVOS .RMD
# =============================================================================
# Versión: 1.0
# Propósito: Revisión automatizada integral de ejercicios r-exams
# Incluye: Análisis visual, semántico, sintáctico, gramatical y de estilo
# =============================================================================

library(exams)
library(knitr)
library(rmarkdown)
library(testthat)
library(magick)  # Para análisis de imágenes
library(png)     # Para lectura de archivos PNG
library(ggplot2) # Para análisis y corrección de gráficos

# =============================================================================
# FUNCIÓN PRINCIPAL DE CONTROL DE CALIDAD
# =============================================================================

control_calidad_integral <- function(archivo_rmd, generar_reporte = TRUE) {
  
  cat("🔍 INICIANDO CONTROL DE CALIDAD INTEGRAL\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("📁 Archivo:", archivo_rmd, "\n")
  cat("📅 Fecha:", Sys.time(), "\n\n")
  
  # Crear directorio para pruebas
  dir_pruebas <- paste0("QC-", gsub("\\.Rmd$", "", basename(archivo_rmd)))
  dir.create(dir_pruebas, showWarnings = FALSE)
  
  # Inicializar reporte de calidad
  reporte_calidad <- list(
    archivo = archivo_rmd,
    fecha = Sys.time(),
    errores = list(),
    advertencias = list(),
    sugerencias = list(),
    puntuacion_total = 0,
    estado = "PENDIENTE"
  )
  
  # ==========================================================================
  # FASE 1: ANÁLISIS SINTÁCTICO Y ESTRUCTURAL
  # ==========================================================================
  
  cat("📋 FASE 1: Análisis Sintáctico y Estructural\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")
  
  # Verificar existencia del archivo
  if (!file.exists(archivo_rmd)) {
    reporte_calidad$errores$archivo <- "Archivo no encontrado"
    return(reporte_calidad)
  }
  
  # Leer contenido del archivo
  contenido <- readLines(archivo_rmd, warn = FALSE)
  reporte_calidad$lineas_total <- length(contenido)
  
  # Verificar estructura YAML
  yaml_inicio <- which(grepl("^---$", contenido))[1]
  yaml_fin <- which(grepl("^---$", contenido))[2]
  
  if (is.na(yaml_inicio) || is.na(yaml_fin)) {
    reporte_calidad$errores$yaml <- "Estructura YAML incompleta o faltante"
  } else {
    cat("✅ Estructura YAML válida\n")
  }
  
  # Verificar chunks de R
  chunks_r <- grep("```\\{r", contenido)
  chunks_fin <- grep("```$", contenido)
  
  if (length(chunks_r) != length(chunks_fin)) {
    reporte_calidad$errores$chunks <- "Chunks de R no balanceados"
  } else {
    cat("✅ Chunks de R balanceados (", length(chunks_r), "chunks)\n")
  }
  
  # Verificar secciones obligatorias
  secciones_obligatorias <- c("Question", "Answerlist", "Solution", "Meta-information")
  secciones_encontradas <- sapply(secciones_obligatorias, function(s) {
    any(grepl(paste0("^", s, "$"), contenido))
  })
  
  if (!all(secciones_encontradas)) {
    faltantes <- names(secciones_encontradas)[!secciones_encontradas]
    reporte_calidad$errores$secciones <- paste("Secciones faltantes:", paste(faltantes, collapse = ", "))
  } else {
    cat("✅ Todas las secciones obligatorias presentes\n")
  }
  
  # ==========================================================================
  # FASE 2: GENERACIÓN Y ANÁLISIS VISUAL AVANZADO
  # ==========================================================================

  cat("\n🎨 FASE 2: Generación y Análisis Visual Avanzado\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")

  # Generar versión HTML para análisis visual
  tryCatch({
    exams2html(archivo_rmd,
               n = 3,  # Generar 3 versiones para verificar aleatorización
               name = "control-calidad",
               dir = dir_pruebas,
               template = "plain.html")

    cat("✅ Versiones HTML generadas exitosamente\n")

    # Analizar archivos HTML generados
    archivos_html <- list.files(dir_pruebas, pattern = "\\.html$", full.names = TRUE)

    for (i in seq_along(archivos_html)) {
      cat("🔍 Analizando versión", i, "...\n")

      # Leer contenido HTML
      html_content <- readLines(archivos_html[i], warn = FALSE)
      html_text <- paste(html_content, collapse = " ")

      # Verificar elementos visuales críticos
      verificaciones_visuales <- list(
        imagenes = grepl("<img|!\\[", html_text),
        tablas = grepl("<table|\\|.*\\|", html_text),
        formulas = grepl("\\$.*\\$|\\\\\\(.*\\\\\\)", html_text),
        listas = grepl("<li>|^\\s*[-\\*]", html_text)
      )

      elementos_faltantes <- names(verificaciones_visuales)[!unlist(verificaciones_visuales)]
      if (length(elementos_faltantes) > 0) {
        reporte_calidad$advertencias[[paste0("visual_v", i)]] <-
          paste("Posibles elementos visuales faltantes:", paste(elementos_faltantes, collapse = ", "))
      }

      # ANÁLISIS AVANZADO DE CONTAMINACIÓN VISUAL EN GRÁFICOS
      problemas_visuales <- analizar_contaminacion_visual(dir_pruebas, i)
      if (length(problemas_visuales) > 0) {
        reporte_calidad$advertencias[[paste0("contaminacion_v", i)]] <- problemas_visuales
      }
    }

    # ANÁLISIS ESPECÍFICO DE GRÁFICOS ESTADÍSTICOS
    problemas_estadisticos <- analizar_calidad_graficos_estadisticos(dir_pruebas)
    if (length(problemas_estadisticos) > 0) {
      reporte_calidad$advertencias$graficos_estadisticos <- problemas_estadisticos
    }

    # ANÁLISIS DE SOLAPAMIENTOS DE ETIQUETAS
    problemas_solapamiento <- detectar_solapamientos_etiquetas(dir_pruebas)
    if (length(problemas_solapamiento) > 0) {
      reporte_calidad$advertencias$solapamientos_etiquetas <- problemas_solapamiento

      # Generar código de corrección automática
      codigo_correccion <- generar_codigo_correccion_solapamientos(archivo_rmd, problemas_solapamiento)
      reporte_calidad$codigo_correccion <- codigo_correccion
    }

  }, error = function(e) {
    reporte_calidad$errores$generacion_html <- paste("Error al generar HTML:", e$message)
    cat("❌ Error al generar HTML:", e$message, "\n")
  })
  
  # ==========================================================================
  # FASE 3: ANÁLISIS SEMÁNTICO Y MATEMÁTICO
  # ==========================================================================
  
  cat("\n🧮 FASE 3: Análisis Semántico y Matemático\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")
  
  # Buscar variables matemáticas y verificar coherencia
  variables_matematicas <- grep("\\$.*\\$|\\\\\\(.*\\\\\\)|\\\\begin\\{", contenido, value = TRUE)
  
  if (length(variables_matematicas) > 0) {
    cat("✅ Contenido matemático detectado (", length(variables_matematicas), "expresiones)\n")
    
    # Verificar balance de delimitadores matemáticos
    dolares_simples <- sum(gregexpr("\\$", paste(contenido, collapse = ""))[[1]] > 0)
    if (dolares_simples %% 2 != 0) {
      reporte_calidad$errores$matematicas <- "Delimitadores matemáticos ($) no balanceados"
    }
  }
  
  # Verificar coherencia de variables R
  variables_r <- grep("`r\\s+\\w+", contenido, value = TRUE)
  if (length(variables_r) > 0) {
    cat("✅ Variables R dinámicas detectadas (", length(variables_r), "referencias)\n")
  }
  
  return(reporte_calidad)
}

# =============================================================================
# FUNCIÓN DE ANÁLISIS GRAMATICAL Y DE ESTILO
# =============================================================================

analisis_gramatical <- function(archivo_rmd) {
  
  cat("\n📝 FASE 4: Análisis Gramatical y de Estilo\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")
  
  contenido <- readLines(archivo_rmd, warn = FALSE)
  
  # Extraer texto de las secciones principales
  texto_pregunta <- extraer_seccion(contenido, "Question")
  texto_solucion <- extraer_seccion(contenido, "Solution")
  
  problemas_gramaticales <- list()
  
  # Verificaciones gramaticales básicas
  if (nchar(texto_pregunta) > 0) {
    
    # Verificar puntuación
    if (!grepl("\\?$", texto_pregunta)) {
      problemas_gramaticales$pregunta_sin_interrogacion <- 
        "La pregunta no termina con signo de interrogación"
    }
    
    # Verificar mayúsculas al inicio
    if (!grepl("^[A-ZÁÉÍÓÚÑ]", texto_pregunta)) {
      problemas_gramaticales$mayuscula_inicial <- 
        "La pregunta no inicia con mayúscula"
    }
    
    # Verificar espacios dobles
    if (grepl("  +", texto_pregunta)) {
      problemas_gramaticales$espacios_dobles <- 
        "Espacios dobles detectados en la pregunta"
    }
    
    cat("✅ Análisis gramatical de pregunta completado\n")
  }
  
  if (nchar(texto_solucion) > 0) {
    
    # Verificar estructura de solución
    if (!grepl("paso|Paso|PASO", texto_solucion)) {
      problemas_gramaticales$solucion_sin_pasos <- 
        "La solución no parece tener estructura de pasos"
    }
    
    cat("✅ Análisis gramatical de solución completado\n")
  }
  
  return(problemas_gramaticales)
}

# =============================================================================
# FUNCIÓN DE ANÁLISIS DE CONTAMINACIÓN VISUAL EN GRÁFICOS
# =============================================================================

analizar_contaminacion_visual <- function(dir_pruebas, version) {

  cat("🖼️ Analizando contaminación visual en gráficos...\n")

  problemas_detectados <- c()

  # Buscar archivos de imagen en el directorio
  archivos_imagen <- list.files(dir_pruebas,
                               pattern = "\\.(png|jpg|jpeg|pdf)$",
                               full.names = TRUE,
                               recursive = TRUE)

  if (length(archivos_imagen) == 0) {
    return("No se encontraron archivos de imagen para analizar")
  }

  # Analizar cada imagen
  for (archivo_img in archivos_imagen) {

    tryCatch({

      # Verificar si el archivo existe y es accesible
      if (!file.exists(archivo_img) || file.size(archivo_img) == 0) {
        problemas_detectados <- c(problemas_detectados,
                                paste("Archivo de imagen corrupto o vacío:", basename(archivo_img)))
        next
      }

      # Análisis específico para archivos PNG
      if (grepl("\\.png$", archivo_img, ignore.case = TRUE)) {

        # Verificar si magick está disponible
        if (requireNamespace("magick", quietly = TRUE)) {

          # Leer imagen con magick
          img <- magick::image_read(archivo_img)
          info <- magick::image_info(img)

          # VERIFICACIONES DE CALIDAD VISUAL

          # 1. Verificar resolución mínima
          if (info$width < 400 || info$height < 300) {
            problemas_detectados <- c(problemas_detectados,
              paste("Resolución muy baja en", basename(archivo_img),
                   "- Tamaño:", info$width, "x", info$height))
          }

          # 2. Verificar relación de aspecto extrema
          aspecto <- info$width / info$height
          if (aspecto > 4 || aspecto < 0.25) {
            problemas_detectados <- c(problemas_detectados,
              paste("Relación de aspecto problemática en", basename(archivo_img),
                   "- Ratio:", round(aspecto, 2)))
          }

          # 3. Análisis de histograma para detectar problemas de contraste
          hist_data <- magick::image_histogram(img)

          # Verificar si la imagen es demasiado oscura o clara
          if (nrow(hist_data) > 0) {
            # Calcular brillo promedio
            brillo_promedio <- mean(as.numeric(gsub("#", "", hist_data$color)), na.rm = TRUE)

            if (brillo_promedio < 50) {
              problemas_detectados <- c(problemas_detectados,
                paste("Imagen muy oscura:", basename(archivo_img)))
            } else if (brillo_promedio > 200) {
              problemas_detectados <- c(problemas_detectados,
                paste("Imagen muy clara/sobreexpuesta:", basename(archivo_img)))
            }
          }

          # 4. Verificar si la imagen tiene transparencia problemática
          if (info$matte && info$colorspace == "sRGB") {
            # Verificar canales alpha
            alpha_stats <- magick::image_channel(img, "alpha")
            alpha_info <- magick::image_info(alpha_stats)

            if (alpha_info$filesize < info$filesize * 0.1) {
              problemas_detectados <- c(problemas_detectados,
                paste("Posible problema de transparencia en", basename(archivo_img)))
            }
          }

        } else {
          # Fallback sin magick - análisis básico con png package
          if (requireNamespace("png", quietly = TRUE)) {

            tryCatch({
              img_data <- png::readPNG(archivo_img)

              # Verificar dimensiones
              dims <- dim(img_data)
              if (length(dims) >= 2) {
                if (dims[1] < 300 || dims[2] < 400) {
                  problemas_detectados <- c(problemas_detectados,
                    paste("Resolución baja detectada en", basename(archivo_img)))
                }
              }

            }, error = function(e) {
              problemas_detectados <- c(problemas_detectados,
                paste("Error al leer PNG:", basename(archivo_img)))
            })
          }
        }
      }

      # 5. Verificar tamaño de archivo (muy grande o muy pequeño)
      tamaño_kb <- file.size(archivo_img) / 1024

      if (tamaño_kb > 2048) {  # Mayor a 2MB
        problemas_detectados <- c(problemas_detectados,
          paste("Archivo muy grande:", basename(archivo_img),
               "- Tamaño:", round(tamaño_kb), "KB"))
      } else if (tamaño_kb < 5) {  # Menor a 5KB
        problemas_detectados <- c(problemas_detectados,
          paste("Archivo sospechosamente pequeño:", basename(archivo_img),
               "- Tamaño:", round(tamaño_kb), "KB"))
      }

    }, error = function(e) {
      problemas_detectados <- c(problemas_detectados,
        paste("Error al analizar", basename(archivo_img), ":", e$message))
    })
  }

  # ANÁLISIS DE PATRONES DE NOMBRES DE ARCHIVOS
  nombres_archivos <- basename(archivos_imagen)

  # Verificar nombres duplicados o confusos
  if (length(unique(nombres_archivos)) < length(nombres_archivos)) {
    problemas_detectados <- c(problemas_detectados,
      "Nombres de archivos duplicados detectados")
  }

  # Verificar nombres descriptivos
  nombres_genericos <- sum(grepl("^(plot|graph|image|fig)\\d*\\.(png|jpg)$",
                                nombres_archivos, ignore.case = TRUE))
  if (nombres_genericos > 0) {
    problemas_detectados <- c(problemas_detectados,
      paste("Archivos con nombres poco descriptivos:", nombres_genericos))
  }

  if (length(problemas_detectados) == 0) {
    cat("✅ No se detectaron problemas de contaminación visual\n")
    return(NULL)
  } else {
    cat("⚠️ Detectados", length(problemas_detectados), "problemas visuales\n")
    return(problemas_detectados)
  }
}

# =============================================================================
# FUNCIÓN DE DETECCIÓN Y CORRECCIÓN DE SOLAPAMIENTOS DE ETIQUETAS
# =============================================================================

detectar_solapamientos_etiquetas <- function(dir_pruebas) {

  cat("🏷️ Analizando solapamientos de etiquetas en gráficos...\n")

  problemas_solapamiento <- c()

  # Buscar archivos de imagen
  archivos_imagen <- list.files(dir_pruebas,
                               pattern = "\\.(png|jpg|jpeg)$",
                               full.names = TRUE,
                               recursive = TRUE)

  if (length(archivos_imagen) == 0) {
    return("No se encontraron imágenes para analizar solapamientos")
  }

  for (archivo_img in archivos_imagen) {

    tryCatch({

      if (requireNamespace("magick", quietly = TRUE)) {

        # Leer imagen
        img <- magick::image_read(archivo_img)
        info <- magick::image_info(img)

        # ANÁLISIS DE DENSIDAD DE TEXTO/ETIQUETAS

        # 1. Detectar áreas con alta densidad de píxeles no blancos
        img_array <- as.integer(img[[1]])

        if (length(dim(img_array)) >= 3) {

          # Convertir a escala de grises para análisis
          gray_img <- (img_array[,,1] + img_array[,,2] + img_array[,,3]) / 3

          # Detectar píxeles de texto (no blancos, no del color de fondo)
          text_pixels <- which(gray_img < 240 & gray_img > 50)  # Rango típico de texto

          if (length(text_pixels) > 0) {

            # Convertir índices lineales a coordenadas
            coords <- arrayInd(text_pixels, dim(gray_img))

            # ANÁLISIS DE CLUSTERING DE TEXTO

            # Dividir imagen en regiones para detectar concentraciones
            height <- dim(gray_img)[1]
            width <- dim(gray_img)[2]

            # Crear grid de análisis (dividir en celdas de 50x50 píxeles)
            cell_size <- 50
            rows_grid <- ceiling(height / cell_size)
            cols_grid <- ceiling(width / cell_size)

            # Contar densidad de texto por celda
            text_density <- matrix(0, nrow = rows_grid, ncol = cols_grid)

            for (i in 1:nrow(coords)) {
              row_cell <- min(ceiling(coords[i, 1] / cell_size), rows_grid)
              col_cell <- min(ceiling(coords[i, 2] / cell_size), cols_grid)
              text_density[row_cell, col_cell] <- text_density[row_cell, col_cell] + 1
            }

            # DETECTAR SOLAPAMIENTOS

            # Umbral de densidad alta (ajustable según tipo de gráfico)
            umbral_solapamiento <- quantile(text_density[text_density > 0], 0.8, na.rm = TRUE)

            if (length(umbral_solapamiento) > 0 && !is.na(umbral_solapamiento)) {
              celdas_problematicas <- which(text_density > umbral_solapamiento, arr.ind = TRUE)

              if (nrow(celdas_problematicas) > 0) {
                problemas_solapamiento <- c(problemas_solapamiento,
                  paste("Posible solapamiento de etiquetas detectado en",
                       basename(archivo_img), "- Zonas problemáticas:", nrow(celdas_problematicas)))
              }
            }

            # ANÁLISIS ESPECÍFICO PARA DIAGRAMAS DE CAJA

            # Detectar si es un diagrama de caja por características típicas
            if (grepl("diagrama|box|caja", basename(archivo_img), ignore.case = TRUE)) {

              # En diagramas de caja, las etiquetas suelen estar en el lado derecho
              # Analizar densidad en la región derecha (último tercio)
              region_derecha <- text_density[, (2*cols_grid/3):cols_grid]

              if (ncol(region_derecha) > 0) {
                densidad_derecha <- sum(region_derecha)
                densidad_total <- sum(text_density)

                # Si más del 60% del texto está en el lado derecho, posible solapamiento
                if (densidad_total > 0 && (densidad_derecha / densidad_total) > 0.6) {

                  # Verificar concentración vertical en región derecha
                  filas_con_texto <- which(rowSums(region_derecha) > 0)

                  if (length(filas_con_texto) > 0) {
                    # Calcular dispersión vertical
                    rango_vertical <- max(filas_con_texto) - min(filas_con_texto)
                    altura_relativa <- rango_vertical / rows_grid

                    # Si las etiquetas están muy concentradas verticalmente
                    if (altura_relativa < 0.3) {
                      problemas_solapamiento <- c(problemas_solapamiento,
                        paste("CRÍTICO: Etiquetas concentradas verticalmente en",
                             basename(archivo_img), "- Probable solapamiento en diagrama de caja"))
                    }
                  }
                }
              }
            }

            # ANÁLISIS DE PROXIMIDAD DE ELEMENTOS DE TEXTO

            # Detectar líneas horizontales de texto muy próximas
            filas_con_texto <- which(rowSums(text_density) > 0)

            if (length(filas_con_texto) > 1) {
              diferencias <- diff(filas_con_texto)
              lineas_muy_proximas <- sum(diferencias <= 2)  # Menos de 2 celdas de separación

              if (lineas_muy_proximas > 2) {
                problemas_solapamiento <- c(problemas_solapamiento,
                  paste("Múltiples líneas de texto muy próximas en",
                       basename(archivo_img), "- Posible solapamiento"))
              }
            }
          }
        }
      }

    }, error = function(e) {
      problemas_solapamiento <- c(problemas_solapamiento,
        paste("Error al analizar solapamientos en", basename(archivo_img), ":", e$message))
    })
  }

  if (length(problemas_solapamiento) == 0) {
    cat("✅ No se detectaron solapamientos de etiquetas\n")
    return(NULL)
  } else {
    cat("⚠️ Detectados", length(problemas_solapamiento), "problemas de solapamiento\n")
    return(problemas_solapamiento)
  }
}

# =============================================================================
# FUNCIÓN DE CORRECCIÓN AUTOMÁTICA DE SOLAPAMIENTOS
# =============================================================================

generar_codigo_correccion_solapamientos <- function(archivo_rmd, problemas_detectados) {

  cat("🛠️ Generando código de corrección para solapamientos...\n")

  # Leer el archivo .Rmd original
  contenido <- readLines(archivo_rmd, warn = FALSE)

  # Buscar el chunk de generación de gráficos
  chunk_inicio <- grep("```\\{r.*generar.*diagrama", contenido, ignore.case = TRUE)

  if (length(chunk_inicio) == 0) {
    return("No se encontró chunk de generación de gráficos para corregir")
  }

  # Generar código de corrección mejorado
  codigo_corregido <- '
# =============================================================================
# CÓDIGO CORREGIDO PARA EVITAR SOLAPAMIENTOS DE ETIQUETAS
# =============================================================================

```{r generar_diagramas_sin_solapamiento, echo=FALSE, results="asis", fig.width=8, fig.height=6}

# Función para calcular posiciones de etiquetas sin solapamiento
calcular_posiciones_etiquetas <- function(valores, ancho_grafico = 8, altura_grafico = 6) {

  # Ordenar valores para análisis
  vals_ordenados <- sort(valores)
  n_vals <- length(vals_ordenados)

  # Calcular separación mínima necesaria (en unidades del gráfico)
  rango_datos <- max(vals_ordenados) - min(vals_ordenados)
  separacion_minima <- rango_datos * 0.08  # 8% del rango como separación mínima

  # Posiciones iniciales (lado derecho del gráfico)
  posiciones_y <- vals_ordenados
  posiciones_x <- rep(1.3, n_vals)  # Posición X fija a la derecha

  # Ajustar posiciones para evitar solapamientos
  for (i in 2:n_vals) {
    diferencia <- posiciones_y[i] - posiciones_y[i-1]

    if (diferencia < separacion_minima) {
      # Ajustar posición hacia arriba
      posiciones_y[i] <- posiciones_y[i-1] + separacion_minima
    }
  }

  # Verificar que no se salgan del rango del gráfico
  rango_grafico <- max(vals_ordenados) + rango_datos * 0.1 - (min(vals_ordenados) - rango_datos * 0.1)

  if (max(posiciones_y) > max(vals_ordenados) + rango_datos * 0.1) {
    # Redistribuir uniformemente si se salen del rango
    posiciones_y <- seq(min(vals_ordenados) - rango_datos * 0.05,
                       max(vals_ordenados) + rango_datos * 0.05,
                       length.out = n_vals)
  }

  return(data.frame(
    x = posiciones_x,
    y = posiciones_y,
    valores_originales = vals_ordenados
  ))
}

# Generar diagramas con etiquetas corregidas
for (i in seq_along(opciones_mezcladas)) {
  stats <- opciones_mezcladas[[i]]

  # Crear data frame para ggplot
  df_box <- data.frame(
    x = 1,
    ymin = stats$minimo,
    lower = stats$q1,
    middle = stats$mediana,
    upper = stats$q3,
    ymax = stats$maximo
  )

  # Calcular posiciones de etiquetas sin solapamiento
  valores_etiquetas <- c(stats$minimo, stats$q1, stats$mediana, stats$q3, stats$maximo)
  nombres_etiquetas <- c(
    paste("Mín:", stats$minimo),
    paste("Q1:", round(stats$q1, 1)),
    paste("Med:", round(stats$mediana, 1)),
    paste("Q3:", round(stats$q3, 1)),
    paste("Máx:", stats$maximo)
  )

  posiciones <- calcular_posiciones_etiquetas(valores_etiquetas)

  # Crear gráfico mejorado
  p <- ggplot(df_box, aes(x = x)) +
    geom_boxplot(aes(ymin = ymin, lower = lower, middle = middle,
                    upper = upper, ymax = ymax),
                stat = "identity",
                fill = colores_seleccionados[i],
                alpha = 0.7,
                color = "black",
                linewidth = 1.2) +
    labs(title = paste("Opción", LETTERS[i]),
         y = paste(contexto_seleccionado$variable, "(", contexto_seleccionado$unidad, ")")) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title.y = element_text(size = 12, face = "bold"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      plot.margin = margin(20, 60, 20, 20)  # Más margen derecho para etiquetas
    ) +
    xlim(0.5, 2.0) +  # Ampliar límite X para dar espacio a etiquetas
    ylim(min(valores_etiquetas) - 10, max(posiciones$y) + 5)

  # Añadir etiquetas con posiciones calculadas
  for (j in 1:nrow(posiciones)) {
    p <- p + annotate("text",
                     x = posiciones$x[j],
                     y = posiciones$y[j],
                     label = nombres_etiquetas[j],
                     hjust = 0,
                     size = 3.5,
                     fontface = "bold",
                     color = if(j == 3) "darkred" else "black")  # Mediana en rojo

    # Añadir línea conectora sutil
    p <- p + annotate("segment",
                     x = 1.1, xend = posiciones$x[j] - 0.02,
                     y = valores_etiquetas[j], yend = posiciones$y[j],
                     color = "gray60", size = 0.3, alpha = 0.7)
  }

  # Guardar gráfico
  nombre_archivo <- paste0("diagrama_opcion_", letters[i], "_corregido.png")
  ggsave(nombre_archivo, plot = p, width = 8, height = 6, dpi = 200, bg = "white")

  # Mostrar gráfico en el documento
  cat("\\n**", LETTERS[i], ".**\\n\\n")
  cat("![](", nombre_archivo, "){width=80%}\\n\\n")
}
```'

  return(codigo_corregido)
}

# =============================================================================
# FUNCIÓN DE ANÁLISIS ESPECÍFICO DE GRÁFICOS ESTADÍSTICOS
# =============================================================================

analizar_calidad_graficos_estadisticos <- function(dir_pruebas) {

  cat("📊 Analizando calidad específica de gráficos estadísticos...\n")

  problemas_estadisticos <- c()

  # Buscar archivos HTML para extraer información de gráficos
  archivos_html <- list.files(dir_pruebas, pattern = "\\.html$", full.names = TRUE)

  for (html_file in archivos_html) {
    html_content <- readLines(html_file, warn = FALSE)
    html_text <- paste(html_content, collapse = " ")

    # VERIFICACIONES ESPECÍFICAS PARA GRÁFICOS ESTADÍSTICOS

    # 1. Verificar si hay referencias a gráficos sin imágenes
    referencias_graficos <- gregexpr("(gráfico|gráfica|diagrama|chart|plot)",
                                   html_text, ignore.case = TRUE)[[1]]
    imagenes_presentes <- gregexpr("(<img|!\\[)", html_text)[[1]]

    if (length(referencias_graficos) > 0 && referencias_graficos[1] != -1 &&
        (length(imagenes_presentes) == 0 || imagenes_presentes[1] == -1)) {
      problemas_estadisticos <- c(problemas_estadisticos,
        "Referencias a gráficos sin imágenes correspondientes")
    }

    # 2. NUEVO: Verificar opciones sin gráficos correspondientes
    opciones_texto <- gregexpr("Opción [A-D]", html_text)[[1]]
    if (length(opciones_texto) > 0 && opciones_texto[1] != -1) {
      num_opciones_texto <- length(opciones_texto)

      if (length(imagenes_presentes) == 0 || imagenes_presentes[1] == -1) {
        problemas_estadisticos <- c(problemas_estadisticos,
          paste("CRÍTICO: Se mencionan", num_opciones_texto,
               "opciones pero NO hay imágenes en el HTML"))
      } else {
        num_imagenes_html <- length(imagenes_presentes)
        if (num_imagenes_html < num_opciones_texto) {
          problemas_estadisticos <- c(problemas_estadisticos,
            paste("INCONSISTENCIA: ", num_opciones_texto, "opciones mencionadas pero solo",
                 num_imagenes_html, "imágenes en HTML"))
        }
      }
    }

    # 3. NUEVO: Verificar tablas mencionadas pero no presentes
    referencias_tabla <- gregexpr("(tabla|table)", html_text, ignore.case = TRUE)[[1]]
    tablas_html <- gregexpr("<table", html_text, ignore.case = TRUE)[[1]]

    if (length(referencias_tabla) > 0 && referencias_tabla[1] != -1 &&
        (length(tablas_html) == 0 || tablas_html[1] == -1)) {
      problemas_estadisticos <- c(problemas_estadisticos,
        "CRÍTICO: Se menciona tabla pero NO está presente en el HTML")
    }

    # 2. Verificar menciones de elementos específicos sin visualización
    elementos_visuales <- c("diagrama de caja", "histograma", "gráfico circular",
                           "diagrama de barras", "scatter plot", "línea de tendencia")

    for (elemento in elementos_visuales) {
      if (grepl(elemento, html_text, ignore.case = TRUE) &&
          (length(imagenes_presentes) == 0 || imagenes_presentes[1] == -1)) {
        problemas_estadisticos <- c(problemas_estadisticos,
          paste("Mención de", elemento, "sin visualización"))
      }
    }

    # 3. Verificar consistencia en opciones de respuesta visual
    opciones_pattern <- "Opción [A-D]"
    opciones_encontradas <- gregexpr(opciones_pattern, html_text)[[1]]

    if (length(opciones_encontradas) > 0 && opciones_encontradas[1] != -1) {
      num_opciones <- length(opciones_encontradas)
      num_imagenes <- length(imagenes_presentes)

      if (imagenes_presentes[1] != -1 && num_imagenes < num_opciones) {
        problemas_estadisticos <- c(problemas_estadisticos,
          paste("Inconsistencia: ", num_opciones, "opciones pero solo",
               num_imagenes, "imágenes"))
      }
    }
  }

  return(problemas_estadisticos)
}

# =============================================================================
# FUNCIONES AUXILIARES
# =============================================================================

extraer_seccion <- function(contenido, seccion) {
  inicio <- which(grepl(paste0("^", seccion, "$"), contenido))
  if (length(inicio) == 0) return("")
  
  fin <- which(grepl("^[A-Z][a-z]+$", contenido[-(1:inicio[1])]))
  if (length(fin) == 0) {
    fin <- length(contenido)
  } else {
    fin <- inicio[1] + fin[1] - 1
  }
  
  texto <- paste(contenido[(inicio[1] + 1):fin], collapse = " ")
  return(gsub("\\s+", " ", texto))
}

# =============================================================================
# FUNCIÓN DE REPORTE FINAL
# =============================================================================

generar_reporte_final <- function(reporte_calidad, problemas_gramaticales) {
  
  cat("\n📊 REPORTE FINAL DE CALIDAD\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # Calcular puntuación con criterios visuales avanzados
  puntos_base <- 100

  # Penalizaciones por tipo de problema
  puntos_errores_criticos <- length(reporte_calidad$errores) * 25
  puntos_advertencias_generales <- length(reporte_calidad$advertencias) * 8
  puntos_problemas_gramaticales <- length(problemas_gramaticales) * 5

  # Penalizaciones específicas por contaminación visual
  problemas_contaminacion <- sum(grepl("contaminacion_", names(reporte_calidad$advertencias)))
  puntos_contaminacion_visual <- problemas_contaminacion * 15

  # Penalizaciones por problemas de gráficos estadísticos
  if ("graficos_estadisticos" %in% names(reporte_calidad$advertencias)) {
    puntos_graficos_estadisticos <- length(reporte_calidad$advertencias$graficos_estadisticos) * 12
  } else {
    puntos_graficos_estadisticos <- 0
  }

  # Penalizaciones por solapamientos de etiquetas
  if ("solapamientos_etiquetas" %in% names(reporte_calidad$advertencias)) {
    puntos_solapamientos <- length(reporte_calidad$advertencias$solapamientos_etiquetas) * 18
  } else {
    puntos_solapamientos <- 0
  }

  puntos_perdidos <- puntos_errores_criticos +
                   puntos_advertencias_generales +
                   puntos_problemas_gramaticales +
                   puntos_contaminacion_visual +
                   puntos_graficos_estadisticos +
                   puntos_solapamientos

  puntuacion_final <- max(0, puntos_base - puntos_perdidos)
  
  # Determinar estado
  if (puntuacion_final >= 90) {
    estado <- "EXCELENTE ✅"
  } else if (puntuacion_final >= 75) {
    estado <- "BUENO ⚠️"
  } else if (puntuacion_final >= 60) {
    estado <- "REGULAR ⚠️"
  } else {
    estado <- "REQUIERE MEJORAS ❌"
  }
  
  cat("📁 Archivo:", reporte_calidad$archivo, "\n")
  cat("📊 Puntuación:", puntuacion_final, "/100\n")
  cat("🎯 Estado:", estado, "\n")
  cat("📏 Líneas totales:", reporte_calidad$lineas_total, "\n\n")
  
  # Mostrar errores
  if (length(reporte_calidad$errores) > 0) {
    cat("❌ ERRORES CRÍTICOS:\n")
    for (i in seq_along(reporte_calidad$errores)) {
      cat("  •", reporte_calidad$errores[[i]], "\n")
    }
    cat("\n")
  }
  
  # Mostrar advertencias categorizadas
  if (length(reporte_calidad$advertencias) > 0) {

    # Separar advertencias por categoría
    advertencias_generales <- reporte_calidad$advertencias[!grepl("contaminacion_|graficos_estadisticos", names(reporte_calidad$advertencias))]
    advertencias_contaminacion <- reporte_calidad$advertencias[grepl("contaminacion_", names(reporte_calidad$advertencias))]
    advertencias_graficos <- reporte_calidad$advertencias[names(reporte_calidad$advertencias) == "graficos_estadisticos"]

    # Mostrar advertencias generales
    if (length(advertencias_generales) > 0) {
      cat("⚠️ ADVERTENCIAS GENERALES:\n")
      for (i in seq_along(advertencias_generales)) {
        if (is.character(advertencias_generales[[i]])) {
          cat("  •", advertencias_generales[[i]], "\n")
        } else {
          cat("  •", paste(advertencias_generales[[i]], collapse = ", "), "\n")
        }
      }
      cat("\n")
    }

    # Mostrar problemas de contaminación visual
    if (length(advertencias_contaminacion) > 0) {
      cat("🖼️ PROBLEMAS DE CONTAMINACIÓN VISUAL:\n")
      for (i in seq_along(advertencias_contaminacion)) {
        if (is.character(advertencias_contaminacion[[i]])) {
          for (problema in advertencias_contaminacion[[i]]) {
            cat("  •", problema, "\n")
          }
        }
      }
      cat("\n")
    }

    # Mostrar problemas específicos de gráficos estadísticos
    if (length(advertencias_graficos) > 0) {
      cat("📊 PROBLEMAS EN GRÁFICOS ESTADÍSTICOS:\n")
      for (problema in advertencias_graficos[[1]]) {
        cat("  •", problema, "\n")
      }
      cat("\n")
    }

    # Mostrar problemas de solapamientos de etiquetas
    advertencias_solapamientos <- reporte_calidad$advertencias[names(reporte_calidad$advertencias) == "solapamientos_etiquetas"]
    if (length(advertencias_solapamientos) > 0) {
      cat("🏷️ PROBLEMAS DE SOLAPAMIENTO DE ETIQUETAS:\n")
      for (problema in advertencias_solapamientos[[1]]) {
        cat("  •", problema, "\n")
      }
      cat("\n")
    }
  }
  
  # Mostrar problemas gramaticales
  if (length(problemas_gramaticales) > 0) {
    cat("📝 PROBLEMAS GRAMATICALES/ESTILO:\n")
    for (i in seq_along(problemas_gramaticales)) {
      cat("  •", problemas_gramaticales[[i]], "\n")
    }
    cat("\n")
  }
  
  if (length(reporte_calidad$errores) == 0 &&
      length(reporte_calidad$advertencias) == 0 &&
      length(problemas_gramaticales) == 0) {
    cat("🎉 ¡PERFECTO! No se encontraron problemas.\n\n")
  }

  # Mostrar código de corrección automática si está disponible
  if (!is.null(reporte_calidad$codigo_correccion)) {
    cat("🛠️ CÓDIGO DE CORRECCIÓN AUTOMÁTICA DISPONIBLE:\n")
    cat("Se ha generado código para corregir automáticamente los solapamientos detectados.\n")
    cat("El código corregido está disponible en el objeto de retorno.\n\n")
  }
  
  return(list(
    puntuacion = puntuacion_final,
    estado = estado,
    errores = reporte_calidad$errores,
    advertencias = reporte_calidad$advertencias,
    gramaticales = problemas_gramaticales
  ))
}

# =============================================================================
# FUNCIONES DE CORRECCIÓN AUTOMÁTICA
# =============================================================================

aplicar_correcciones_automaticas <- function(archivo_rmd, reporte_errores, problemas_gramaticales) {

  cat("\n🛠️ APLICANDO CORRECCIONES AUTOMÁTICAS\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")

  # Leer archivo original
  contenido <- readLines(archivo_rmd, warn = FALSE)
  contenido_original <- contenido
  correcciones_aplicadas <- c()

  # 1. CORRECCIONES GRAMATICALES
  if (length(problemas_gramaticales) > 0) {
    cat("📝 Aplicando correcciones gramaticales...\n")

    # Encontrar la sección Question
    linea_question <- which(grepl("^Question$", contenido))
    if (length(linea_question) > 0) {

      # Buscar la pregunta (primera línea no vacía después de Question)
      for (i in (linea_question + 1):length(contenido)) {
        if (nchar(trimws(contenido[i])) > 0 && !grepl("^=+$", contenido[i])) {

          # Corregir mayúscula inicial
          if ("mayuscula_inicial" %in% names(problemas_gramaticales)) {
            primera_letra <- substr(contenido[i], 1, 1)
            if (primera_letra %in% letters) {
              contenido[i] <- paste0(toupper(primera_letra), substr(contenido[i], 2, nchar(contenido[i])))
              correcciones_aplicadas <- c(correcciones_aplicadas, "Mayúscula inicial añadida")
            }
          }

          # Corregir signo de interrogación
          if ("pregunta_sin_interrogacion" %in% names(problemas_gramaticales)) {
            if (!grepl("\\?$", trimws(contenido[i]))) {
              contenido[i] <- paste0(trimws(contenido[i]), "?")
              correcciones_aplicadas <- c(correcciones_aplicadas, "Signo de interrogación añadido")
            }
          }

          break
        }
      }
    }
  }

  # 2. CORRECCIONES DE DELIMITADORES MATEMÁTICOS
  if ("matematicas" %in% names(reporte_errores)) {
    cat("🧮 Corrigiendo delimitadores matemáticos...\n")

    # Contar delimitadores $ en todo el documento
    texto_completo <- paste(contenido, collapse = "\n")
    dolares <- gregexpr("\\$", texto_completo)[[1]]

    if (length(dolares) > 0 && dolares[1] != -1) {
      num_dolares <- length(dolares)

      # Si es impar, añadir un $ al final del documento
      if (num_dolares %% 2 != 0) {
        # Buscar la última línea con contenido matemático
        lineas_con_math <- grep("\\$", contenido)
        if (length(lineas_con_math) > 0) {
          ultima_linea_math <- max(lineas_con_math)
          # Añadir $ al final de esa línea si no termina con $
          if (!grepl("\\$$", contenido[ultima_linea_math])) {
            contenido[ultima_linea_math] <- paste0(contenido[ultima_linea_math], "$")
            correcciones_aplicadas <- c(correcciones_aplicadas, "Delimitador matemático balanceado")
          }
        }
      }
    }
  }

  return(list(
    contenido_corregido = contenido,
    correcciones_aplicadas = correcciones_aplicadas,
    contenido_original = contenido_original
  ))
}

corregir_solapamientos_graficos <- function(archivo_rmd, reporte_errores) {

  cat("🎨 Corrigiendo solapamientos de etiquetas en gráficos...\n")

  # Leer archivo original
  contenido <- readLines(archivo_rmd, warn = FALSE)

  # Buscar el chunk de generación de diagramas
  chunk_inicio <- grep("```\\{r.*generar.*diagrama", contenido, ignore.case = TRUE)

  if (length(chunk_inicio) == 0) {
    # Buscar chunk alternativo
    chunk_inicio <- grep("```\\{r.*generar_diagramas", contenido, ignore.case = TRUE)
  }

  if (length(chunk_inicio) > 0) {
    # Encontrar el final del chunk
    chunk_fin <- grep("```$", contenido[(chunk_inicio + 1):length(contenido)])[1] + chunk_inicio

    if (!is.na(chunk_fin)) {

      # Código mejorado para evitar solapamientos
      codigo_mejorado <- c(
        "```{r generar_diagramas_sin_solapamiento, echo=FALSE, results='asis', fig.width=8, fig.height=6}",
        "# =============================================================================",
        "# GENERAR DIAGRAMAS DE CAJA SIN SOLAPAMIENTOS - VERSIÓN CORREGIDA",
        "# =============================================================================",
        "",
        "# Función para calcular posiciones de etiquetas sin solapamiento",
        "calcular_posiciones_etiquetas <- function(valores, separacion_minima = 8) {",
        "  n_vals <- length(valores)",
        "  posiciones_y <- valores",
        "  ",
        "  # Ordenar por valor para procesamiento",
        "  orden <- order(valores)",
        "  vals_ordenados <- valores[orden]",
        "  pos_ordenadas <- vals_ordenados",
        "  ",
        "  # Ajustar posiciones para evitar solapamientos",
        "  for (i in 2:length(pos_ordenadas)) {",
        "    diferencia <- pos_ordenadas[i] - pos_ordenadas[i-1]",
        "    if (diferencia < separacion_minima) {",
        "      pos_ordenadas[i] <- pos_ordenadas[i-1] + separacion_minima",
        "    }",
        "  }",
        "  ",
        "  # Restaurar orden original",
        "  posiciones_finales <- numeric(n_vals)",
        "  posiciones_finales[orden] <- pos_ordenadas",
        "  ",
        "  return(posiciones_finales)",
        "}",
        "",
        "for (i in seq_along(opciones_mezcladas)) {",
        "  stats <- opciones_mezcladas[[i]]",
        "",
        "  # Crear data frame para ggplot",
        "  df_box <- data.frame(",
        "    x = 1,",
        "    ymin = stats$minimo,",
        "    lower = stats$q1,",
        "    middle = stats$mediana,",
        "    upper = stats$q3,",
        "    ymax = stats$maximo",
        "  )",
        "",
        "  # Calcular posiciones de etiquetas sin solapamiento",
        "  valores_etiquetas <- c(stats$minimo, stats$q1, stats$mediana, stats$q3, stats$maximo)",
        "  nombres_etiquetas <- c(",
        "    paste('Mín:', stats$minimo),",
        "    paste('Q1:', round(stats$q1, 1)),",
        "    paste('Med:', round(stats$mediana, 1)),",
        "    paste('Q3:', round(stats$q3, 1)),",
        "    paste('Máx:', stats$maximo)",
        "  )",
        "  ",
        "  posiciones_y <- calcular_posiciones_etiquetas(valores_etiquetas)",
        "",
        "  # Crear gráfico mejorado",
        "  p <- ggplot(df_box, aes(x = x)) +",
        "    geom_boxplot(aes(ymin = ymin, lower = lower, middle = middle,",
        "                    upper = upper, ymax = ymax),",
        "                stat = 'identity',",
        "                fill = colores_seleccionados[i],",
        "                alpha = 0.7,",
        "                color = 'black',",
        "                linewidth = 1.2) +",
        "    labs(title = paste('Opción', LETTERS[i]),",
        "         y = paste(contexto_seleccionado$variable, '(', contexto_seleccionado$unidad, ')')) +",
        "    theme_minimal() +",
        "    theme(",
        "      axis.text.x = element_blank(),",
        "      axis.ticks.x = element_blank(),",
        "      plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'),",
        "      axis.title.y = element_text(size = 12, face = 'bold'),",
        "      panel.grid.major.x = element_blank(),",
        "      panel.grid.minor.x = element_blank(),",
        "      plot.margin = margin(20, 80, 20, 20)  # Más margen derecho",
        "    ) +",
        "    xlim(0.5, 2.2) +  # Más espacio para etiquetas",
        "    ylim(min(valores_etiquetas) - 10, max(posiciones_y) + 10)",
        "",
        "  # Añadir etiquetas con posiciones calculadas",
        "  for (j in 1:length(valores_etiquetas)) {",
        "    p <- p + annotate('text',",
        "                     x = 1.35,",
        "                     y = posiciones_y[j],",
        "                     label = nombres_etiquetas[j],",
        "                     hjust = 0,",
        "                     size = 3.5,",
        "                     fontface = 'bold',",
        "                     color = if(j == 3) 'darkred' else 'black')",
        "    ",
        "    # Línea conectora sutil",
        "    p <- p + annotate('segment',",
        "                     x = 1.15, xend = 1.32,",
        "                     y = valores_etiquetas[j], yend = posiciones_y[j],",
        "                     color = 'gray60', size = 0.3, alpha = 0.7)",
        "  }",
        "",
        "  # Guardar gráfico",
        "  nombre_archivo <- paste0('diagrama_opcion_', letters[i], '.png')",
        "  ggsave(nombre_archivo, plot = p, width = 8, height = 6, dpi = 200, bg = 'white')",
        "",
        "  # Mostrar gráfico en el documento",
        "  cat('\\n**', LETTERS[i], '.**\\n\\n')",
        "  cat('![](', nombre_archivo, '){width=80%}\\n\\n')",
        "}",
        "```"
      )

      # Reemplazar el chunk original
      contenido <- c(
        contenido[1:(chunk_inicio - 1)],
        codigo_mejorado,
        contenido[(chunk_fin + 1):length(contenido)]
      )

      return(contenido)
    }
  }

  return(contenido)
}

# =============================================================================
# FUNCIÓN PRINCIPAL EJECUTORA MEJORADA
# =============================================================================

ejecutar_control_calidad_completo <- function(archivo_rmd, aplicar_correcciones = TRUE) {

  # Verificar que el archivo existe
  if (!file.exists(archivo_rmd)) {
    stop("❌ El archivo ", archivo_rmd, " no existe")
  }

  cat("🚀 EJECUTANDO CONTROL DE CALIDAD COMPLETO\n")
  cat("📁 Archivo:", archivo_rmd, "\n")
  cat("📅 Inicio:", Sys.time(), "\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n")

  # Ejecutar análisis principal
  reporte_principal <- control_calidad_integral(archivo_rmd)

  # Ejecutar análisis gramatical
  problemas_gramaticales <- analisis_gramatical(archivo_rmd)

  # Generar reporte final
  reporte_final <- generar_reporte_final(reporte_principal, problemas_gramaticales)

  # APLICAR CORRECCIONES AUTOMÁTICAS SI SE DETECTAN ERRORES
  archivo_corregido <- NULL
  if (aplicar_correcciones && (length(reporte_principal$errores) > 0 ||
                              length(problemas_gramaticales) > 0 ||
                              "solapamientos_etiquetas" %in% names(reporte_principal$advertencias))) {

    cat("\n🔧 DETECTADOS ERRORES CORREGIBLES - APLICANDO CORRECCIONES AUTOMÁTICAS\n")
    cat(paste(rep("=", 60), collapse = ""), "\n")

    # Aplicar correcciones básicas
    resultado_correcciones <- aplicar_correcciones_automaticas(archivo_rmd, reporte_principal$errores, problemas_gramaticales)
    contenido_corregido <- resultado_correcciones$contenido_corregido

    # Aplicar correcciones de solapamientos si es necesario
    if ("solapamientos_etiquetas" %in% names(reporte_principal$advertencias)) {
      contenido_corregido <- corregir_solapamientos_graficos(archivo_rmd, reporte_principal$advertencias)
      resultado_correcciones$correcciones_aplicadas <- c(resultado_correcciones$correcciones_aplicadas,
                                                        "Solapamientos de etiquetas corregidos")
    }

    # Guardar archivo corregido
    archivo_corregido <- gsub("\\.Rmd$", "-AUTO-CORREGIDO.Rmd", archivo_rmd)
    writeLines(contenido_corregido, archivo_corregido)

    cat("✅ CORRECCIONES APLICADAS:\n")
    for (correccion in resultado_correcciones$correcciones_aplicadas) {
      cat("  •", correccion, "\n")
    }
    cat("📁 Archivo corregido guardado como:", archivo_corregido, "\n")

    # Ejecutar control de calidad en el archivo corregido para verificar mejoras
    cat("\n🔍 VERIFICANDO CORRECCIONES...\n")
    cat(paste(rep("-", 40), collapse = ""), "\n")

    reporte_verificacion <- control_calidad_integral(archivo_corregido)
    problemas_verificacion <- analisis_gramatical(archivo_corregido)
    reporte_verificacion_final <- generar_reporte_final(reporte_verificacion, problemas_verificacion)

    cat("📊 COMPARACIÓN DE RESULTADOS:\n")
    cat("  • Puntuación original:", reporte_final$puntuacion, "/100\n")
    cat("  • Puntuación corregida:", reporte_verificacion_final$puntuacion, "/100\n")
    mejora <- reporte_verificacion_final$puntuacion - reporte_final$puntuacion
    cat("  • Mejora:", if(mejora > 0) paste("+", mejora) else mejora, "puntos\n\n")

    # Actualizar reporte final con información de correcciones
    reporte_final$archivo_corregido <- archivo_corregido
    reporte_final$correcciones_aplicadas <- resultado_correcciones$correcciones_aplicadas
    reporte_final$puntuacion_corregida <- reporte_verificacion_final$puntuacion
    reporte_final$mejora <- mejora
  }

  # Abrir archivo HTML para revisión visual manual
  dir_pruebas <- paste0("QC-", gsub("\\.Rmd$", "", basename(archivo_rmd)))
  archivos_html <- list.files(dir_pruebas, pattern = "\\.html$", full.names = TRUE)

  if (length(archivos_html) > 0) {
    cat("🌐 Abriendo archivo HTML para revisión visual...\n")
    cat("📂 Ubicación:", archivos_html[1], "\n\n")

    # Instrucciones para revisión manual
    cat("👁️ INSTRUCCIONES PARA REVISIÓN VISUAL MANUAL:\n")
    cat(paste(rep("=", 50), collapse = ""), "\n")
    cat("📋 ELEMENTOS BÁSICOS:\n")
    cat("1. Verificar que las imágenes se muestren correctamente\n")
    cat("2. Comprobar que las tablas estén bien formateadas\n")
    cat("3. Validar que las fórmulas matemáticas se rendericen bien\n")
    cat("4. Verificar que las opciones de respuesta sean claras\n")
    cat("5. Comprobar que la solución sea comprensible\n\n")

    cat("🎨 CALIDAD VISUAL AVANZADA:\n")
    cat("6. Revisar resolución y claridad de gráficos\n")
    cat("7. ⚠️ VERIFICAR SOLAPAMIENTOS: Comprobar que las etiquetas no se superpongan\n")
    cat("8. Validar separación adecuada entre elementos de texto\n")
    cat("9. Comprobar contraste adecuado en colores\n")
    cat("10. Validar que los ejes y etiquetas sean legibles\n")
    cat("11. Verificar que los elementos gráficos no se superpongan\n\n")

    cat("🏷️ ANÁLISIS ESPECÍFICO DE ETIQUETAS:\n")
    cat("12. Verificar que todas las etiquetas sean legibles\n")
    cat("13. Comprobar separación mínima entre etiquetas adyacentes\n")
    cat("14. Validar que las líneas conectoras no se crucen\n")
    cat("15. Verificar alineación consistente de etiquetas\n\n")

    cat("📊 GRÁFICOS ESTADÍSTICOS:\n")
    cat("11. Comprobar que cada opción tenga su gráfico correspondiente\n")
    cat("12. Verificar que los valores en gráficos coincidan con los datos\n")
    cat("13. Validar que las escalas sean apropiadas y no distorsionen\n")
    cat("14. Comprobar que las leyendas y títulos sean descriptivos\n")
    cat("15. Verificar que no haya elementos visuales confusos\n\n")
  }

  cat("✅ CONTROL DE CALIDAD COMPLETADO\n")
  cat("📊 Puntuación final:", reporte_final$puntuacion, "/100\n")
  cat("🎯 Estado:", reporte_final$estado, "\n")

  # Mostrar información de correcciones si se aplicaron
  if (!is.null(archivo_corregido)) {
    cat("\n🛠️ CORRECCIONES AUTOMÁTICAS APLICADAS\n")
    cat("📁 Archivo corregido:", archivo_corregido, "\n")
    cat("📈 Mejora obtenida:", reporte_final$mejora, "puntos\n")
    cat("💡 Recomendación: Revisar el archivo corregido y usar esa versión.\n")
  }

  return(reporte_final)
}

# =============================================================================
# FUNCIÓN MEJORADA PARA APLICAR A CUALQUIER ARCHIVO .RMD
# =============================================================================

# Uso: revisar_ejercicio("mi-archivo.Rmd", aplicar_correcciones = TRUE)
revisar_ejercicio <- function(archivo_rmd, aplicar_correcciones = TRUE) {
  return(ejecutar_control_calidad_completo(archivo_rmd, aplicar_correcciones))
}

# =============================================================================
# FUNCIÓN PARA CORRECCIÓN AUTOMÁTICA MASIVA
# =============================================================================

corregir_todos_los_archivos <- function(patron = "*.Rmd", directorio = ".", aplicar_correcciones = TRUE) {

  cat("🔧 CORRECCIÓN AUTOMÁTICA MASIVA\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")

  # Buscar todos los archivos .Rmd
  archivos_rmd <- list.files(directorio, pattern = patron, full.names = TRUE)

  if (length(archivos_rmd) == 0) {
    cat("❌ No se encontraron archivos .Rmd en el directorio especificado\n")
    return(NULL)
  }

  cat("📁 Archivos encontrados:", length(archivos_rmd), "\n")
  for (archivo in archivos_rmd) {
    cat("  •", basename(archivo), "\n")
  }
  cat("\n")

  resultados <- list()
  archivos_corregidos <- c()

  for (i in seq_along(archivos_rmd)) {
    archivo <- archivos_rmd[i]
    cat("🔍 Procesando", i, "de", length(archivos_rmd), ":", basename(archivo), "\n")
    cat(paste(rep("-", 60), collapse = ""), "\n")

    tryCatch({
      resultado <- revisar_ejercicio(archivo, aplicar_correcciones)
      resultados[[basename(archivo)]] <- resultado

      if (!is.null(resultado$archivo_corregido)) {
        archivos_corregidos <- c(archivos_corregidos, resultado$archivo_corregido)
      }

    }, error = function(e) {
      cat("❌ Error procesando", basename(archivo), ":", e$message, "\n")
      resultados[[basename(archivo)]] <- list(error = e$message)
    })

    cat("\n")
  }

  # Resumen final
  cat("📊 RESUMEN DE CORRECCIÓN MASIVA\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  cat("📁 Archivos procesados:", length(archivos_rmd), "\n")
  cat("🛠️ Archivos corregidos:", length(archivos_corregidos), "\n")

  if (length(archivos_corregidos) > 0) {
    cat("📋 Archivos corregidos generados:\n")
    for (archivo in archivos_corregidos) {
      cat("  •", basename(archivo), "\n")
    }
  }

  return(list(
    resultados = resultados,
    archivos_corregidos = archivos_corregidos,
    total_procesados = length(archivos_rmd),
    total_corregidos = length(archivos_corregidos)
  ))
}


