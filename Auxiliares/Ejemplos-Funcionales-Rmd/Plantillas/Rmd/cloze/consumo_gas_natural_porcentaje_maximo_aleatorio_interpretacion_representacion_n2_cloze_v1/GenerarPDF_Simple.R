# ===============================================================================
# GENERADOR DE PDF SIMPLIFICADO PARA EXÁMENES CLOZE
# ===============================================================================

library(exams)

# Configuración
archivo_original <- "costo_promedio_diario_numerico_variacional_formulacion_ejecucion_n2_cloze_v1.Rmd"
dir_salida <- "salida_pdf_simple"

# Crear directorio de salida
if (!dir.exists(dir_salida)) {
  dir.create(dir_salida, recursive = TRUE)
}

cat("🔧 Creando versión simplificada para PDF...\n")

# Leer archivo original
contenido <- readLines(archivo_original, warn = FALSE)

# Encontrar secciones importantes
inicio_question <- which(grepl("^Question", contenido))
inicio_solution <- which(grepl("^Solution", contenido))
inicio_meta <- which(grepl("^Meta-information", contenido))

if (length(inicio_question) == 0 || length(inicio_solution) == 0 || length(inicio_meta) == 0) {
  stop("❌ No se pudieron encontrar las secciones principales del archivo")
}

# Crear header simplificado
header_simple <- c(
  "---",
  "output:",
  "  pdf_document:",
  "    latex_engine: pdflatex",
  "    keep_tex: false",
  "header-includes:",
  "- \\usepackage[spanish]{babel}",
  "- \\usepackage{amsmath}",
  "- \\usepackage{graphicx}",
  "---",
  "",
  "```{r setup, include=FALSE}",
  "library(exams)",
  "options(OutDec = '.')",
  "```",
  "",
  "```{r variables, include=FALSE}",
  "# Variables simplificadas para PDF",
  "nombres_masculinos <- c('Pedro', 'Carlos', 'Miguel', 'Antonio', 'Diego')",
  "nombres_femeninos <- c('María', 'Ana', 'Carmen', 'Laura', 'Sofía')",
  "",
  "nombre_masculino <- sample(nombres_masculinos, 1)",
  "nombre_femenino <- sample(nombres_femeninos, 1)",
  "",
  "if(sample(c(TRUE, FALSE), 1)) {",
  "  nombre1 <- nombre_masculino",
  "  nombre2 <- nombre_femenino",
  "} else {",
  "  nombre1 <- nombre_femenino", 
  "  nombre2 <- nombre_masculino",
  "}",
  "",
  "tipos_vivienda <- c('apartamento', 'hogar', 'aparta-estudio')",
  "tipo_vivienda <- sample(tipos_vivienda, 1)",
  "",
  "meses_disponibles <- c('Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo', 'Junio',",
  "                      'Julio', 'Agosto', 'Septiembre', 'Octubre', 'Noviembre', 'Diciembre')",
  "",
  "inicio_mes <- sample(1:6, 1)",
  "meses_seleccionados <- meses_disponibles[inicio_mes:(inicio_mes + 6)]",
  "posicion_pregunta <- sample(1:6, 1)",
  "mes_pregunta <- meses_seleccionados[posicion_pregunta]",
  "mes_factura <- meses_seleccionados[7]",
  "",
  "meses <- meses_seleccionados",
  "consumos_base <- numeric(7)",
  "for(i in 1:7) {",
  "  consumos_base[i] <- sample(10:18, 1)",
  "}",
  "consumos <- consumos_base",
  "names(consumos) <- meses",
  "",
  "# Datos económicos",
  "cobro_total <- sample(15000:25000, 1)",
  "valor_fijo <- sample(3000:8000, 1)",
  "costo_consumo_variable <- cobro_total - valor_fijo",
  "costo_promedio_diario <- round(costo_consumo_variable / 30, 0)",
  "",
  "# Respuestas cloze",
  "respuesta_1 <- cobro_total",
  "respuesta_2 <- valor_fijo", 
  "respuesta_3 <- costo_consumo_variable",
  "respuesta_4 <- 30",
  "respuesta_5 <- round(costo_consumo_variable / 30, 4)",
  "respuesta_6 <- costo_promedio_diario",
  "",
  "# Distractores para schoice",
  "distractores <- c()",
  "distractor_1 <- round(cobro_total / 30, 0)",
  "if(distractor_1 != respuesta_6 && distractor_1 > 0) {",
  "  distractores <- c(distractores, distractor_1)",
  "}",
  "distractor_2 <- round(costo_consumo_variable / 31, 0)",
  "if(distractor_2 != respuesta_6 && distractor_2 > 0 && !distractor_2 %in% distractores) {",
  "  distractores <- c(distractores, distractor_2)",
  "}",
  "distractor_3 <- round(valor_fijo / 30, 0)",
  "if(distractor_3 != respuesta_6 && distractor_3 > 0 && !distractor_3 %in% distractores) {",
  "  distractores <- c(distractores, distractor_3)",
  "}",
  "",
  "# Completar distractores si es necesario",
  "opciones_adicionales <- c(90, 150, 200, 250, 300, 350, 400, 450, 500)",
  "for(opcion in sample(opciones_adicionales)) {",
  "  if(length(distractores) >= 3) break",
  "  if(!opcion %in% distractores) {",
  "    distractores <- c(distractores, opcion)",
  "  }",
  "}",
  "distractores <- distractores[1:3]",
  "",
  "# Opciones schoice",
  "opciones_schoice <- c(respuesta_6, distractores)",
  "opciones_texto <- paste0('$', opciones_schoice)",
  "orden_aleatorio <- sample(1:4)",
  "opciones_mezcladas <- opciones_texto[orden_aleatorio]",
  "indice_respuesta_correcta <- which(orden_aleatorio == 1)",
  "solucion_schoice <- rep(FALSE, 4)",
  "solucion_schoice[indice_respuesta_correcta] <- TRUE",
  "",
  "# Solución cloze",
  "solucion_cloze <- list(respuesta_1, respuesta_2, respuesta_3, respuesta_4, respuesta_5, respuesta_6)",
  "tipos_respuesta <- c('num', 'num', 'num', 'num', 'num', 'num', 'schoice')",
  "tolerancias <- c(0, 0, 0, 0, 0.0001, 0, 0)",
  "```",
  ""
)

# Obtener contenido desde Question hasta el final, pero sin chunks problemáticos
contenido_question <- contenido[inicio_question:length(contenido)]

# Remover referencias a gráficos Python
contenido_question <- gsub("```\\{r mostrar_grafico.*?```", "**Gráfico no disponible en versión PDF**", contenido_question, perl = TRUE)
contenido_question <- gsub("!\\[\\]\\(grafico_consumo_gas\\.png\\).*", "**Gráfico de consumo histórico (no disponible en PDF)**", contenido_question)

# Combinar todo
contenido_final <- c(header_simple, contenido_question)

# Crear archivo simplificado
archivo_simple <- "costo_promedio_diario_pdf_simple.Rmd"
writeLines(contenido_final, archivo_simple)

cat("✅ Archivo simplificado creado:", archivo_simple, "\n")

# Generar PDF
cat("📄 Generando PDF...\n")
tryCatch({
  set.seed(12345)
  resultado <- exams2pdf(archivo_simple,
                        n = 3,
                        name = "costo_promedio_diario_pdf",
                        dir = dir_salida,
                        template = "plain")
  
  cat("✅ PDF generado exitosamente\n")
  archivos <- list.files(dir_salida, full.names = TRUE)
  for(arch in archivos) {
    cat("  -", arch, "\n")
    if (grepl("\\.pdf$", arch)) {
      cat("    Tamaño:", file.size(arch), "bytes\n")
    }
  }
  
}, error = function(e) {
  cat("❌ Error:", e$message, "\n")
})

# Limpiar archivo temporal
if (file.exists(archivo_simple)) {
  file.remove(archivo_simple)
  cat("🧹 Archivo temporal eliminado\n")
}

cat("\n✅ Proceso completado. Revise el directorio:", dir_salida, "\n")
