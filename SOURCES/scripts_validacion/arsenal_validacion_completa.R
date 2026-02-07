#!/usr/bin/env Rscript
# =============================================================================
# arsenal_validacion_completa.R (v1.0)
# ARSENAL COMPLETO DE VALIDACIONES POST-RENDERIZADO
#
# Se ejecuta AUTOMÁTICAMENTE después de TODA renderización exams2*
# Incluye TODAS las validaciones sin excepción:
#
# FASE 2A: Coherencia Matemática (existente)
# FASE 2B: Preview Visual (existente)
# FASE 2C: Validación de Opciones Únicas (NUEVO)
# FASE 2D: Validación de Ortografía (NUEVO)
# FASE 2E: Validación de Metadatos ICFES (NUEVO)
# FASE 2F: Validación de Estructura Metacognitiva (NUEVO)
#
# Uso: Rscript arsenal_validacion_completa.R archivo.Rmd [--fix]
# =============================================================================

suppressPackageStartupMessages({
  library(knitr)
})

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("  ARSENAL DE VALIDACIÓN COMPLETA\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("Uso: Rscript arsenal_validacion_completa.R archivo.Rmd [--fix]\n")
  quit(status = 1)
}

rmd_file <- args[1]
fix_mode <- "--fix" %in% args

if (!file.exists(rmd_file)) {
  cat("ERROR: Archivo no encontrado:", rmd_file, "\n")
  quit(status = 1)
}

# Leer contenido del archivo
contenido <- readLines(rmd_file, warn = FALSE)
contenido_texto <- paste(contenido, collapse = "\n")

# =============================================================================
# CONTADORES GLOBALES
# =============================================================================
errores_totales <- 0
advertencias_totales <- 0
resultados <- list()

cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("  ARSENAL DE VALIDACIÓN COMPLETA POST-RENDERIZADO\n")
cat("  Archivo:", basename(rmd_file), "\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# =============================================================================
# FASE 2C: VALIDACIÓN DE OPCIONES ÚNICAS (GRÁFICOS)
# =============================================================================
cat("───────────────────────────────────────────────────────────────\n")
cat("FASE 2C: Validación de Opciones Únicas\n")
cat("───────────────────────────────────────────────────────────────\n")

fase_2c_errores <- 0

# Verificar si es ejercicio con gráficos como opciones
tiene_graficos_opciones <- grepl("diagrama_[a-d]\\.png|plot_[A-D]|opciones_graficos", contenido_texto)

if (tiene_graficos_opciones) {
  # Verificar patrón de validación de unicidad
  tiene_validacion_hashes <- grepl("hashes|unique\\(hashes\\)|length\\(unique", contenido_texto)

  if (!tiene_validacion_hashes) {
    cat("  ⚠️  ADVERTENCIA: Ejercicio con gráficos pero sin validación de unicidad\n")
    cat("      Agregar verificación: length(unique(hashes)) == 4\n")
    advertencias_totales <- advertencias_totales + 1
  } else {
    cat("  ✓ Validación de opciones únicas presente\n")
  }

  # Verificar que no usa grid.arrange para opciones
  usa_grid_arrange <- grepl("grid\\.arrange.*plot_[A-D]|grid\\.arrange.*opciones", contenido_texto)
  if (usa_grid_arrange) {
    cat("  ❌ ERROR: Usa grid.arrange() para mostrar opciones (PROHIBIDO)\n")
    cat("      Cada opción debe ser PNG separado\n")
    fase_2c_errores <- fase_2c_errores + 1
  }

  # Verificar archivos individuales
  tiene_pngs_individuales <- grepl("diagrama_a\\.png.*diagrama_b\\.png|ggsave.*diagrama_.*\\.png", contenido_texto)
  if (!tiene_pngs_individuales) {
    cat("  ⚠️  ADVERTENCIA: No se detectan archivos PNG individuales por opción\n")
    advertencias_totales <- advertencias_totales + 1
  } else {
    cat("  ✓ Archivos PNG individuales por opción\n")
  }
} else {
  cat("  ○ No aplica (ejercicio sin gráficos como opciones)\n")
}

errores_totales <- errores_totales + fase_2c_errores
resultados$fase_2c <- list(errores = fase_2c_errores, status = ifelse(fase_2c_errores == 0, "OK", "ERROR"))

# =============================================================================
# FASE 2D: VALIDACIÓN DE ORTOGRAFÍA
# =============================================================================
cat("\n───────────────────────────────────────────────────────────────\n")
cat("FASE 2D: Validación de Ortografía Española\n")
cat("───────────────────────────────────────────────────────────────\n")

fase_2d_errores <- 0

# Palabras que SIEMPRE deben llevar tilde
palabras_con_tilde <- c(
  "mas" = "más",
  "numero" = "número",
  "calculo" = "cálculo",
  "angulo" = "ángulo",
  "grafica" = "gráfica",
  "grafico" = "gráfico",
  "funcion" = "función",
  "ecuacion" = "ecuación",
  "solucion" = "solución",
  "informacion" = "información",
  "relacion" = "relación",
  "descripcion" = "descripción",
  "variacion" = "variación",
  "seleccion" = "selección",
  "seccion" = "sección",
  "expresion" = "expresión",
  "posicion" = "posición",
  "operacion" = "operación",
  "clasificacion" = "clasificación",
  "estadistica" = "estadística",
  "aritmetica" = "aritmética",
  "geometrica" = "geométrica",
  "matematica" = "matemática",
  "unico" = "único",
  "minimo" = "mínimo",
  "maximo" = "máximo",
  "metodo" = "método",
  "codigo" = "código",
  "analisis" = "análisis"
)

# Excluir contenido de chunks R y metadatos
contenido_texto_limpio <- contenido_texto
# Remover sección YAML del inicio (entre ---)
contenido_texto_limpio <- gsub("^---[\\s\\S]*?---", "", contenido_texto_limpio, perl = TRUE)
# Remover chunks R completos (incluyendo multilinea)
contenido_texto_limpio <- gsub("```\\{r[^`]*```", "", contenido_texto_limpio, perl = TRUE)
# Remover código R inline
contenido_texto_limpio <- gsub("`r [^`]+`", "", contenido_texto_limpio)
# Remover metadatos YAML/R-exams (Meta-information section)
contenido_texto_limpio <- gsub("exname:.*|extype:.*|exsolution:.*|exshuffle:.*|extol:.*|exextra\\[.*", "", contenido_texto_limpio)
# Remover códigos de error conceptuales (EST-BOX-01, ALG-OPE-01, etc.)
contenido_texto_limpio <- gsub("[A-Z]{2,4}-[A-Z]{2,4}-\\d{2}", "", contenido_texto_limpio)
# Remover nombres de variables R comunes
contenido_texto_limpio <- gsub("\\b(codigo|estadistica|grafica|angulo|numero|funcion|solucion|ecuacion)_[a-z_]+\\b", "", contenido_texto_limpio)
# Remover líneas que comienzan con identificadores de categoría YAML
contenido_texto_limpio <- gsub("\\b(categoria|componente|competencia):\\s*[a-z_]+", "", contenido_texto_limpio)

errores_ortografia <- c()
for (incorrecta in names(palabras_con_tilde)) {
  patron <- paste0("\\b", incorrecta, "\\b")
  if (grepl(patron, contenido_texto_limpio, ignore.case = FALSE)) {
    correcta <- palabras_con_tilde[incorrecta]
    errores_ortografia <- c(errores_ortografia, paste0("  '", incorrecta, "' → '", correcta, "'"))
    fase_2d_errores <- fase_2d_errores + 1
  }
}

if (length(errores_ortografia) > 0) {
  cat("  ❌ Errores de tildes encontrados:\n")
  cat(paste(errores_ortografia, collapse = "\n"), "\n")
} else {
  cat("  ✓ Ortografía de tildes correcta\n")
}

errores_totales <- errores_totales + fase_2d_errores
resultados$fase_2d <- list(errores = fase_2d_errores, status = ifelse(fase_2d_errores == 0, "OK", "ERROR"))

# =============================================================================
# FASE 2E: VALIDACIÓN DE METADATOS ICFES
# =============================================================================
cat("\n───────────────────────────────────────────────────────────────\n")
cat("FASE 2E: Validación de Metadatos ICFES\n")
cat("───────────────────────────────────────────────────────────────\n")

fase_2e_errores <- 0

# Metadatos obligatorios
metadatos_obligatorios <- c(
  "exname",
  "extype",
  "exsolution",
  "exshuffle"
)

metadatos_icfes <- c(
  "exextra\\[Competencia\\]",
  "exextra\\[Componente\\]",
  "exextra\\[Nivel\\]"
)

for (meta in metadatos_obligatorios) {
  if (!grepl(meta, contenido_texto)) {
    cat("  ❌ Falta metadato obligatorio:", meta, "\n")
    fase_2e_errores <- fase_2e_errores + 1
  }
}

# Verificar exshuffle: TRUE
if (grepl("exshuffle:\\s*FALSE", contenido_texto, ignore.case = TRUE)) {
  cat("  ❌ ERROR CRÍTICO: exshuffle debe ser TRUE (OBLIGATORIO)\n")
  fase_2e_errores <- fase_2e_errores + 1
} else if (grepl("exshuffle:\\s*TRUE", contenido_texto, ignore.case = TRUE)) {
  cat("  ✓ exshuffle: TRUE (correcto)\n")
} else {
  cat("  ⚠️  exshuffle no encontrado explícitamente\n")
  advertencias_totales <- advertencias_totales + 1
}

# Verificar metadatos ICFES
icfes_faltantes <- 0
for (meta in metadatos_icfes) {
  if (!grepl(meta, contenido_texto)) {
    icfes_faltantes <- icfes_faltantes + 1
  }
}

if (icfes_faltantes > 0) {
  cat("  ⚠️  Faltan", icfes_faltantes, "metadatos ICFES (Competencia/Componente/Nivel)\n")
  advertencias_totales <- advertencias_totales + icfes_faltantes
} else {
  cat("  ✓ Metadatos ICFES completos\n")
}

# Verificar campo Tarea
if (!grepl("Tarea|tarea", contenido_texto)) {
  cat("  ⚠️  Campo 'Tarea' no encontrado en Solution\n")
  advertencias_totales <- advertencias_totales + 1
} else {
  cat("  ✓ Campo 'Tarea' presente\n")
}

errores_totales <- errores_totales + fase_2e_errores
resultados$fase_2e <- list(errores = fase_2e_errores, status = ifelse(fase_2e_errores == 0, "OK", "ERROR"))

# =============================================================================
# FASE 2F: VALIDACIÓN DE ESTRUCTURA METACOGNITIVA
# =============================================================================
cat("\n───────────────────────────────────────────────────────────────\n")
cat("FASE 2F: Validación de Estructura Metacognitiva\n")
cat("───────────────────────────────────────────────────────────────\n")

fase_2f_errores <- 0

# Verificar sección Solution existe
tiene_solution <- grepl("Solution\\s*\\n=+", contenido_texto)
if (!tiene_solution) {
  cat("  ❌ Falta sección Solution\n")
  fase_2f_errores <- fase_2f_errores + 1
} else {
  cat("  ✓ Sección Solution presente\n")

  # Verificar subsecciones obligatorias de Solution
  subsecciones <- c(
    "Información de la Pregunta" = "tabla de metadatos",
    "Qué evalúa" = "descripción de evaluación",
    "Respuesta Correcta" = "identificación de respuesta",
    "Opciones No Válidas|opciones incorrectas" = "análisis de distractores",
    "Reflexión Metacognitiva|reflexion" = "componente metacognitivo"
  )

  for (patron in names(subsecciones)) {
    if (!grepl(patron, contenido_texto, ignore.case = TRUE)) {
      cat("  ⚠️  Falta:", subsecciones[patron], "\n")
      advertencias_totales <- advertencias_totales + 1
    }
  }

  # Verificar patrón "Es posible que los estudiantes"
  tiene_patron_icfes <- grepl("Es posible que (los )?estudiantes", contenido_texto, ignore.case = TRUE)
  if (!tiene_patron_icfes) {
    cat("  ⚠️  Falta patrón ICFES: 'Es posible que los estudiantes...'\n")
    advertencias_totales <- advertencias_totales + 1
  } else {
    cat("  ✓ Patrón ICFES de retroalimentación presente\n")
  }
}

# Verificar pool de errores conceptuales
tiene_pool_errores <- grepl("errores_conceptuales|error.*codigo.*=|EST-|ALG-|GEO-|ARI-", contenido_texto)
if (!tiene_pool_errores) {
  cat("  ⚠️  No se detecta pool de errores conceptuales con códigos\n")
  advertencias_totales <- advertencias_totales + 1
} else {
  cat("  ✓ Pool de errores conceptuales presente\n")
}

# Verificar letra_correcta dinámica
tiene_letra_correcta <- grepl("letra_correcta", contenido_texto)
if (!tiene_letra_correcta) {
  cat("  ⚠️  Variable 'letra_correcta' no encontrada\n")
  advertencias_totales <- advertencias_totales + 1
} else {
  cat("  ✓ Variable 'letra_correcta' para Solution dinámica\n")
}

errores_totales <- errores_totales + fase_2f_errores
resultados$fase_2f <- list(errores = fase_2f_errores, status = ifelse(fase_2f_errores == 0, "OK", "ERROR"))

# =============================================================================
# RESUMEN FINAL
# =============================================================================
cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("  RESUMEN DEL ARSENAL DE VALIDACIÓN\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("\n")

cat("  FASE 2C (Opciones Únicas):      ", resultados$fase_2c$status, "\n")
cat("  FASE 2D (Ortografía):           ", resultados$fase_2d$status, "\n")
cat("  FASE 2E (Metadatos ICFES):      ", resultados$fase_2e$status, "\n")
cat("  FASE 2F (Metacognición):        ", resultados$fase_2f$status, "\n")
cat("\n")
cat("  Total ERRORES:      ", errores_totales, "\n")
cat("  Total ADVERTENCIAS: ", advertencias_totales, "\n")
cat("\n")

if (errores_totales > 0) {
  cat("  ❌ VALIDACIÓN FALLIDA - Corregir errores antes de continuar\n")
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  quit(status = 1)
} else if (advertencias_totales > 0) {
  cat("  ⚠️  VALIDACIÓN CON ADVERTENCIAS - Revisar antes de aprobar\n")
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  quit(status = 0)
} else {
  cat("  ✓ VALIDACIÓN COMPLETA APROBADA\n")
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  quit(status = 0)
}
