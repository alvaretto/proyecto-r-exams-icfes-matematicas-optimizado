# Script de Validación de Coherencia - FASE 2
# Ciclo de Validación Automática

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║  🔍 FASE 2: VALIDACIÓN VISUAL Y FUNCIONAL                ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

archivo <- "migracion_atun_representacion_grafica_aleatorio_interpretacion_n2_v1.Rmd"
contenido <- readLines(archivo)

# Inicializar contadores de errores
errores_matematica <- 0
errores_imagen_texto <- 0
errores_codigo <- 0
advertencias <- list()

cat("📋 Analizando coherencia del ejercicio...\n\n")

# ═══════════════════════════════════════════════════════════
# 1. COHERENCIA MATEMÁTICA (ERR_C1)
# ═══════════════════════════════════════════════════════════
cat("1️⃣  COHERENCIA MATEMÁTICA\n")
cat("─────────────────────────────────────────────────────────\n")

# Verificar que exsolution esté presente
exsolution_lineas <- grep("^exsolution:", contenido, value = TRUE)
if (length(exsolution_lineas) > 0) {
  exsolution_valor <- gsub("exsolution:\\s*", "", exsolution_lineas[1])
  cat("   ✓ exsolution encontrado:", exsolution_valor, "\n")

  # Verificar que sea válido para schoice (debe tener exactamente un '1')
  num_unos <- length(gregexpr("1", exsolution_valor)[[1]])
  if (num_unos == 1) {
    cat("   ✓ Formato válido para schoice (1 respuesta correcta)\n")
  } else {
    cat("   ❌ ERROR: exsolution debe tener exactamente un '1'\n")
    errores_matematica <- errores_matematica + 1
  }
} else {
  cat("   ❌ ERROR: No se encontró exsolution\n")
  errores_matematica <- errores_matematica + 1
}

# Verificar que extype sea schoice
extype_lineas <- grep("^extype:", contenido, value = TRUE)
if (length(extype_lineas) > 0) {
  extype_valor <- gsub("extype:\\s*", "", extype_lineas[1])
  cat("   ✓ extype encontrado:", extype_valor, "\n")

  if (extype_valor == "schoice") {
    cat("   ✓ Tipo correcto para ejercicio de selección única\n")
  } else {
    cat("   ⚠️  ADVERTENCIA: extype debería ser 'schoice'\n")
    advertencias <- c(advertencias, "extype no es 'schoice'")
  }
}

# Verificar cálculos matemáticos en data_generation
cat("   ✓ Verificando cálculos de la función cuadrática...\n")

# Buscar definición de vértice
vertice_lineas <- grep("d_vertice.*<-.*b.*2", contenido)
if (length(vertice_lineas) > 0) {
  cat("   ✓ Cálculo de vértice encontrado (línea", vertice_lineas[1], ")\n")
} else {
  cat("   ⚠️  ADVERTENCIA: No se encontró cálculo de vértice\n")
  advertencias <- c(advertencias, "Verificar cálculo de vértice")
}

# Buscar cálculo de p_max
pmax_lineas <- grep("p_max.*<-.*-d_vertice.*2.*\\+.*b.*d_vertice.*\\+.*c", contenido)
if (length(pmax_lineas) > 0) {
  cat("   ✓ Cálculo de p_max encontrado (línea", pmax_lineas[1], ")\n")
} else {
  cat("   ⚠️  ADVERTENCIA: No se encontró cálculo de p_max\n")
  advertencias <- c(advertencias, "Verificar cálculo de p_max")
}

cat("\n")

# ═══════════════════════════════════════════════════════════
# 2. COHERENCIA IMAGEN-TEXTO (ERR_C2)
# ═══════════════════════════════════════════════════════════
cat("2️⃣  COHERENCIA IMAGEN-TEXTO\n")
cat("─────────────────────────────────────────────────────────\n")

# Verificar que se genera el código TikZ
tikz_gen_lineas <- grep("generar_tikz_migracion_atun", contenido)
if (length(tikz_gen_lineas) > 0) {
  cat("   ✓ Función de generación TikZ encontrada\n")
} else {
  cat("   ❌ ERROR: No se encontró función de generación TikZ\n")
  errores_imagen_texto <- errores_imagen_texto + 1
}

# Verificar que se usan los datos correctos en TikZ
coords_lineas <- grep("coords_b.*<-.*paste.*dias_correctos.*pesca_correcta", contenido)
if (length(coords_lineas) > 0) {
  cat("   ✓ Coordenadas TikZ sincronizadas con datos calculados\n")
} else {
  cat("   ⚠️  ADVERTENCIA: Verificar sincronización de coordenadas TikZ\n")
  advertencias <- c(advertencias, "Verificar coords_b en TikZ")
}

# Verificar que los puntos se calculan con la fórmula correcta
formula_puntos <- grep("pesca_correcta.*sapply.*-d\\^2.*\\+.*b_coef.*\\*.*d.*\\+.*c_coef", contenido)
if (length(formula_puntos) > 0) {
  cat("   ✓ Puntos calculados con fórmula P = -d² + b*d + c\n")
} else {
  cat("   ❌ ERROR: Fórmula de puntos no encontrada o incorrecta\n")
  errores_imagen_texto <- errores_imagen_texto + 1
}

# Verificar que el enunciado muestra la fórmula correcta
formula_enunciado <- grep("\\$\\$P = -d\\^2 \\+", contenido)
if (length(formula_enunciado) > 0) {
  cat("   ✓ Fórmula mostrada en enunciado (línea", formula_enunciado[1], ")\n")
} else {
  cat("   ⚠️  ADVERTENCIA: Verificar fórmula en enunciado\n")
  advertencias <- c(advertencias, "Verificar fórmula en Question")
}

cat("\n")

# ═══════════════════════════════════════════════════════════
# 3. COHERENCIA DE CÓDIGO (ERR_C3)
# ═══════════════════════════════════════════════════════════
cat("3️⃣  COHERENCIA DE CÓDIGO\n")
cat("─────────────────────────────────────────────────────────\n")

# Verificar que no hay funciones matemáticas sobre strings
abs_formateado <- grep("abs\\(.*formateado", contenido)
round_formateado <- grep("round\\(.*formateado", contenido)

if (length(abs_formateado) > 0) {
  cat("   ❌ ERROR: abs() sobre variable formateada (línea", abs_formateado[1], ")\n")
  errores_codigo <- errores_codigo + 1
} else if (length(round_formateado) > 0) {
  cat("   ❌ ERROR: round() sobre variable formateada (línea", round_formateado[1], ")\n")
  errores_codigo <- errores_codigo + 1
} else {
  cat("   ✓ No se encontraron funciones matemáticas sobre strings\n")
}

# Verificar uso correcto de variables R en inline code
inline_r <- grep("`r ", contenido, fixed = TRUE)
cat("   ✓ Variables R inline encontradas:", length(inline_r), "instancias\n")

# Verificar que las variables usadas inline existen en data_generation
variables_inline <- c("especie", "region", "b_coef", "c_coef", "d_vertice", "p_max")
data_gen_start <- grep("```\\{r data_generation", contenido)[1]
data_gen_end <- grep("```", contenido)
data_gen_end <- data_gen_end[data_gen_end > data_gen_start][1]

variables_no_definidas <- c()
for (var in variables_inline) {
  var_def <- grep(paste0(var, "\\s*<-"), contenido[data_gen_start:data_gen_end])
  if (length(var_def) == 0) {
    variables_no_definidas <- c(variables_no_definidas, var)
  }
}

if (length(variables_no_definidas) > 0) {
  cat("   ⚠️  ADVERTENCIA: Variables no definidas en data_generation:",
      paste(variables_no_definidas, collapse = ", "), "\n")
  advertencias <- c(advertencias, paste("Variables no definidas:", paste(variables_no_definidas, collapse = ", ")))
} else {
  cat("   ✓ Todas las variables inline están definidas\n")
}

# Verificar test de diversidad
diversity_test <- grep("expect_true\\(n_versiones_unicas >= 300", contenido)
if (length(diversity_test) > 0) {
  cat("   ✓ Test de diversidad presente (>= 300 versiones)\n")
} else {
  cat("   ⚠️  ADVERTENCIA: Test de diversidad no encontrado\n")
  advertencias <- c(advertencias, "Agregar test de diversidad >= 300")
}

cat("\n")

# ═══════════════════════════════════════════════════════════
# 4. COHERENCIA DE METADATOS
# ═══════════════════════════════════════════════════════════
cat("4️⃣  COHERENCIA DE METADATOS ICFES\n")
cat("─────────────────────────────────────────────────────────\n")

# Verificar metadatos ICFES
icfes_competencia <- grep("competencia:", contenido, value = TRUE)
icfes_nivel <- grep("nivel_dificultad:", contenido, value = TRUE)
icfes_componente <- grep("componente:", contenido, value = TRUE)

if (length(icfes_competencia) > 0) {
  cat("   ✓", icfes_competencia[1], "\n")
}
if (length(icfes_nivel) > 0) {
  cat("   ✓", icfes_nivel[1], "\n")
}
if (length(icfes_componente) > 0) {
  cat("   ✓", icfes_componente[1], "\n")
}

cat("\n")

# ═══════════════════════════════════════════════════════════
# REPORTE FINAL
# ═══════════════════════════════════════════════════════════
cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║              📊 REPORTE DE COHERENCIA - FASE 2            ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

cat("📊 ERRORES DETECTADOS:\n")
cat("   Matemática:     ", errores_matematica, "error(es)\n")
cat("   Imagen-Texto:   ", errores_imagen_texto, "error(es)\n")
cat("   Código:         ", errores_codigo, "error(es)\n")
cat("\n")

total_errores <- errores_matematica + errores_imagen_texto + errores_codigo

if (length(advertencias) > 0) {
  cat("⚠️  ADVERTENCIAS (", length(advertencias), "):\n")
  for (i in 1:length(advertencias)) {
    cat("   ", i, ".", advertencias[[i]], "\n")
  }
  cat("\n")
}

cat("═══════════════════════════════════════════════════════\n")
if (total_errores == 0) {
  cat("✅ FASE 2 COMPLETADA - Sin errores críticos\n")
  if (length(advertencias) > 0) {
    cat("⚠️  Revisar", length(advertencias), "advertencia(s)\n")
  }
  cat("\n→ Siguiente paso: Aprobar para producción o /promover-ejercicio\n")
} else {
  cat("❌ FASE 2 CON ERRORES - Total:", total_errores, "\n")
  cat("\n→ Siguiente paso: Ejecutar /diagnosticar-errores\n")
}
cat("═══════════════════════════════════════════════════════\n\n")
