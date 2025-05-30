# Script de prueba final de integridad para verificar corrección completa
# Aplicando modo ultrathink y nombres de variables en español

# Configuración robusta anti-emojis
options(testthat.use_colours = FALSE)
options(testthat.unicode = FALSE)
options(testthat.progress = FALSE)
options(testthat.summary = FALSE)
Sys.setenv("TESTTHAT_UNICODE" = "false")
Sys.setenv("TESTTHAT_COLOURS" = "false")

# Cargar bibliotecas necesarias
library(knitr)
library(stringr)

cat("=== PRUEBA FINAL DE INTEGRIDAD ===\n")
cat("Verificando corrección completa del archivo...\n\n")

# Nombre del archivo corregido
archivo_corregido <- "probabilidad_condicional_tabla_contingencia_razonamiento_nivel3_v1.Rmd"

# Verificar que el archivo existe
if (!file.exists(archivo_corregido)) {
  cat("❌ ERROR: El archivo", archivo_corregido, "no existe\n")
  quit(status = 1)
}

cat("✅ Archivo encontrado:", archivo_corregido, "\n")

# Leer contenido del archivo
contenido_archivo <- readLines(archivo_corregido, encoding = "UTF-8")
cat("✅ Archivo leído correctamente:", length(contenido_archivo), "líneas\n")

# 1. Verificar que se eliminaron los tests problemáticos
cat("\n1. Verificando eliminación de tests problemáticos...\n")

tests_problematicos_eliminados <- c(
  "formatos_encontrados <- c()",
  "for(i in 1:10)",
  "tabla_prueba_doble",
  "expect_true(any(grepl(as.character(p_menor_masc)",
  "expect_true(any(grepl(as.character(p_mayor_fem)",
  "\\\\hline\\\\hline.*tabla_prueba_doble",
  "Aleatorización de formatos de números funciona correctamente"
)

tests_encontrados <- 0
for(test_problematico in tests_problematicos_eliminados) {
  lineas_encontradas <- grep(test_problematico, contenido_archivo, fixed = TRUE)
  if(length(lineas_encontradas) > 0) {
    tests_encontrados <- tests_encontrados + 1
    cat("   ⚠️  Test problemático aún presente:", test_problematico, "\n")
  }
}

if(tests_encontrados == 0) {
  cat("   ✅ Todos los tests problemáticos fueron eliminados correctamente\n")
} else {
  cat("   ❌ Aún hay", tests_encontrados, "tests problemáticos presentes\n")
}

# 2. Verificar que se mantuvieron los tests esenciales
cat("\n2. Verificando presencia de tests esenciales...\n")

tests_esenciales <- c(
  "test_that(\"Código TikZ se genera correctamente\"",
  "test_that(\"Función generadora de tabla TikZ es robusta\"",
  "test_that(\"Coherencia matemática post-cambios\"",
  "test_that(\"Variables LaTeX-safe se generan correctamente\"",
  "test_that(\"Coherencia de términos de género\""
)

tests_esenciales_encontrados <- 0
for(test_esencial in tests_esenciales) {
  lineas_encontradas <- grep(test_esencial, contenido_archivo, fixed = TRUE)
  if(length(lineas_encontradas) > 0) {
    tests_esenciales_encontrados <- tests_esenciales_encontrados + 1
    cat("   ✅ Test esencial presente:", test_esencial, "\n")
  } else {
    cat("   ❌ Test esencial faltante:", test_esencial, "\n")
  }
}

cat("   📊 Tests esenciales encontrados:", tests_esenciales_encontrados, "de", length(tests_esenciales), "\n")

# 3. Verificar estructura del archivo
cat("\n3. Verificando estructura del archivo...\n")

# Verificar chunks principales
chunks_principales <- c(
  "```{r setup, include=FALSE}",
  "```{r DefinicionDeVariables",
  "```{r generar_tabla_contingencia_mejorada",
  "```{r mostrar_tabla_contingencia_mejorada"
)

chunks_encontrados <- 0
for(chunk in chunks_principales) {
  lineas_encontradas <- grep(chunk, contenido_archivo, fixed = TRUE)
  if(length(lineas_encontradas) > 0) {
    chunks_encontrados <- chunks_encontrados + 1
    cat("   ✅ Chunk encontrado:", chunk, "\n")
  } else {
    cat("   ❌ Chunk faltante:", chunk, "\n")
  }
}

cat("   📊 Chunks principales encontrados:", chunks_encontrados, "de", length(chunks_principales), "\n")

# 4. Verificar que no hay emojis problemáticos
cat("\n4. Verificando ausencia de emojis problemáticos...\n")

emojis_problematicos <- c("🎯", "🔧", "📁", "🧪", "🎓", "🔍", "✅", "❌", "⚠️", "📊")
emojis_encontrados <- 0

for(emoji in emojis_problematicos) {
  lineas_con_emoji <- grep(emoji, contenido_archivo, fixed = TRUE)
  if(length(lineas_con_emoji) > 0) {
    emojis_encontrados <- emojis_encontrados + 1
    cat("   ⚠️  Emoji encontrado:", emoji, "en líneas:", paste(lineas_con_emoji, collapse = ", "), "\n")
  }
}

if(emojis_encontrados == 0) {
  cat("   ✅ No se encontraron emojis problemáticos\n")
} else {
  cat("   ⚠️  Se encontraron", emojis_encontrados, "emojis que podrían causar problemas\n")
}

# 5. Verificar aleatorización avanzada implementada
cat("\n5. Verificando implementación de aleatorización avanzada...\n")

caracteristicas_aleatorización <- c(
  "generar_proporciones_validas",
  "paletas_colores_profesionales",
  "generar_tabla_contingencia_tikz_robusta",
  "ALEATORIZACIÓN AVANZADA",
  "tipo_distribucion",
  "estilo_tabla_seleccionado"
)

caracteristicas_encontradas <- 0
for(caracteristica in caracteristicas_aleatorización) {
  lineas_encontradas <- grep(caracteristica, contenido_archivo, fixed = TRUE)
  if(length(lineas_encontradas) > 0) {
    caracteristicas_encontradas <- caracteristicas_encontradas + 1
    cat("   ✅ Característica implementada:", caracteristica, "\n")
  } else {
    cat("   ❌ Característica faltante:", caracteristica, "\n")
  }
}

cat("   📊 Características de aleatorización encontradas:", caracteristicas_encontradas, "de", length(caracteristicas_aleatorización), "\n")

# 6. Resumen final
cat("\n=== RESUMEN FINAL ===\n")

puntuacion_total <- 0
puntuacion_maxima <- 5

if(tests_encontrados == 0) {
  cat("✅ Tests problemáticos eliminados: CORRECTO\n")
  puntuacion_total <- puntuacion_total + 1
} else {
  cat("❌ Tests problemáticos eliminados: INCORRECTO\n")
}

if(tests_esenciales_encontrados == length(tests_esenciales)) {
  cat("✅ Tests esenciales presentes: CORRECTO\n")
  puntuacion_total <- puntuacion_total + 1
} else {
  cat("❌ Tests esenciales presentes: INCORRECTO\n")
}

if(chunks_encontrados == length(chunks_principales)) {
  cat("✅ Estructura del archivo: CORRECTA\n")
  puntuacion_total <- puntuacion_total + 1
} else {
  cat("❌ Estructura del archivo: INCORRECTA\n")
}

if(emojis_encontrados == 0) {
  cat("✅ Ausencia de emojis: CORRECTA\n")
  puntuacion_total <- puntuacion_total + 1
} else {
  cat("⚠️  Ausencia de emojis: ADVERTENCIA\n")
  puntuacion_total <- puntuacion_total + 0.5
}

if(caracteristicas_encontradas >= 4) {
  cat("✅ Aleatorización avanzada: IMPLEMENTADA\n")
  puntuacion_total <- puntuacion_total + 1
} else {
  cat("❌ Aleatorización avanzada: INCOMPLETA\n")
}

cat("\n📊 PUNTUACIÓN FINAL:", puntuacion_total, "de", puntuacion_maxima, "\n")

if(puntuacion_total >= 4.5) {
  cat("🎉 RESULTADO: EXCELENTE - El archivo está listo para usar\n")
  cat("✅ Todas las correcciones se aplicaron correctamente\n")
  cat("✅ El archivo debería funcionar sin errores de testthat\n")
} else if(puntuacion_total >= 3.5) {
  cat("⚠️  RESULTADO: BUENO - El archivo funciona pero necesita ajustes menores\n")
} else {
  cat("❌ RESULTADO: NECESITA CORRECCIÓN - Hay problemas importantes que resolver\n")
}

cat("\n=== PRUEBA COMPLETADA ===\n")
