# Test para verificar si múltiples instancias del mismo .Rmd generan preguntas diferentes
# Este es el problema que reporta el usuario

library(exams)

cat("=== TEST DE REPETICIONES DENTRO DEL MISMO PDF ===\n\n")

# Configurar semilla
semilla <- 1001
set.seed(semilla)

# Generar 5 instancias de preg01
cat("Generando 5 instancias de preg01 (cateto_teorema_pitagoras)...\n\n")

ejercicios_preg01 <- exams2html(
  rep("cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_1.Rmd", 5),
  n = 1,
  name = "test_preg01",
  dir = "salida/test_repeticiones",
  edir = ".",
  verbose = FALSE,
  quiet = TRUE
)

# Extraer primeros 300 caracteres de cada pregunta
cat("Primeros 300 caracteres de cada instancia de preg01:\n\n")
for (i in 1:5) {
  pregunta_texto <- substr(ejercicios_preg01[[1]][[i]]$question, 1, 300)
  cat("Instancia", i, ":\n")
  cat(pregunta_texto, "...\n\n")
}

# Verificar si son idénticas
cat("=== VERIFICACIÓN DE IDENTIDAD ===\n\n")
identicas <- TRUE
for (i in 2:5) {
  if (!identical(ejercicios_preg01[[1]][[1]]$question, ejercicios_preg01[[1]][[i]]$question)) {
    identicas <- FALSE
    break
  }
}

if (identicas) {
  cat("❌ ERROR CRÍTICO: Las 5 instancias de preg01 son IDÉNTICAS\n")
  cat("   Esto confirma el problema reportado por el usuario\n\n")
} else {
  cat("✓ OK: Las instancias de preg01 son DIFERENTES\n\n")
}

# Ahora probar con preg03
cat("\n=== TEST CON PREG03 (que funciona bien) ===\n\n")

set.seed(semilla)  # Resetear semilla

ejercicios_preg03 <- exams2html(
  rep("ExportacionesGraficosEstadisticaInterpretacion_n3_v1.Rmd", 5),
  n = 1,
  name = "test_preg03",
  dir = "salida/test_repeticiones",
  edir = ".",
  verbose = FALSE,
  quiet = TRUE
)

cat("Primeros 300 caracteres de cada instancia de preg03:\n\n")
for (i in 1:5) {
  pregunta_texto <- substr(ejercicios_preg03[[1]][[i]]$question, 1, 300)
  cat("Instancia", i, ":\n")
  cat(pregunta_texto, "...\n\n")
}

# Verificar si son idénticas
identicas_preg03 <- TRUE
for (i in 2:5) {
  if (!identical(ejercicios_preg03[[1]][[1]]$question, ejercicios_preg03[[1]][[i]]$question)) {
    identicas_preg03 <- FALSE
    break
  }
}

if (identicas_preg03) {
  cat("❌ preg03: Las 5 instancias son IDÉNTICAS\n\n")
} else {
  cat("✓ preg03: Las instancias son DIFERENTES (como debería ser)\n\n")
}

# Limpiar
unlink("salida/test_repeticiones", recursive = TRUE)

cat("=== CONCLUSIÓN ===\n")
if (identicas && !identicas_preg03) {
  cat("CONFIRMADO: preg01 genera instancias idénticas, preg03 genera instancias diferentes\n")
  cat("El problema está en cómo preg01 usa la secuencia aleatoria\n")
} else if (!identicas && !identicas_preg03) {
  cat("AMBOS archivos generan instancias diferentes. El problema podría estar en otro lugar.\n")
} else {
  cat("Resultados inesperados. Necesita investigación adicional.\n")
}

