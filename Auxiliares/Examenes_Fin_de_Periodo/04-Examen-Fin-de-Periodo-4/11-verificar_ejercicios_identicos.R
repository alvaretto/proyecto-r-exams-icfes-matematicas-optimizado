#!/usr/bin/env Rscript
################################################################################
# SCRIPT: 11-verificar_ejercicios_identicos.R
# DESCRIPCIÓN: Verifica que todos los formatos generados contengan
#              exactamente los mismos ejercicios .Rmd en el mismo orden
# AUTOR: Sistema ICFES R-Exams
################################################################################

library(exams)

cat("\n")
cat("================================================================================\n")
cat("  VERIFICACIÓN DE CONSISTENCIA DE EJERCICIOS ENTRE FORMATOS\n")
cat("================================================================================\n")
cat("\n")

# Leer los archivos RDS generados por cada formato
archivos_rds <- list.files("salida", pattern = "\\.rds$", full.names = TRUE)

if (length(archivos_rds) == 0) {
  cat("❌ ERROR: No se encontraron archivos .rds en el directorio salida/\n")
  cat("   Ejecute primero SemilleroFinDePeriodo_v4.R para generar los exámenes.\n\n")
  quit(status = 1)
}

cat(sprintf("Archivos .rds encontrados: %d\n\n", length(archivos_rds)))

# Leer todos los archivos RDS
examenes <- list()
nombres_formatos <- c()

for (archivo in archivos_rds) {
  nombre_formato <- basename(archivo)
  nombres_formatos <- c(nombres_formatos, nombre_formato)
  examenes[[nombre_formato]] <- readRDS(archivo)
  cat(sprintf("✓ Leído: %s\n", nombre_formato))
}

cat("\n")
cat("================================================================================\n")
cat("  EXTRAYENDO NOMBRES DE EJERCICIOS DE CADA FORMATO\n")
cat("================================================================================\n")
cat("\n")

# Extraer los nombres de los ejercicios de cada formato
ejercicios_por_formato <- list()

for (nombre_formato in nombres_formatos) {
  examen <- examenes[[nombre_formato]]
  
  # Los exámenes están en formato de lista
  # Cada elemento de la lista es un examen (en nuestro caso, solo 1)
  if (length(examen) > 0) {
    # Extraer los nombres de los ejercicios del primer examen
    ejercicios <- sapply(examen[[1]], function(x) {
      # Cada ejercicio tiene un campo 'metainfo' con 'file'
      if (!is.null(x$metainfo$file)) {
        return(basename(x$metainfo$file))
      } else {
        return(NA)
      }
    })
    
    ejercicios_por_formato[[nombre_formato]] <- ejercicios
    
    cat(sprintf("Formato: %s\n", nombre_formato))
    cat(sprintf("Número de ejercicios: %d\n", length(ejercicios)))
    cat("Ejercicios:\n")
    for (i in 1:length(ejercicios)) {
      cat(sprintf("  %2d. %s\n", i, ejercicios[i]))
    }
    cat("\n")
  }
}

cat("================================================================================\n")
cat("  COMPARANDO EJERCICIOS ENTRE FORMATOS\n")
cat("================================================================================\n")
cat("\n")

# Tomar el primer formato como referencia
formato_referencia <- nombres_formatos[1]
ejercicios_referencia <- ejercicios_por_formato[[formato_referencia]]

cat(sprintf("Formato de referencia: %s\n", formato_referencia))
cat(sprintf("Número de ejercicios: %d\n\n", length(ejercicios_referencia)))

# Comparar con los demás formatos
todos_identicos <- TRUE

for (nombre_formato in nombres_formatos[-1]) {
  ejercicios_actual <- ejercicios_por_formato[[nombre_formato]]
  
  cat(sprintf("Comparando con: %s\n", nombre_formato))
  
  # Verificar que tengan el mismo número de ejercicios
  if (length(ejercicios_referencia) != length(ejercicios_actual)) {
    cat(sprintf("  ❌ DIFERENTE: Número de ejercicios no coincide (%d vs %d)\n\n",
                length(ejercicios_referencia), length(ejercicios_actual)))
    todos_identicos <- FALSE
    next
  }
  
  # Verificar que los ejercicios sean idénticos en el mismo orden
  diferencias <- ejercicios_referencia != ejercicios_actual
  
  if (any(diferencias)) {
    cat("  ❌ DIFERENTE: Los ejercicios no coinciden\n")
    cat("  Diferencias encontradas:\n")
    for (i in which(diferencias)) {
      cat(sprintf("    Posición %d: %s vs %s\n", i,
                  ejercicios_referencia[i], ejercicios_actual[i]))
    }
    cat("\n")
    todos_identicos <- FALSE
  } else {
    cat("  ✅ IDÉNTICO: Todos los ejercicios coinciden en orden y contenido\n\n")
  }
}

cat("================================================================================\n")
cat("  RESULTADO FINAL\n")
cat("================================================================================\n")
cat("\n")

if (todos_identicos) {
  cat("✅ ÉXITO: Todos los formatos contienen exactamente los mismos ejercicios\n")
  cat("         en el mismo orden.\n\n")
  cat("El script SemilleroFinDePeriodo_v4.R está funcionando correctamente.\n\n")
} else {
  cat("❌ ERROR: Se encontraron diferencias entre los formatos.\n\n")
  cat("El script SemilleroFinDePeriodo_v4.R necesita corrección.\n\n")
}

cat("================================================================================\n")
cat("\n")

