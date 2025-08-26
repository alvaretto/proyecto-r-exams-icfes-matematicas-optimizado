# ===================================================================
# AUDITORÍA DE CÓDIGO R Y FUNCIONALIDAD
# ===================================================================
# Script para validar la sintaxis, funcionalidad y estrategia avanzada

library(exams)
library(digest)
library(testthat)

cat("=== AUDITORÍA DE CÓDIGO R Y FUNCIONALIDAD ===\n\n")

# ===== PRUEBA 1: SINTAXIS Y COMPILACIÓN =====
cat("1. SINTAXIS Y COMPILACIÓN:\n")
cat("==========================\n")

tryCatch({
  # Intentar compilar el ejercicio
  resultado <- exams2html("pasteleria_sabores_ventas_estadistica_interpretacion_representacion_n2_v1.Rmd", 
                         n = 1, 
                         name = "test_sintaxis", 
                         dir = "test_output",
                         quiet = TRUE)
  cat("✅ CORRECTO: El ejercicio compila sin errores de sintaxis\n")
  sintaxis_ok <- TRUE
}, error = function(e) {
  cat("❌ ERROR DE SINTAXIS:", e$message, "\n")
  sintaxis_ok <- FALSE
})

# ===== PRUEBA 2: FUNCIONALIDAD DE generar_datos() =====
cat("\n2. FUNCIONALIDAD DE generar_datos():\n")
cat("====================================\n")

# Cargar la función
source_lines <- readLines("pasteleria_sabores_ventas_estadistica_interpretacion_representacion_n2_v1.Rmd")
start_line <- grep("generar_datos <- function", source_lines)
end_line <- grep("# Generar datos para este ejercicio", source_lines) - 1

if(length(start_line) > 0 && length(end_line) > 0) {
  function_lines <- source_lines[start_line:end_line]
  function_code <- paste(function_lines, collapse = "\n")
  
  tryCatch({
    eval(parse(text = function_code))
    cat("✅ CORRECTO: Función generar_datos() se carga correctamente\n")
    
    # Probar ejecución
    datos_test <- generar_datos()
    cat("✅ CORRECTO: Función generar_datos() se ejecuta correctamente\n")
    
    # Verificar estructura de retorno
    campos_esperados <- c("sabores", "preferencias", "sabores_mas_preferidos", 
                         "dias_semana", "ventas_por_dia", "dia_mayor_ventas",
                         "opciones", "respuesta_correcta", "posicion_correcta",
                         "tipos_opciones", "pool_correcta_usada", "distractores_tipos",
                         "validacion_unicidad")
    
    campos_faltantes <- setdiff(campos_esperados, names(datos_test))
    if(length(campos_faltantes) == 0) {
      cat("✅ CORRECTO: Estructura de retorno completa\n")
    } else {
      cat("❌ ERROR: Campos faltantes en estructura de retorno:", 
          paste(campos_faltantes, collapse = ", "), "\n")
    }
    
  }, error = function(e) {
    cat("❌ ERROR EN FUNCIÓN generar_datos():", e$message, "\n")
  })
} else {
  cat("❌ ERROR: No se pudo extraer la función generar_datos()\n")
}

# ===== PRUEBA 3: ESTRATEGIA AVANZADA DE GENERACIÓN =====
cat("\n3. ESTRATEGIA AVANZADA DE GENERACIÓN:\n")
cat("=====================================\n")

if(exists("generar_datos")) {
  # Probar diversidad de pools correctas
  pools_usadas <- c()
  tipos_distractores <- list()
  
  for(i in 1:50) {
    datos <- generar_datos()
    pools_usadas <- c(pools_usadas, datos$pool_correcta_usada)
    tipos_distractores[[i]] <- datos$distractores_tipos
  }
  
  # Verificar diversidad de pools
  pools_unicas <- length(unique(pools_usadas))
  if(pools_unicas >= 2) {
    cat("✅ CORRECTO: Se utilizan múltiples pools de respuestas correctas (", 
        pools_unicas, " diferentes)\n")
  } else {
    cat("⚠️  ADVERTENCIA: Poca diversidad en pools de respuestas correctas\n")
  }
  
  # Verificar tipos de distractores
  todos_tipos <- unlist(tipos_distractores)
  tipos_unicos <- unique(todos_tipos)
  tipos_esperados <- c("dia_correcto_sabores_incorrectos", 
                      "sabores_correctos_dia_incorrecto", 
                      "ambos_incorrectos_plausibles")
  
  if(all(tipos_esperados %in% tipos_unicos)) {
    cat("✅ CORRECTO: Se generan todos los tipos de distractores esperados\n")
  } else {
    tipos_faltantes <- setdiff(tipos_esperados, tipos_unicos)
    cat("⚠️  ADVERTENCIA: Tipos de distractores faltantes:", 
        paste(tipos_faltantes, collapse = ", "), "\n")
  }
  
  # Verificar validación de unicidad
  intentos_promedio <- mean(sapply(1:20, function(i) {
    datos <- generar_datos()
    datos$validacion_unicidad$intentos_realizados
  }))
  
  if(intentos_promedio < 30) {
    cat("✅ CORRECTO: Algoritmo de unicidad eficiente (promedio:", 
        round(intentos_promedio, 1), "intentos)\n")
  } else {
    cat("⚠️  ADVERTENCIA: Algoritmo de unicidad poco eficiente (promedio:", 
        round(intentos_promedio, 1), "intentos)\n")
  }
}

# ===== PRUEBA 4: CHUNKS R Y CONFIGURACIÓN =====
cat("\n4. CHUNKS R Y CONFIGURACIÓN:\n")
cat("============================\n")

# Verificar chunks críticos
chunks_criticos <- c("setup", "generar_datos", "crear_tabla_tikz", 
                    "tabla_preferencias", "grafico_ventas", "opciones")

chunks_encontrados <- c()
for(chunk in chunks_criticos) {
  pattern <- paste0("```\\{r ", chunk)
  if(any(grepl(pattern, source_lines))) {
    chunks_encontrados <- c(chunks_encontrados, chunk)
  }
}

chunks_faltantes <- setdiff(chunks_criticos, chunks_encontrados)
if(length(chunks_faltantes) == 0) {
  cat("✅ CORRECTO: Todos los chunks críticos están presentes\n")
} else {
  cat("❌ ERROR: Chunks faltantes:", paste(chunks_faltantes, collapse = ", "), "\n")
}

# Verificar configuración de chunks
chunks_con_echo_false <- sum(grepl("echo=FALSE", source_lines))
if(chunks_con_echo_false >= 3) {
  cat("✅ CORRECTO: Configuración adecuada de echo=FALSE en chunks de presentación\n")
} else {
  cat("⚠️  ADVERTENCIA: Pocos chunks con echo=FALSE, puede mostrar código innecesario\n")
}

# ===== PRUEBA 5: LIBRERÍAS Y DEPENDENCIAS =====
cat("\n5. LIBRERÍAS Y DEPENDENCIAS:\n")
cat("============================\n")

# Verificar librerías cargadas
librerias_necesarias <- c("exams", "digest", "testthat", "knitr")
librerias_cargadas <- c()

for(lib in librerias_necesarias) {
  if(any(grepl(paste0("library\\(", lib, "\\)"), source_lines))) {
    librerias_cargadas <- c(librerias_cargadas, lib)
  }
}

librerias_faltantes <- setdiff(librerias_necesarias, librerias_cargadas)
if(length(librerias_faltantes) == 0) {
  cat("✅ CORRECTO: Todas las librerías necesarias están cargadas\n")
} else {
  cat("⚠️  ADVERTENCIA: Librerías faltantes:", paste(librerias_faltantes, collapse = ", "), "\n")
}

# ===== PRUEBA 6: CONFIGURACIÓN TIKZ =====
cat("\n6. CONFIGURACIÓN TIKZ:\n")
cat("======================\n")

# Verificar configuración TikZ
tikz_config_lines <- grep("tikz", source_lines, ignore.case = TRUE)
if(length(tikz_config_lines) > 0) {
  cat("✅ CORRECTO: Configuración TikZ presente\n")
  
  # Verificar include_tikz
  if(any(grepl("include_tikz", source_lines))) {
    cat("✅ CORRECTO: Función include_tikz utilizada correctamente\n")
  } else {
    cat("⚠️  ADVERTENCIA: No se encontró uso de include_tikz\n")
  }
} else {
  cat("❌ ERROR: No se encontró configuración TikZ\n")
}

# ===== RESUMEN FINAL =====
cat("\n=== RESUMEN DE CÓDIGO R Y FUNCIONALIDAD ===\n")

# Contar problemas críticos
problemas_criticos <- 0
if(!exists("sintaxis_ok") || !sintaxis_ok) problemas_criticos <- problemas_criticos + 1
if(length(chunks_faltantes) > 0) problemas_criticos <- problemas_criticos + 1
if(!exists("generar_datos")) problemas_criticos <- problemas_criticos + 1

if(problemas_criticos == 0) {
  cat("🎯 EXCELENTE: El código R funciona correctamente\n")
  cat("✅ Sintaxis correcta y compilación exitosa\n")
  cat("✅ Función generar_datos() operativa\n")
  cat("✅ Estrategia avanzada implementada\n")
  cat("✅ Chunks R configurados adecuadamente\n")
  if(length(librerias_faltantes) == 0) {
    cat("✅ Todas las dependencias presentes\n")
  } else {
    cat("⚠️  Algunas librerías faltantes (no crítico)\n")
  }
} else {
  cat("❌ PROBLEMAS CRÍTICOS ENCONTRADOS:", problemas_criticos, "\n")
  cat("🔧 REQUIERE CORRECCIÓN INMEDIATA\n")
}

cat("\nLa auditoría de código R y funcionalidad ha finalizado.\n")
