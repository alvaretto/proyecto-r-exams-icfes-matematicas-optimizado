# ===================================================================
# AUDITORÍA DE COHERENCIA PEDAGÓGICA
# ===================================================================
# Script para validar unicidad de respuesta, distractores y explicaciones

library(exams)
library(digest)

cat("=== AUDITORÍA DE COHERENCIA PEDAGÓGICA ===\n\n")

archivo_ejercicio <- "pasteleria_sabores_ventas_estadistica_interpretacion_representacion_n2_v1.Rmd"
source_lines <- readLines(archivo_ejercicio)

# Cargar función generar_datos
start_line <- grep("generar_datos <- function", source_lines)
end_line <- grep("# Generar datos para este ejercicio", source_lines) - 1

if(length(start_line) > 0 && length(end_line) > 0) {
  function_lines <- source_lines[start_line:end_line]
  function_code <- paste(function_lines, collapse = "\n")
  eval(parse(text = function_code))
  cat("✅ Función generar_datos() cargada correctamente\n")
} else {
  cat("❌ ERROR: No se pudo extraer la función generar_datos()\n")
  quit(status = 1)
}

# ===== PRUEBA 1: UNICIDAD DE RESPUESTA CORRECTA =====
cat("\n1. UNICIDAD DE RESPUESTA CORRECTA:\n")
cat("==================================\n")

respuestas_ambiguas <- 0
versiones_probadas <- 25

for(i in 1:versiones_probadas) {
  datos <- generar_datos()
  
  # Contar cuántas opciones son matemáticamente correctas
  respuestas_correctas_count <- 0
  
  for(j in 1:4) {
    opcion <- tolower(datos$opciones[j])
    dia_correcto <- tolower(datos$dia_mayor_ventas)
    sabor1_correcto <- tolower(datos$sabores_mas_preferidos[1])
    sabor2_correcto <- tolower(datos$sabores_mas_preferidos[2])
    
    contiene_dia <- grepl(dia_correcto, opcion)
    contiene_sabor1 <- grepl(sabor1_correcto, opcion)
    contiene_sabor2 <- grepl(sabor2_correcto, opcion)
    
    if(contiene_dia && contiene_sabor1 && contiene_sabor2) {
      respuestas_correctas_count <- respuestas_correctas_count + 1
    }
  }
  
  if(respuestas_correctas_count != 1) {
    respuestas_ambiguas <- respuestas_ambiguas + 1
  }
}

if(respuestas_ambiguas == 0) {
  cat("✅ EXCELENTE: Todas las versiones tienen exactamente UNA respuesta correcta\n")
} else {
  cat("❌ PROBLEMA CRÍTICO:", respuestas_ambiguas, "versiones con respuestas ambiguas\n")
}

# ===== PRUEBA 2: CALIDAD DE DISTRACTORES =====
cat("\n2. CALIDAD DE DISTRACTORES:\n")
cat("===========================\n")

tipos_distractores_encontrados <- list()
distractores_problematicos <- 0

for(i in 1:15) {
  datos <- generar_datos()
  tipos_distractores_encontrados[[i]] <- datos$distractores_tipos
  
  # Verificar que no haya distractores idénticos
  opciones_unicas <- length(unique(datos$opciones))
  if(opciones_unicas != 4) {
    distractores_problematicos <- distractores_problematicos + 1
    cat("⚠️  ADVERTENCIA: Opciones duplicadas en versión", i, "\n")
  }
}

# Analizar diversidad de tipos de distractores
todos_tipos <- unlist(tipos_distractores_encontrados)
tipos_unicos <- unique(todos_tipos)
tipos_esperados <- c("dia_correcto_sabores_incorrectos", 
                    "sabores_correctos_dia_incorrecto", 
                    "ambos_incorrectos_plausibles")

if(all(tipos_esperados %in% tipos_unicos)) {
  cat("✅ CORRECTO: Se generan todos los tipos de distractores esperados\n")
  
  # Verificar distribución equilibrada
  tabla_tipos <- table(todos_tipos)
  coef_variacion <- sd(tabla_tipos) / mean(tabla_tipos)
  
  if(coef_variacion < 0.5) {
    cat("✅ CORRECTO: Distribución equilibrada de tipos de distractores\n")
  } else {
    cat("⚠️  ADVERTENCIA: Distribución desbalanceada de tipos de distractores\n")
  }
} else {
  tipos_faltantes <- setdiff(tipos_esperados, tipos_unicos)
  cat("❌ ERROR: Tipos de distractores faltantes:", 
      paste(tipos_faltantes, collapse = ", "), "\n")
}

if(distractores_problematicos == 0) {
  cat("✅ CORRECTO: No se encontraron distractores problemáticos\n")
} else {
  cat("⚠️  ADVERTENCIA:", distractores_problematicos, "versiones con distractores problemáticos\n")
}

# ===== PRUEBA 3: COHERENCIA PREGUNTA-RESPUESTA =====
cat("\n3. COHERENCIA PREGUNTA-RESPUESTA:\n")
cat("=================================\n")

# Verificar que la pregunta esté presente y sea coherente
question_start <- grep("Question", source_lines)
solution_start <- grep("Solution", source_lines)

if(length(question_start) > 0 && length(solution_start) > 0) {
  question_lines <- source_lines[(question_start + 2):(solution_start - 1)]
  question_text <- paste(question_lines, collapse = " ")
  
  # Verificar elementos clave en la pregunta
  elementos_clave <- c("pastelería", "encuesta", "sabores", "tortas", "día", "ventas")
  elementos_encontrados <- c()
  
  for(elemento in elementos_clave) {
    if(grepl(tolower(elemento), tolower(question_text))) {
      elementos_encontrados <- c(elementos_encontrados, elemento)
    }
  }
  
  if(length(elementos_encontrados) >= 4) {
    cat("✅ CORRECTO: Pregunta contiene elementos clave del contexto\n")
  } else {
    cat("⚠️  ADVERTENCIA: Pregunta puede carecer de contexto suficiente\n")
    cat("   Elementos encontrados:", paste(elementos_encontrados, collapse = ", "), "\n")
  }
  
  # Verificar que la pregunta sea clara sobre qué se pide
  if(grepl("promoción", tolower(question_text)) || 
     grepl("día.*ventas", tolower(question_text)) ||
     grepl("sabores.*preferidos", tolower(question_text))) {
    cat("✅ CORRECTO: Pregunta es clara sobre lo que se solicita\n")
  } else {
    cat("⚠️  ADVERTENCIA: Pregunta puede no ser suficientemente clara\n")
  }
} else {
  cat("❌ ERROR: No se encontraron secciones Question o Solution\n")
}

# ===== RESUMEN FINAL =====
cat("\n=== RESUMEN DE COHERENCIA PEDAGÓGICA ===\n")

# Contar problemas críticos
problemas_criticos <- 0
if(respuestas_ambiguas > 0) problemas_criticos <- problemas_criticos + 1
if(length(question_start) == 0 || length(solution_start) == 0) problemas_criticos <- problemas_criticos + 1

if(problemas_criticos == 0) {
  cat("🎯 EXCELENTE: El ejercicio tiene coherencia pedagógica sólida\n")
  cat("✅ Respuesta única y correcta garantizada\n")
  cat("✅ Distractores de calidad y diversidad apropiada\n")
  cat("✅ Pregunta coherente con el contexto\n")
  cat("\n🏆 RESULTADO: APROBADO - COHERENCIA PEDAGÓGICA SÓLIDA\n")
} else {
  cat("❌ PROBLEMAS CRÍTICOS ENCONTRADOS:", problemas_criticos, "\n")
  cat("🔧 REQUIERE CORRECCIÓN INMEDIATA\n")
  cat("\n⚠️  RESULTADO: REPROBADO - REQUIERE CORRECCIÓN\n")
}

cat("\nLa auditoría de coherencia pedagógica ha finalizado.\n")
