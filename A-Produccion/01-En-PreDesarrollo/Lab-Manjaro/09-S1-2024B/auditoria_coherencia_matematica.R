# ===================================================================
# AUDITORÍA DE COHERENCIA MATEMÁTICA - EJERCICIO ESTADÍSTICA PASTELERÍA
# ===================================================================
# Script para validar cálculos, respuesta correcta única y consistencia de datos

library(exams)
library(digest)

cat("=== AUDITORÍA DE COHERENCIA MATEMÁTICA ===\n\n")

# Cargar la función de generación de datos del ejercicio
source_lines <- readLines("pasteleria_sabores_ventas_estadistica_interpretacion_representacion_n2_v1.Rmd")

# Extraer y evaluar solo la función generar_datos
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

# ===== PRUEBA 1: CONSISTENCIA DE DATOS =====
cat("1. CONSISTENCIA DE DATOS:\n")
cat("========================\n")

errores_encontrados <- 0

for(i in 1:20) {
  datos <- generar_datos()
  
  # Verificar que los sabores más preferidos sean correctos
  preferencias_ordenadas <- sort(datos$preferencias, decreasing = TRUE)
  sabores_correctos <- names(preferencias_ordenadas)[1:2]
  
  if(!all(datos$sabores_mas_preferidos %in% sabores_correctos)) {
    cat("❌ ERROR: Sabores más preferidos incorrectos en versión", i, "\n")
    cat("   Calculados:", paste(sabores_correctos, collapse = ", "), "\n")
    cat("   Almacenados:", paste(datos$sabores_mas_preferidos, collapse = ", "), "\n")
    errores_encontrados <- errores_encontrados + 1
  }
  
  # Verificar que el día con mayor ventas sea correcto
  ventas_ordenadas <- sort(datos$ventas_por_dia, decreasing = TRUE)
  dia_correcto <- names(ventas_ordenadas)[1]
  
  if(datos$dia_mayor_ventas != dia_correcto) {
    cat("❌ ERROR: Día con mayor ventas incorrecto en versión", i, "\n")
    cat("   Calculado:", dia_correcto, "\n")
    cat("   Almacenado:", datos$dia_mayor_ventas, "\n")
    errores_encontrados <- errores_encontrados + 1
  }
}

if(errores_encontrados == 0) {
  cat("✅ CORRECTO: Todos los cálculos son consistentes (20 versiones probadas)\n")
} else {
  cat("❌ PROBLEMA:", errores_encontrados, "errores de consistencia encontrados\n")
}

# ===== PRUEBA 2: UNICIDAD DE RESPUESTA CORRECTA =====
cat("\n2. UNICIDAD DE RESPUESTA CORRECTA:\n")
cat("==================================\n")

respuestas_ambiguas <- 0

for(i in 1:20) {
  datos <- generar_datos()
  
  # Verificar que solo una opción sea matemáticamente correcta
  respuestas_correctas_count <- 0
  
  for(j in 1:4) {
    opcion <- datos$opciones[j]
    
    # Extraer día y sabores de la opción
    opcion_lower <- tolower(opcion)
    
    # Verificar si contiene el día correcto
    dia_correcto <- tolower(datos$dia_mayor_ventas)
    contiene_dia_correcto <- grepl(dia_correcto, opcion_lower)
    
    # Verificar si contiene los sabores correctos
    sabor1_correcto <- tolower(datos$sabores_mas_preferidos[1])
    sabor2_correcto <- tolower(datos$sabores_mas_preferidos[2])
    
    contiene_sabores_correctos <- grepl(sabor1_correcto, opcion_lower) && 
                                  grepl(sabor2_correcto, opcion_lower)
    
    if(contiene_dia_correcto && contiene_sabores_correctos) {
      respuestas_correctas_count <- respuestas_correctas_count + 1
    }
  }
  
  if(respuestas_correctas_count != 1) {
    cat("❌ ERROR: Respuesta ambigua en versión", i, "\n")
    cat("   Respuestas matemáticamente correctas:", respuestas_correctas_count, "\n")
    cat("   Día correcto:", datos$dia_mayor_ventas, "\n")
    cat("   Sabores correctos:", paste(datos$sabores_mas_preferidos, collapse = ", "), "\n")
    cat("   Opciones:\n")
    for(k in 1:4) {
      cat("     ", LETTERS[k], ":", datos$opciones[k], "\n")
    }
    respuestas_ambiguas <- respuestas_ambiguas + 1
  }
}

if(respuestas_ambiguas == 0) {
  cat("✅ CORRECTO: Todas las versiones tienen exactamente una respuesta correcta\n")
} else {
  cat("❌ PROBLEMA:", respuestas_ambiguas, "versiones con respuestas ambiguas\n")
}

# ===== PRUEBA 3: VALIDACIÓN DE POSICIÓN CORRECTA =====
cat("\n3. VALIDACIÓN DE POSICIÓN CORRECTA:\n")
cat("===================================\n")

posiciones_incorrectas <- 0

for(i in 1:20) {
  datos <- generar_datos()
  
  # Verificar que la posición correcta apunte a la respuesta correcta
  opcion_marcada_correcta <- datos$opciones[datos$posicion_correcta]
  
  # Verificar si esta opción contiene los datos correctos
  opcion_lower <- tolower(opcion_marcada_correcta)
  dia_correcto <- tolower(datos$dia_mayor_ventas)
  sabor1_correcto <- tolower(datos$sabores_mas_preferidos[1])
  sabor2_correcto <- tolower(datos$sabores_mas_preferidos[2])
  
  contiene_dia_correcto <- grepl(dia_correcto, opcion_lower)
  contiene_sabores_correctos <- grepl(sabor1_correcto, opcion_lower) && 
                                grepl(sabor2_correcto, opcion_lower)
  
  if(!(contiene_dia_correcto && contiene_sabores_correctos)) {
    cat("❌ ERROR: Posición correcta incorrecta en versión", i, "\n")
    cat("   Posición marcada:", datos$posicion_correcta, "\n")
    cat("   Opción marcada:", opcion_marcada_correcta, "\n")
    cat("   Día correcto:", datos$dia_mayor_ventas, "\n")
    cat("   Sabores correctos:", paste(datos$sabores_mas_preferidos, collapse = ", "), "\n")
    posiciones_incorrectas <- posiciones_incorrectas + 1
  }
}

if(posiciones_incorrectas == 0) {
  cat("✅ CORRECTO: Todas las posiciones correctas apuntan a la respuesta correcta\n")
} else {
  cat("❌ PROBLEMA:", posiciones_incorrectas, "posiciones incorrectas encontradas\n")
}

# ===== PRUEBA 4: VALIDACIÓN DE DISTRACTORES =====
cat("\n4. VALIDACIÓN DE DISTRACTORES:\n")
cat("==============================\n")

distractores_invalidos <- 0

for(i in 1:10) {  # Menos iteraciones para esta prueba más compleja
  datos <- generar_datos()
  
  for(j in 1:4) {
    if(j != datos$posicion_correcta) {
      opcion <- datos$opciones[j]
      tipo_esperado <- datos$tipos_opciones[j]
      
      # Verificar que el distractor corresponda a su tipo
      opcion_lower <- tolower(opcion)
      dia_correcto <- tolower(datos$dia_mayor_ventas)
      sabor1_correcto <- tolower(datos$sabores_mas_preferidos[1])
      sabor2_correcto <- tolower(datos$sabores_mas_preferidos[2])
      
      contiene_dia_correcto <- grepl(dia_correcto, opcion_lower)
      contiene_sabor1 <- grepl(sabor1_correcto, opcion_lower)
      contiene_sabor2 <- grepl(sabor2_correcto, opcion_lower)
      contiene_ambos_sabores <- contiene_sabor1 && contiene_sabor2
      
      tipo_real <- "desconocido"
      
      if(contiene_dia_correcto && !contiene_ambos_sabores) {
        tipo_real <- "dia_correcto_sabores_incorrectos"
      } else if(!contiene_dia_correcto && contiene_ambos_sabores) {
        tipo_real <- "sabores_correctos_dia_incorrecto"
      } else if(!contiene_dia_correcto && !contiene_ambos_sabores) {
        tipo_real <- "ambos_incorrectos_plausibles"
      }
      
      if(tipo_esperado != tipo_real && tipo_real != "desconocido") {
        cat("⚠️  ADVERTENCIA: Tipo de distractor inconsistente en versión", i, "opción", LETTERS[j], "\n")
        cat("   Tipo esperado:", tipo_esperado, "\n")
        cat("   Tipo real:", tipo_real, "\n")
        cat("   Opción:", opcion, "\n")
        distractores_invalidos <- distractores_invalidos + 1
      }
    }
  }
}

if(distractores_invalidos == 0) {
  cat("✅ CORRECTO: Todos los distractores corresponden a su tipo declarado\n")
} else {
  cat("⚠️  ADVERTENCIA:", distractores_invalidos, "distractores con tipos inconsistentes\n")
}

# ===== RESUMEN FINAL =====
cat("\n=== RESUMEN DE COHERENCIA MATEMÁTICA ===\n")

total_errores <- errores_encontrados + respuestas_ambiguas + posiciones_incorrectas

if(total_errores == 0) {
  cat("🎯 EXCELENTE: El ejercicio tiene coherencia matemática perfecta\n")
  cat("✅ Cálculos consistentes\n")
  cat("✅ Respuesta única y correcta\n")
  cat("✅ Posiciones correctas válidas\n")
  if(distractores_invalidos == 0) {
    cat("✅ Distractores pedagógicamente válidos\n")
  } else {
    cat("⚠️  Algunos distractores con tipos inconsistentes (no crítico)\n")
  }
} else {
  cat("❌ PROBLEMAS CRÍTICOS ENCONTRADOS:", total_errores, "\n")
  cat("🔧 REQUIERE CORRECCIÓN INMEDIATA\n")
}

cat("\nLa auditoría de coherencia matemática ha finalizado.\n")
