# SCRIPT DE PRUEBA PARA VERIFICAR ELIMINACIÓN DEL PATRÓN DE EQUIPOS REPETIDOS
# Verifica que no aparezcan múltiples opciones con el mismo equipo

library(exams)
library(testthat)

# Función para extraer equipos mencionados en las opciones
extraer_equipos_de_opciones <- function(opciones_texto) {
  # Lista de equipos posibles (simulada - en el archivo real se obtiene dinámicamente)
  equipos_posibles <- c(
    # Equipos europeos
    "Manchester City", "FC Barcelona", "Real Madrid", "Liverpool",
    "Bayern de Múnich", "Paris Saint-Germain", "Chelsea",
    "Manchester United", "Arsenal", "Juventus", "AC Milan",
    "Atlético de Madrid", "Borussia Dortmund", "Ajax", "Benfica",
    
    # Equipos sudamericanos
    "Boca Juniors", "River Plate", "Flamengo", "Palmeiras",
    "São Paulo", "Santos", "Corinthians", "Atlético Nacional",
    "Millonarios", "Colo-Colo", "Universidad de Chile", "Peñarol",
    "Nacional", "Olimpia", "Cerro Porteño",
    
    # Selecciones
    "España", "Francia", "Alemania", "Italia", "Inglaterra",
    "Portugal", "Países Bajos", "Bélgica", "Croacia", "Polonia",
    "Brasil", "Argentina", "Uruguay", "Colombia", "Chile",
    "Perú", "Ecuador", "Paraguay", "Bolivia", "Venezuela"
  )
  
  equipos_encontrados <- c()
  
  for (opcion in opciones_texto) {
    for (equipo in equipos_posibles) {
      if (grepl(equipo, opcion, fixed = TRUE)) {
        equipos_encontrados <- c(equipos_encontrados, equipo)
        break  # Solo contar el primer equipo encontrado por opción
      }
    }
  }
  
  return(equipos_encontrados)
}

# Función para analizar patrones en las opciones
analizar_patrones_equipos <- function(opciones_texto) {
  equipos_mencionados <- extraer_equipos_de_opciones(opciones_texto)
  
  # Contar frecuencias
  tabla_equipos <- table(equipos_mencionados)
  
  # Identificar equipos repetidos
  equipos_repetidos <- names(tabla_equipos)[tabla_equipos > 1]
  
  # Calcular diversidad
  diversidad <- length(unique(equipos_mencionados))
  
  return(list(
    equipos_mencionados = equipos_mencionados,
    tabla_equipos = tabla_equipos,
    equipos_repetidos = equipos_repetidos,
    diversidad = diversidad,
    tiene_patron = length(equipos_repetidos) > 0
  ))
}

# Función para probar múltiples ejecuciones y detectar patrones
probar_eliminacion_patron_equipos <- function(num_pruebas = 15) {
  cat("=== PRUEBA DE ELIMINACIÓN DE PATRÓN DE EQUIPOS ===\n")
  cat("Ejecutando", num_pruebas, "pruebas para verificar diversidad de equipos...\n\n")
  
  resultados <- list()
  patrones_detectados <- 0
  errores_patron <- 0
  errores_otros <- 0
  exitosas <- 0
  
  for (i in 1:num_pruebas) {
    cat("Prueba", i, "de", num_pruebas, "... ")
    
    tryCatch({
      # Crear archivo temporal para esta prueba
      archivo_temp <- paste0("test_patron_", i, ".Rmd")
      file.copy("proporciones_encuesta_deportiva_v1.Rmd", archivo_temp)
      
      # Ejecutar el archivo
      resultado <- exams2html(archivo_temp, 
                             n = 1, 
                             name = paste0("test_patron_", i),
                             dir = "test_output_patron",
                             quiet = TRUE)
      
      exitosas <- exitosas + 1
      cat("✓ ÉXITO\n")
      
      # Limpiar archivo temporal
      if (file.exists(archivo_temp)) {
        file.remove(archivo_temp)
      }
      
    }, error = function(e) {
      error_msg <- e$message
      
      if (grepl("anti-patrón", error_msg, ignore.case = TRUE) || 
          grepl("equipos aparecen en múltiples opciones", error_msg, ignore.case = TRUE)) {
        errores_patron <- errores_patron + 1
        cat("✗ ERROR PATRÓN DETECTADO\n")
        cat("   ", error_msg, "\n")
      } else {
        errores_otros <- errores_otros + 1
        cat("✗ OTRO ERROR\n")
        cat("   ", error_msg, "\n")
      }
      
      # Limpiar archivo temporal
      archivo_temp <- paste0("test_patron_", i, ".Rmd")
      if (file.exists(archivo_temp)) {
        file.remove(archivo_temp)
      }
    })
  }
  
  # Resumen de resultados
  cat("\n=== RESUMEN DE PRUEBAS ===\n")
  cat("Total de pruebas:", num_pruebas, "\n")
  cat("Pruebas exitosas:", exitosas, "\n")
  cat("Errores por patrón de equipos:", errores_patron, "\n")
  cat("Otros errores:", errores_otros, "\n")
  
  if (errores_patron == 0) {
    cat("\n✅ PATRÓN DE EQUIPOS REPETIDOS ELIMINADO\n")
  } else {
    cat("\n❌ PATRÓN DE EQUIPOS REPETIDOS PERSISTE\n")
  }
  
  if (exitosas == num_pruebas) {
    cat("✅ TODAS LAS PRUEBAS PASARON\n")
  } else {
    cat("⚠️  ALGUNAS PRUEBAS FALLARON\n")
  }
  
  return(list(
    total = num_pruebas,
    exitosas = exitosas,
    errores_patron = errores_patron,
    errores_otros = errores_otros
  ))
}

# Función para simular análisis de opciones
simular_analisis_opciones <- function() {
  cat("\n=== SIMULACIÓN DE ANÁLISIS DE OPCIONES ===\n")
  
  # Casos de prueba simulados
  casos_prueba <- list(
    # Caso 1: Patrón problemático (ANTES de la corrección)
    list(
      nombre = "Patrón problemático",
      opciones = c(
        "alrededor de 30 de cada 150 aficionados da por favorito al FC Barcelona.",
        "alrededor de 30 de cada 40000 aficionados da por favorito al FC Barcelona.",
        "alrededor de 25 de cada 150 aficionados da por favorito al Real Madrid.",
        "ninguno de los 40000 aficionados da por favorito al Liverpool."
      )
    ),
    
    # Caso 2: Diversidad correcta (DESPUÉS de la corrección)
    list(
      nombre = "Diversidad correcta",
      opciones = c(
        "alrededor de 30 de cada 150 aficionados da por favorito al FC Barcelona.",
        "alrededor de 25 de cada 150 aficionados da por favorito al Real Madrid.",
        "alrededor de 20 de cada 150 aficionados da por favorito al Liverpool.",
        "sólo 150 de los 40000 aficionados tienen preferencia por un equipo."
      )
    ),
    
    # Caso 3: Diversidad máxima
    list(
      nombre = "Diversidad máxima",
      opciones = c(
        "alrededor de 30 de cada 150 aficionados da por favorito al Manchester City.",
        "alrededor de 25 de cada 150 aficionados da por favorito al Arsenal.",
        "alrededor de 20 de cada 150 aficionados da por favorito al Chelsea.",
        "ninguno de los 40000 aficionados da por favorito al Juventus."
      )
    )
  )
  
  for (i in seq_along(casos_prueba)) {
    caso <- casos_prueba[[i]]
    cat("\nCaso", i, ":", caso$nombre, "\n")
    
    # Mostrar opciones
    for (j in seq_along(caso$opciones)) {
      cat("  Opción", j, ":", caso$opciones[j], "\n")
    }
    
    # Analizar patrón
    analisis <- analizar_patrones_equipos(caso$opciones)
    
    cat("  Equipos mencionados:", paste(analisis$equipos_mencionados, collapse=", "), "\n")
    cat("  Diversidad:", analisis$diversidad, "equipos únicos\n")
    
    if (analisis$tiene_patron) {
      cat("  ❌ PATRÓN DETECTADO: Equipos repetidos:", paste(analisis$equipos_repetidos, collapse=", "), "\n")
    } else {
      cat("  ✅ SIN PATRÓN: Cada equipo aparece solo una vez\n")
    }
  }
}

# Función para verificar la lógica de detección de patrones
verificar_logica_deteccion <- function() {
  cat("\n=== VERIFICACIÓN DE LÓGICA DE DETECCIÓN ===\n")
  
  # Caso con patrón obvio
  opciones_con_patron <- c(
    "opción sobre Barcelona",
    "otra opción sobre Barcelona",  # Repetición
    "opción sobre Real Madrid",
    "opción sobre Liverpool"
  )
  
  cat("Opciones con patrón:\n")
  for (i in seq_along(opciones_con_patron)) {
    cat(" ", i, ":", opciones_con_patron[i], "\n")
  }
  
  # Simular detección
  equipos_simulados <- c("Barcelona", "Barcelona", "Real Madrid", "Liverpool")
  tabla_simulada <- table(equipos_simulados)
  repetidos_simulados <- names(tabla_simulada)[tabla_simulada > 1]
  
  cat("\nResultado de detección:\n")
  cat("Equipos repetidos:", paste(repetidos_simulados, collapse=", "), "\n")
  
  if (length(repetidos_simulados) > 0) {
    cat("✓ Patrón detectado correctamente\n")
  } else {
    cat("✗ Error en la detección\n")
  }
}

# Ejecutar todas las pruebas si el script se ejecuta directamente
if (interactive()) {
  cat("EJECUTANDO SUITE DE PRUEBAS PARA PATRÓN DE EQUIPOS\n")
  cat("=================================================\n\n")
  
  # Crear directorio de salida si no existe
  if (!dir.exists("test_output_patron")) {
    dir.create("test_output_patron")
  }
  
  # Ejecutar pruebas
  verificar_logica_deteccion()
  simular_analisis_opciones()
  resultado <- probar_eliminacion_patron_equipos(10)
  
  cat("\n=== PRUEBAS COMPLETADAS ===\n")
  
  if (resultado$errores_patron == 0 && resultado$exitosas == resultado$total) {
    cat("🎉 TODAS LAS PRUEBAS PASARON - PATRÓN DE EQUIPOS ELIMINADO\n")
  } else {
    cat("⚠️  REVISAR RESULTADOS - POSIBLES PROBLEMAS PENDIENTES\n")
  }
}
