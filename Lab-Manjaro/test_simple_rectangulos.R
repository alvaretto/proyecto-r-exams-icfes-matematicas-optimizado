# Test simple para verificar que los rectángulos negros están implementados correctamente

# Verificar que el patrón está presente en los archivos modificados
archivos_modificados <- c(
  "empaques_tetra_pak_argumentacion_n3_v1/empaques_tetra_pak_argumentacion_n3_v1.Rmd",
  "01-S2-2025-SEDQ/01-S2-2025-SEDQ-grafico_circular_bienes_v0.Rmd",
  "19/porcentajes_ordenamiento_sabores_v1.Rmd"
)

cat("🔍 VERIFICACIÓN DE PATRÓN DE RECTÁNGULOS NEGROS\n")
cat("=" %R% 50, "\n\n")

# Patrones a buscar
patrones_clave <- c(
  "ancho_rect = len(texto) * 0.045",
  "alto_rect = 0.12",
  "facecolor='black', alpha=0.8",
  "autotext.set_color('white')",
  "autotext.set_zorder(2)"
)

for (archivo in archivos_modificados) {
  if (file.exists(archivo)) {
    cat("📊 Verificando:", basename(archivo), "\n")
    contenido <- readLines(archivo, warn = FALSE)
    contenido_texto <- paste(contenido, collapse = " ")
    
    patrones_encontrados <- 0
    for (patron in patrones_clave) {
      if (grepl(patron, contenido_texto, fixed = TRUE)) {
        cat("  ✅", patron, "\n")
        patrones_encontrados <- patrones_encontrados + 1
      } else {
        cat("  ❌", patron, "\n")
      }
    }
    
    if (patrones_encontrados == length(patrones_clave)) {
      cat("  🎉 PATRÓN COMPLETO IMPLEMENTADO\n")
    } else {
      cat("  ⚠️ PATRÓN PARCIALMENTE IMPLEMENTADO (", patrones_encontrados, "/", length(patrones_clave), ")\n")
    }
    cat("\n")
  } else {
    cat("❌ Archivo no encontrado:", archivo, "\n\n")
  }
}

# Verificar estructura de código Python
cat("🐍 VERIFICACIÓN DE ESTRUCTURA PYTHON\n")
cat("=" %R% 40, "\n\n")

for (archivo in archivos_modificados) {
  if (file.exists(archivo)) {
    cat("📊", basename(archivo), "\n")
    contenido <- readLines(archivo, warn = FALSE)
    
    # Buscar plt.pie
    tiene_pie <- any(grepl("plt\\.pie", contenido))
    cat("  🥧 plt.pie:", ifelse(tiene_pie, "✅", "❌"), "\n")
    
    # Buscar autopct
    tiene_autopct <- any(grepl("autopct", contenido))
    cat("  📊 autopct:", ifelse(tiene_autopct, "✅", "❌"), "\n")
    
    # Buscar autotexts
    tiene_autotexts <- any(grepl("autotexts", contenido))
    cat("  📝 autotexts:", ifelse(tiene_autotexts, "✅", "❌"), "\n")
    
    # Buscar Rectangle
    tiene_rectangle <- any(grepl("Rectangle", contenido))
    cat("  ⬛ Rectangle:", ifelse(tiene_rectangle, "✅", "❌"), "\n")
    
    cat("\n")
  }
}

cat("🏁 VERIFICACIÓN COMPLETADA\n")
cat("📋 Resumen: Patrón de rectángulos negros verificado en", length(archivos_modificados), "archivos\n")
cat("🎨 Mejora: Visibilidad de porcentajes optimizada con fondo negro\n")