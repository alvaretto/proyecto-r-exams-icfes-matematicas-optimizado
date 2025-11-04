################################################################################
# SCRIPT DE VERIFICACIÓN DE REQUISITOS
# Verifica que todos los archivos y dependencias necesarias estén disponibles
# antes de ejecutar SemilleroFinDePeriodo4.R
################################################################################

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════════════╗\n")
cat("║           VERIFICACIÓN DE REQUISITOS - GENERACIÓN DE EXÁMENES              ║\n")
cat("╚════════════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# Contador de errores
errores <- 0
advertencias <- 0

################################################################################
# 1. VERIFICAR PAQUETE EXAMS
################################################################################

cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("1️⃣  Verificando paquete 'exams'...\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

if (requireNamespace("exams", quietly = TRUE)) {
  cat("   ✅ Paquete 'exams' instalado correctamente\n")
  cat("   📦 Versión:", as.character(packageVersion("exams")), "\n")
} else {
  cat("   ❌ ERROR: Paquete 'exams' NO está instalado\n")
  cat("   💡 Solución: Ejecuta install.packages('exams')\n")
  errores <- errores + 1
}
cat("\n")

################################################################################
# 2. VERIFICAR ARCHIVOS .RMD (001-015)
################################################################################

cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("2️⃣  Verificando archivos .Rmd (001-015)...\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

archivos_requeridos <- c(
  "001-muestreo_sesgo_municipio_aleatorio_argumentacion_n2_v1.Rmd",
  "002-cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_2.Rmd",
  "003-cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_1.Rmd",
  "004-pasteleria_sabores_ventas_estadistica_interpretacion_representacion_n2_v1.Rmd",
  "005-estadistica_media_calificaciones_n2_v1.Rmd",
  "006-ganancias_comerciales_formulacion_ejecucion_n2_v1.Rmd",
  "007-proporcionalidad_empresarial_formulacion_ejecucion_n2_v1.Rmd",
  "008-funciones_lineales_interpretacion_grafica_v2.Rmd",
  "009-funciones_lineales_interpretacion_grafica_v1.Rmd",
  "010-empaques_tetra_pak_argumentacion_n3_v1.Rmd",
  "011-probabilidad_extraccion_bolas_v1.Rmd",
  "012-probabilidad_combinaciones_v1.Rmd",
  "013-parabrisas.Rmd",
  "014-parabrisas-2.Rmd",
  "015-volumen_cilindro_hueco_R_v1.Rmd"
)

archivos_faltantes <- c()
for (archivo in archivos_requeridos) {
  if (file.exists(archivo)) {
    cat("   ✅", archivo, "\n")
  } else {
    cat("   ❌", archivo, "(FALTANTE)\n")
    archivos_faltantes <- c(archivos_faltantes, archivo)
    errores <- errores + 1
  }
}

if (length(archivos_faltantes) == 0) {
  cat("\n   ✅ Todos los archivos .Rmd están disponibles (15/15)\n")
} else {
  cat("\n   ❌ Faltan", length(archivos_faltantes), "archivos .Rmd\n")
}
cat("\n")

################################################################################
# 3. VERIFICAR TEMPLATES LATEX
################################################################################

cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("3️⃣  Verificando templates LaTeX...\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

templates_requeridos <- c(
  "pcielo.tex",
  "pcielo_nosol.tex",
  "solpcielo.tex",
  "exam.tex"
)

templates_faltantes <- c()
for (template in templates_requeridos) {
  if (file.exists(template)) {
    cat("   ✅", template, "\n")
  } else {
    cat("   ❌", template, "(FALTANTE)\n")
    templates_faltantes <- c(templates_faltantes, template)
    errores <- errores + 1
  }
}

if (length(templates_faltantes) == 0) {
  cat("\n   ✅ Todos los templates LaTeX están disponibles (4/4)\n")
} else {
  cat("\n   ❌ Faltan", length(templates_faltantes), "templates\n")
}
cat("\n")

################################################################################
# 4. VERIFICAR LATEX
################################################################################

cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("4️⃣  Verificando instalación de LaTeX...\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

# Verificar si pdflatex está disponible
latex_disponible <- system("which pdflatex", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0

if (latex_disponible) {
  cat("   ✅ LaTeX (pdflatex) está instalado y disponible\n")
} else {
  cat("   ⚠️  ADVERTENCIA: LaTeX (pdflatex) no se encuentra en el PATH\n")
  cat("   💡 Esto puede causar problemas al generar PDFs\n")
  cat("   💡 Solución: Instala texlive-full (Linux) o MiKTeX (Windows)\n")
  advertencias <- advertencias + 1
}
cat("\n")

################################################################################
# 5. VERIFICAR DIRECTORIO DE SALIDA
################################################################################

cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("5️⃣  Verificando directorio de salida...\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

if (dir.exists("salida")) {
  cat("   ✅ Directorio 'salida' existe\n")
} else {
  cat("   ℹ️  Directorio 'salida' no existe (se creará automáticamente)\n")
}
cat("\n")

################################################################################
# RESUMEN FINAL
################################################################################

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════════════╗\n")
cat("║                         RESUMEN DE VERIFICACIÓN                            ║\n")
cat("╚════════════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

if (errores == 0 && advertencias == 0) {
  cat("   ✅ ¡TODO LISTO! No se encontraron errores ni advertencias\n")
  cat("   🚀 Puedes ejecutar SemilleroFinDePeriodo4.R sin problemas\n")
  cat("\n")
  cat("   Para ejecutar el script:\n")
  cat("   > source('SemilleroFinDePeriodo4.R')\n")
} else {
  if (errores > 0) {
    cat("   ❌ Se encontraron", errores, "error(es) crítico(s)\n")
    cat("   ⚠️  Debes corregir estos errores antes de ejecutar el script\n")
  }
  if (advertencias > 0) {
    cat("   ⚠️  Se encontraron", advertencias, "advertencia(s)\n")
    cat("   ℹ️  El script puede ejecutarse, pero algunos formatos pueden fallar\n")
  }
}

cat("\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("\n")

