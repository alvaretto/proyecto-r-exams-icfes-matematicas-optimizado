# 📁 TUTORIAL: CORRECCIÓN DE ARCHIVOS .RMD EN CARPETA ESPECÍFICA

## 🎯 Objetivo
Aplicar correcciones semánticas y gramaticales automáticas a todos los archivos .Rmd de una carpeta específica.

## 📋 Prerrequisitos
- Sistema de corrección global instalado en `Auxiliares/sistema_correccion_global/`
- R con librerías: `stringr`, `testthat`
- Permisos de escritura en la carpeta objetivo

## 🚀 MÉTODO 1: CORRECCIÓN AUTOMÁTICA COMPLETA

### Paso 1: Cargar el Sistema
```r
# Abrir R o RStudio en el directorio raíz del proyecto
setwd("/ruta/a/proyecto-r-exams-icfes-matematicas-optimizado")

# Cargar el aplicador automático (incluye todas las funciones)
source("Auxiliares/sistema_correccion_global/aplicador_automatico.R")
```

### Paso 2: Validar Estado Inicial
```r
# Validar archivos en la carpeta específica
carpeta_objetivo <- "Lab/10-S1-2025-SEDQ"  # Cambiar por tu carpeta

# Ver qué errores existen actualmente
archivos_con_errores <- validar_archivos_masivo(carpeta_objetivo)

# Ver estadísticas de la carpeta
source("Auxiliares/sistema_correccion_global/validador_masivo.R")
stats_inicial <- generar_estadisticas_calidad(carpeta_objetivo)
```

### Paso 3: Aplicar Correcciones
```r
# Aplicar correcciones automáticas con backup
archivos_procesados <- aplicar_correcciones_masivas(
  directorio = carpeta_objetivo,
  recursivo = TRUE  # Incluir subcarpetas
)
```

### Paso 4: Verificar Resultados
```r
# Validar estado después de correcciones
archivos_con_errores_final <- validar_archivos_masivo(carpeta_objetivo)
stats_final <- generar_estadisticas_calidad(carpeta_objetivo)

# Comparar antes y después
cat("📊 COMPARACIÓN DE RESULTADOS:\n")
cat("Antes - Archivos con errores:", length(archivos_con_errores), "\n")
cat("Después - Archivos con errores:", length(archivos_con_errores_final), "\n")
```

## 🔍 MÉTODO 2: CORRECCIÓN ARCHIVO POR ARCHIVO

### Para Mayor Control
```r
# Cargar sistema
source("Auxiliares/sistema_correccion_global/aplicador_automatico.R")

# Listar archivos .Rmd en la carpeta
carpeta <- "Lab/mi-carpeta"
archivos <- list.files(carpeta, pattern = "\\.Rmd$", full.names = TRUE)

# Procesar cada archivo individualmente
for (archivo in archivos) {
  cat("🔧 Procesando:", basename(archivo), "\n")
  
  # Aplicar correcciones con backup automático
  resultado <- aplicar_correcciones_a_archivo(archivo, crear_backup = TRUE)
  
  if (resultado) {
    cat("✅ Correcciones aplicadas\n")
  } else {
    cat("ℹ️  Sin cambios necesarios\n")
  }
  cat("\n")
}
```

## 📊 MÉTODO 3: VALIDACIÓN DETALLADA PRIMERO

### Paso 1: Análisis Completo
```r
source("Auxiliares/sistema_correccion_global/validador_masivo.R")

# Generar reporte detallado de la carpeta
carpeta <- "Lab/mi-carpeta"
resultados <- generar_reporte_validacion(carpeta, generar_archivo = TRUE)

# Esto creará un archivo: reporte_validacion_YYYYMMDD_HHMMSS.txt
```

### Paso 2: Revisar Reporte
El reporte incluirá:
- Archivos con errores de concordancia
- Archivos sin sistema de corrección
- Archivos sin metadatos ICFES
- Librerías faltantes

### Paso 3: Aplicar Correcciones Selectivas
```r
# Solo a archivos con errores específicos
if (length(resultados) > 0) {
  for (archivo in names(resultados)) {
    errores <- resultados[[archivo]]
    
    if ("concordancia" %in% names(errores)) {
      cat("🔧 Corrigiendo concordancia en:", basename(archivo), "\n")
      aplicar_correcciones_a_archivo(archivo)
    }
  }
}
```

## 🎯 EJEMPLOS PRÁCTICOS

### Ejemplo 1: Carpeta Lab/10-S1-2025-SEDQ
```r
# Comando completo para esta carpeta específica
source("Auxiliares/sistema_correccion_global/aplicador_automatico.R")

# Validar primero
cat("📊 Estado inicial:\n")
validar_archivos_masivo("Lab/10-S1-2025-SEDQ")

# Aplicar correcciones
cat("\n🔧 Aplicando correcciones:\n")
aplicar_correcciones_masivas("Lab/10-S1-2025-SEDQ")

# Validar después
cat("\n📊 Estado final:\n")
validar_archivos_masivo("Lab/10-S1-2025-SEDQ")
```

### Ejemplo 2: Múltiples Carpetas
```r
carpetas <- c("Lab/carpeta1", "Lab/carpeta2", "Lab/carpeta3")

for (carpeta in carpetas) {
  cat("📁 Procesando carpeta:", carpeta, "\n")
  cat("=" %+% rep("=", 40) %+% "\n")
  
  aplicar_correcciones_masivas(carpeta)
  cat("\n")
}
```

### Ejemplo 3: Solo Archivos Específicos
```r
# Corregir solo archivos que contengan "proporciones" en el nombre
carpeta <- "Lab/10-S1-2025-SEDQ"
archivos <- list.files(carpeta, pattern = "proporciones.*\\.Rmd$", full.names = TRUE)

for (archivo in archivos) {
  aplicar_correcciones_a_archivo(archivo)
}
```

## 🔧 OPCIONES AVANZADAS

### Configuración Personalizada
```r
# Aplicar correcciones sin crear backups (no recomendado)
aplicar_correcciones_a_archivo("mi_archivo.Rmd", crear_backup = FALSE)

# Procesar solo nivel actual (sin subcarpetas)
aplicar_correcciones_masivas("Lab/mi-carpeta", recursivo = FALSE)

# Validar con patrón específico
archivos <- list.files("Lab/", pattern = "ejercicio.*\\.Rmd$", 
                      recursive = TRUE, full.names = TRUE)
```

### Manejo de Errores
```r
# Función segura con manejo de errores
procesar_carpeta_seguro <- function(carpeta) {
  tryCatch({
    source("Auxiliares/sistema_correccion_global/aplicador_automatico.R")
    resultado <- aplicar_correcciones_masivas(carpeta)
    cat("✅ Carpeta procesada exitosamente:", carpeta, "\n")
    return(resultado)
  }, error = function(e) {
    cat("❌ Error procesando", carpeta, ":", e$message, "\n")
    return(FALSE)
  })
}

# Usar función segura
procesar_carpeta_seguro("Lab/mi-carpeta")
```

## 📋 CHECKLIST DE VERIFICACIÓN

### Antes de Aplicar Correcciones
- [ ] Backup del proyecto completo
- [ ] Sistema de corrección cargado correctamente
- [ ] Carpeta objetivo existe y tiene archivos .Rmd
- [ ] Permisos de escritura verificados

### Durante el Proceso
- [ ] Monitorear mensajes de error
- [ ] Verificar que se crean backups automáticos
- [ ] Revisar correcciones aplicadas

### Después del Proceso
- [ ] Validar que no hay errores de concordancia
- [ ] Probar compilación de archivos corregidos
- [ ] Verificar que el contenido sigue siendo correcto
- [ ] Documentar cambios realizados

## 🚨 PRECAUCIONES IMPORTANTES

### ⚠️ Siempre Hacer Backup
```r
# El sistema crea backups automáticos, pero también puedes hacer uno manual
carpeta_backup <- paste0("backup_", format(Sys.time(), "%Y%m%d_%H%M%S"))
dir.create(carpeta_backup)
file.copy("Lab/mi-carpeta", carpeta_backup, recursive = TRUE)
```

### ⚠️ Verificar Resultados
```r
# Después de aplicar correcciones, siempre verificar
source("Auxiliares/sistema_correccion_global/validador_masivo.R")
generar_reporte_validacion("Lab/mi-carpeta")
```

### ⚠️ Probar Compilación
```r
# Verificar que los archivos corregidos compilan sin errores
library(knitr)
archivos_corregidos <- list.files("Lab/mi-carpeta", pattern = "\\.Rmd$", full.names = TRUE)

for (archivo in archivos_corregidos) {
  tryCatch({
    purl(archivo, output = tempfile(), quiet = TRUE)
    cat("✅", basename(archivo), "compila correctamente\n")
  }, error = function(e) {
    cat("❌", basename(archivo), "tiene errores de compilación\n")
  })
}
```

## 🎯 COMANDO RÁPIDO COMPLETO

```r
# Comando todo-en-uno para carpeta específica
carpeta_objetivo <- "Lab/10-S1-2025-SEDQ"  # CAMBIAR AQUÍ

# Cargar sistema y aplicar correcciones
source("Auxiliares/sistema_correccion_global/aplicador_automatico.R")
cat("📊 Estado inicial:\n")
validar_archivos_masivo(carpeta_objetivo)
cat("\n🔧 Aplicando correcciones:\n")
aplicar_correcciones_masivas(carpeta_objetivo)
cat("\n📊 Estado final:\n")
validar_archivos_masivo(carpeta_objetivo)
cat("\n✅ Proceso completado para:", carpeta_objetivo, "\n")
```

## ✅ EJEMPLO EXITOSO REAL

### Resultado de aplicar el tutorial a Lab/10-S1-2025-SEDQ:

```
📁 CORRECCIÓN DE CARPETA ESPECÍFICA
===================================
Carpeta objetivo: Lab/10-S1-2025-SEDQ

🔍 PASO 1: ANÁLISIS INICIAL
---------------------------
❌ proporciones_numerico_variacional_formulacion_ejecucion_n2_v1.Rmd :
   Error de concordancia con 'familias', Error de concordancia con 'empresas'
📊 Total de archivos con errores: 1

🔧 PASO 2: APLICANDO CORRECCIONES
---------------------------------
🔧 Procesando archivo: proporciones_numerico_variacional_formulacion_ejecucion_n2_v1.Rmd
💾 Backup creado: proporciones_numerico_variacional_formulacion_ejecucion_n2_v1.Rmd.backup.20250612_185929
✏️  237 líneas corregidas
📦 Sistema de corrección agregado al archivo
✅ Archivo actualizado con 237 correcciones

🔍 PASO 3: VALIDACIÓN FINAL
---------------------------
✅ No se encontraron errores de concordancia

📊 RESUMEN:
Errores antes: 1
Errores después: 0
Archivos procesados: 1
🎉 ¡Todos los errores corregidos exitosamente!
```

### Lo que se corrigió automáticamente:
- ✅ **Errores de concordancia:** "familias matriculados" → "familias matriculadas"
- ✅ **Errores de concordancia:** "empresas registrados" → "empresas registradas"
- ✅ **Sistema integrado:** Se agregó automáticamente el sistema de corrección
- ✅ **Backup creado:** Archivo original respaldado automáticamente
- ✅ **237 correcciones** aplicadas en total

## 🎉 RESULTADO ESPERADO

Después de seguir este tutorial:
- ✅ Todos los errores de concordancia corregidos
- ✅ Backups automáticos creados
- ✅ Archivos compilables sin errores
- ✅ Reporte de validación limpio
- ✅ Sistema de corrección integrado en archivos
