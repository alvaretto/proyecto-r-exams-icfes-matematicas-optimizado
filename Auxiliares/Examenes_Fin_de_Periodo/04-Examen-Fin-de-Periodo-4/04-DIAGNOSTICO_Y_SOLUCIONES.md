# DIAGNÓSTICO Y SOLUCIONES - Problemas de Generación de Exámenes

## 🔍 PROBLEMA REPORTADO

**Síntoma:** Solo se generaron archivos DOCX, pero NO se generaron archivos PDF ni NOPS.

**Archivos generados exitosamente:**
- ✅ `Evaluacion_Fin_de_Periodo_4-docx1.docx` (con soluciones)
- ✅ `Evaluacion_Fin_de_Periodo_4_sin_sol1.docx` (sin soluciones)

**Archivos NO generados:**
- ❌ `Evaluacion_Fin_de_Periodo_4_sol1.pdf` (PDF con soluciones)
- ❌ `Evaluacion_Fin_de_Periodo_41.pdf` (PDF sin soluciones)
- ❌ `Evaluacion_Fin_de_Periodo_4_nops_sol1.pdf` a `..._sol15.pdf` (NOPS con soluciones)
- ❌ `Evaluacion_Fin_de_Periodo_4_nops1.pdf` a `..._nops15.pdf` (NOPS sin soluciones)

---

## 🛠️ SOLUCIÓN 1: EJECUTAR SCRIPT CON LOGS

He creado un script mejorado que registra todos los errores en un archivo de log.

### **Pasos:**

1. **Ejecutar el script con logs:**
   ```r
   source("03-SemilleroFinDePeriodo_v4_con_logs.R")
   ```

2. **Revisar el archivo de log generado:**
   ```r
   # El script crea automáticamente: log_generacion_examenes.txt
   # Abrir y revisar los mensajes de error
   ```

3. **Identificar errores específicos:**
   - Buscar líneas con `[ERROR]` en el log
   - Identificar qué formato falló y por qué

---

## 🔧 CAUSAS COMUNES Y SOLUCIONES

### **CAUSA 1: LaTeX no instalado o no configurado**

**Síntoma:** Los archivos DOCX se generan pero los PDF no.

**Diagnóstico:**
```r
# Verificar si LaTeX está disponible
system("pdflatex --version")
```

**Solución:**
```bash
# En Manjaro/Arch Linux:
sudo pacman -S texlive-core texlive-latexextra texlive-fontsextra

# Verificar instalación:
pdflatex --version
```

---

### **CAUSA 2: Templates LaTeX faltantes o incorrectos**

**Síntoma:** Error al buscar templates `solpcielo.tex`, `exam.tex`, etc.

**Diagnóstico:**
```r
# Verificar que existen los templates
file.exists("solpcielo.tex")  # Debe retornar TRUE
file.exists("exam.tex")        # Debe retornar TRUE
file.exists("pcielo.tex")      # Debe retornar TRUE
file.exists("pcielo_nosol.tex") # Debe retornar TRUE
```

**Solución:**
```r
# Copiar templates desde el directorio de exams
library(exams)

# Obtener templates por defecto
exams_skeleton(dir = "templates_exams")

# Copiar templates necesarios al directorio actual
```

---

### **CAUSA 3: Problemas con archivos .Rmd individuales**

**Síntoma:** Algunos ejercicios .Rmd tienen errores que impiden la compilación a PDF.

**Diagnóstico:**
```r
# Probar compilar cada ejercicio individualmente
library(exams)

# Probar con un solo ejercicio
exams2pdf("001-muestreo_sesgo_municipio_aleatorio_argumentacion_n2_v1.Rmd",
          n = 1,
          name = "test",
          dir = "salida",
          verbose = TRUE)
```

**Solución:**
- Identificar qué ejercicio(s) causan el error
- Revisar y corregir el código LaTeX/TikZ en esos ejercicios
- Temporalmente excluir ejercicios problemáticos

---

### **CAUSA 4: Paquetes LaTeX faltantes**

**Síntoma:** Error relacionado con paquetes LaTeX no encontrados.

**Diagnóstico:**
Revisar el log de errores buscando mensajes como:
```
! LaTeX Error: File `tikz.sty' not found.
! LaTeX Error: File `pgfplots.sty' not found.
```

**Solución:**
```bash
# Instalar paquetes LaTeX adicionales
sudo pacman -S texlive-pictures  # Para TikZ y pgfplots
sudo pacman -S texlive-science   # Para paquetes matemáticos
```

---

### **CAUSA 5: Problemas con exams2nops**

**Síntoma:** NOPS no se genera, posible error con configuración específica.

**Diagnóstico:**
```r
# Probar exams2nops con configuración mínima
library(exams)

exams2nops("001-muestreo_sesgo_municipio_aleatorio_argumentacion_n2_v1.Rmd",
           n = 1,
           name = "test_nops",
           dir = "salida",
           verbose = TRUE)
```

**Soluciones posibles:**

**A) Simplificar configuración de exams2nops:**
```r
# Usar configuración mínima
exams2nops(rep(archivo_examen, each = numpreg_por_archivo),
           n = copias,
           name = paste0(nombre_sin_extension, "_nops"),
           dir = dir_salida,
           edir = dir_ejercicios,
           language = "es")
```

**B) Verificar que todos los ejercicios son tipo schoice:**
```r
# exams2nops solo funciona con preguntas de selección múltiple (schoice)
# Verificar metainformación de cada ejercicio
```

---

### **CAUSA 6: Memoria insuficiente**

**Síntoma:** El proceso se detiene sin mensaje de error claro.

**Diagnóstico:**
```bash
# Monitorear uso de memoria durante la ejecución
htop
```

**Solución:**
```r
# Reducir número de versiones temporalmente
copias <- 5  # En lugar de 15

# O generar formatos por separado
```

---

## 🚀 PROCEDIMIENTO DE DIAGNÓSTICO PASO A PASO

### **PASO 1: Ejecutar script con logs**

```r
source("03-SemilleroFinDePeriodo_v4_con_logs.R")
```

### **PASO 2: Revisar archivo de log**

```r
# Leer el log
readLines("log_generacion_examenes.txt")
```

### **PASO 3: Identificar primer error**

Buscar la primera línea con `[ERROR]` y anotar:
- ¿Qué formato estaba generando? (PDF, NOPS)
- ¿Cuál es el mensaje de error exacto?
- ¿Hay información de traceback?

### **PASO 4: Aplicar solución específica**

Según el error identificado, aplicar la solución correspondiente de la sección anterior.

### **PASO 5: Probar con un solo ejercicio**

```r
# Probar formato problemático con un solo ejercicio
library(exams)

# Para PDF:
exams2pdf("001-muestreo_sesgo_municipio_aleatorio_argumentacion_n2_v1.Rmd",
          n = 1,
          name = "test_pdf",
          template = "solpcielo",
          dir = "salida",
          verbose = TRUE)

# Para NOPS:
exams2nops("001-muestreo_sesgo_municipio_aleatorio_argumentacion_n2_v1.Rmd",
           n = 1,
           name = "test_nops",
           dir = "salida",
           language = "es",
           verbose = TRUE)
```

### **PASO 6: Probar con todos los ejercicios seleccionados**

Una vez que funcione con un ejercicio, probar con todos:

```r
source("03-SemilleroFinDePeriodo_v4_con_logs.R")
```

---

## 📋 CHECKLIST DE VERIFICACIÓN

Antes de ejecutar el script, verificar:

```
✅ LaTeX instalado y funcionando
   Comando: pdflatex --version

✅ Templates LaTeX disponibles
   Archivos: solpcielo.tex, exam.tex, pcielo.tex, pcielo_nosol.tex

✅ Paquete exams instalado
   Comando: library(exams)

✅ Directorio de salida existe
   Comando: dir.create("salida", showWarnings = FALSE)

✅ Todos los ejercicios .Rmd son válidos
   Probar compilar uno individualmente

✅ Suficiente espacio en disco
   Comando: df -h

✅ Suficiente memoria RAM
   Comando: free -h
```

---

## 🔍 COMANDOS DE DIAGNÓSTICO ÚTILES

### **Verificar instalación de LaTeX:**
```bash
pdflatex --version
which pdflatex
```

### **Verificar paquetes LaTeX instalados:**
```bash
tlmgr list --only-installed | grep tikz
tlmgr list --only-installed | grep pgfplots
```

### **Verificar templates disponibles:**
```r
library(exams)
list.files(system.file("tex", package = "exams"))
```

### **Probar compilación LaTeX directamente:**
```bash
cd salida
pdflatex test.tex
```

---

## 📝 SCRIPT DE PRUEBA RÁPIDA

Crear un archivo `test_formatos.R` para probar cada formato individualmente:

```r
library(exams)

# Ejercicio de prueba
ejercicio_test <- "001-muestreo_sesgo_municipio_aleatorio_argumentacion_n2_v1.Rmd"

cat("Probando formato PDF con soluciones...\n")
tryCatch({
  exams2pdf(ejercicio_test, n = 1, name = "test_pdf_sol",
            template = "solpcielo", dir = "salida", verbose = TRUE)
  cat("✓ PDF con soluciones: OK\n")
}, error = function(e) {
  cat("✗ PDF con soluciones: ERROR\n")
  cat(sprintf("  Error: %s\n", e$message))
})

cat("\nProbando formato PDF sin soluciones...\n")
tryCatch({
  exams2pdf(ejercicio_test, n = 1, name = "test_pdf",
            template = "exam", dir = "salida", verbose = TRUE)
  cat("✓ PDF sin soluciones: OK\n")
}, error = function(e) {
  cat("✗ PDF sin soluciones: ERROR\n")
  cat(sprintf("  Error: %s\n", e$message))
})

cat("\nProbando formato NOPS...\n")
tryCatch({
  exams2nops(ejercicio_test, n = 1, name = "test_nops",
             dir = "salida", language = "es", verbose = TRUE)
  cat("✓ NOPS: OK\n")
}, error = function(e) {
  cat("✗ NOPS: ERROR\n")
  cat(sprintf("  Error: %s\n", e$message))
})

cat("\n¡Pruebas completadas!\n")
```

---

## 🎯 PRÓXIMOS PASOS

1. **Ejecutar script con logs:**
   ```r
   source("03-SemilleroFinDePeriodo_v4_con_logs.R")
   ```

2. **Revisar log generado:**
   ```r
   file.show("log_generacion_examenes.txt")
   ```

3. **Compartir el contenido del log** para diagnóstico específico

4. **Aplicar soluciones** según los errores identificados

5. **Ejecutar script de prueba rápida** para verificar cada formato

---

## 📞 INFORMACIÓN PARA SOPORTE

Si necesitas ayuda adicional, proporciona:

1. **Contenido del archivo de log:** `log_generacion_examenes.txt`
2. **Versión de R:** `R.version.string`
3. **Versión de exams:** `packageVersion("exams")`
4. **Sistema operativo:** `Sys.info()["sysname"]`
5. **Versión de LaTeX:** Salida de `pdflatex --version`
6. **Primeros 5 ejercicios .Rmd** que se intentaron compilar

---

## ✅ RESUMEN

**Problema:** Solo se generaron archivos DOCX, no PDF ni NOPS.

**Causa más probable:** 
- LaTeX no instalado o mal configurado
- Templates LaTeX faltantes
- Errores en ejercicios .Rmd individuales

**Solución inmediata:**
1. Ejecutar `03-SemilleroFinDePeriodo_v4_con_logs.R`
2. Revisar `log_generacion_examenes.txt`
3. Aplicar solución específica según el error

**Herramientas de diagnóstico:**
- Script con logs (creado)
- Script de prueba rápida (proporcionado)
- Checklist de verificación (incluido)

