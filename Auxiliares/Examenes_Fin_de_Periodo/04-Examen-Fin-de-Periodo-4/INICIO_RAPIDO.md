# 🚀 INICIO RÁPIDO - GENERACIÓN DE EXÁMENES

## ⚡ Ejecución en 3 Pasos

### 📍 Paso 1: Abrir RStudio y Establecer Directorio

```r
setwd("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/Auxiliares/Examenes_Fin_de_Periodo/04-Examen-Fin-de-Periodo-4")
```

### 🔍 Paso 2: Verificar Requisitos (Opcional pero Recomendado)

```r
source("verificar_requisitos.R")
```

**Espera a ver:**
```
✅ ¡TODO LISTO! No se encontraron errores ni advertencias
🚀 Puedes ejecutar SemilleroFinDePeriodo4.R sin problemas
```

### 🎯 Paso 3: Generar Exámenes

```r
source("SemilleroFinDePeriodo4.R")
```

**Espera aproximadamente 2-5 minutos** mientras se generan los archivos.

---

## 📦 Archivos Generados

Después de ejecutar, encontrarás en la carpeta `./salida/`:

### 📄 Formato DOCX (Word)
- `Examen_Periodo4_pandoc_sin_soluciones.docx` ← Para entregar a estudiantes
- `Examen_Periodo4_pandoc_con_soluciones.docx` ← Para el profesor

### 📄 Formato PDF
- `Examen_Periodo4_pdf_sin_soluciones1.pdf` hasta `...5.pdf` ← 5 versiones sin soluciones
- `Examen_Periodo4_pdf_con_soluciones1.pdf` hasta `...5.pdf` ← 5 versiones con soluciones

### 📄 Formato NOPS (Escaneable)
- `Examen_Periodo4_nops1.pdf` hasta `...5.pdf` ← 5 versiones escaneables

---

## 🧪 Prueba Rápida (Opcional)

Si quieres probar primero con **solo 1 versión**:

```r
source("prueba_rapida.R")
```

Los archivos de prueba se guardarán en `./salida_prueba/`

Para limpiar archivos de prueba:
```r
unlink("salida_prueba", recursive = TRUE)
```

---

## ⚙️ Personalización Rápida

### Cambiar Número de Versiones

Edita `SemilleroFinDePeriodo4.R` línea 12:
```r
copias <- 10  # Cambia 5 por el número que necesites
```

### Usar Diferentes Preguntas

Edita `SemilleroFinDePeriodo4.R` líneas 27-43 para modificar la lista de archivos .Rmd

---

## ❓ Solución de Problemas Rápidos

### ❌ Error: "no se pudo encontrar la función 'exams2pandoc'"

**Solución:**
```r
install.packages("exams")
library(exams)
```

### ❌ Error: "cannot open file '001-...'"

**Solución:** Verifica que estés en el directorio correcto:
```r
getwd()  # Debe mostrar: .../04-Examen-Fin-de-Periodo-4
```

### ❌ Error en compilación LaTeX

**Solución:** Instala LaTeX:
```bash
# En terminal de Linux
sudo apt-get install texlive-full
```

---

## 📚 Más Información

- **Guía completa:** `README_GENERACION_EXAMENES.md`
- **Resumen técnico:** `00-RESUMEN_ADAPTACION_SCRIPT.md`
- **Índice de archivos:** `INDICE_SCRIPTS.md`

---

## 🎯 Resumen Ultra-Rápido

```r
# Copiar y pegar todo esto en RStudio:

setwd("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/Auxiliares/Examenes_Fin_de_Periodo/04-Examen-Fin-de-Periodo-4")
source("verificar_requisitos.R")
source("SemilleroFinDePeriodo4.R")
```

**¡Listo!** Los archivos estarán en `./salida/`

---

**Tiempo estimado total:** 3-5 minutos  
**Dificultad:** ⭐ Fácil  
**Última actualización:** 2025-11-04

