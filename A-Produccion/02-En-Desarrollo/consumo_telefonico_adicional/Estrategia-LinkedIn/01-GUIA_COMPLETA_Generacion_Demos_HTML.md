# 🚀 GUÍA COMPLETA: Generación de Recursos para LinkedIn (ACTUALIZADA)

## 📋 OBJETIVO

Generar recursos completos para publicación en LinkedIn:

- ✅ 5 demos HTML interactivos
- ✅ PDF con 10 versiones del ejercicio
- ✅ Archivo Moodle XML para importar a LMS

**Sistema**: Usa `SemilleroUnico_v2.R` y `SemilleroMoodle_v2.R` que ya funcionan en el proyecto.

---

## 🔧 REQUISITOS PREVIOS

### Verificar Dependencias del Ejercicio

```r
# Librerías necesarias
library(exams)
library(reticulate)
library(digest)
library(testthat)
library(knitr)

# Verificar Python (para gráficos matplotlib)
library(reticulate)
use_python(Sys.which("python3"), required = TRUE)
py_config()  # Verificar configuración
```

### Verificar Archivos de Dependencia

Asegúrate de que existen estos archivos en el directorio del ejercicio:

- ✅ `SemilleroUnico_v2.R`
- ✅ `SemilleroMoodle_v2.R`
- ✅ `pcielo.tex`
- ✅ `pcielo_nosol.tex`
- ✅ `solpcielo.tex`

---

## 📁 ESTRUCTURA DE DIRECTORIOS

La estructura se crea automáticamente:

```
consumo_telefonico_adicional/
├── consumo_telefonico_adicional_n2_v1.Rmd  (archivo original)
├── SemilleroUnico_v2.R                      (generación PDF)
├── SemilleroMoodle_v2.R                     (generación Moodle)
├── pcielo.tex, pcielo_nosol.tex             (templates LaTeX)
├── Estrategia-LinkedIn/
│   ├── demos-html/                          (demos generados)
│   ├── recursos-descargables/               (PDFs, código, Moodle)
│   └── scripts/
│       ├── generar_recursos_linkedin.R      (script principal - USAR ESTE)
│       └── copiar_a_docs.sh                 (copia a GitHub Pages)
```

---

## 🚀 GENERACIÓN DE RECURSOS (MÉTODO ACTUALIZADO)

### Script Principal: generar_recursos_linkedin.R

**USAR ESTE SCRIPT** en lugar de `generar_demos_individuales.R`

```r
# Ejecutar desde RStudio
source("A-Produccion/En-Desarrollo/consumo_telefonico_adicional/Estrategia-LinkedIn/scripts/generar_recursos_linkedin.R")
```

### ¿Qué hace este script?

1. **Carga scripts del proyecto** que ya funcionan:

   - `SemilleroUnico_v2.R` → Genera PDFs con templates `pcielo.tex`
   - `SemilleroMoodle_v2.R` → Genera archivos Moodle XML

2. **Genera PDF con 10 versiones**:

   - Usa `exams2pdf()` con templates LaTeX del proyecto
   - Si falla, copia PDF existente de `salida/` como respaldo

3. **Genera archivo Moodle XML**:

   - 5 versiones listas para importar a Moodle
   - Compatible con sistemas LMS estándar

4. **Genera 5 demos HTML**:

   - Usa `exams2html()` (estándar de exams)
   - Verificación automática de respuestas
   - Soluciones completas incluidas

5. **Verificación completa**:

   - Lista todos los archivos generados
   - Muestra tamaños y ubicaciones
   - Indica próximos pasos

### Ventajas sobre el script anterior

| Aspecto | Script Anterior | Script Nuevo |
|---------|----------------|--------------|
| Dependencias | Requiere `exams2forms` | Usa scripts del proyecto |
| PDFs | `exams2pdf()` sin templates | Usa `pcielo.tex` probado |
| Manejo errores | Falla si hay problemas | Copia PDF existente como respaldo |
| Compatibilidad | Puede fallar en algunos sistemas | 100% compatible con el proyecto |
| Demos HTML | `exams2webquiz()` | `exams2html()` estándar |

---

## 📊 SALIDA ESPERADA

Al ejecutar el script verás:

```
╔════════════════════════════════════════════════════════╗
║  GENERACIÓN DE RECURSOS PARA LINKEDIN                 ║
╚════════════════════════════════════════════════════════╝

✓ Archivo encontrado: consumo_telefonico_adicional_n2_v1.Rmd
✓ Directorio creado: Estrategia-LinkedIn/recursos-descargables
✓ Directorio creado: Estrategia-LinkedIn/demos-html

📄 PASO 1: Generando PDF con 10 versiones...
✅ PDF generado exitosamente
   📄 Tamaño: 245.3 KB

🎓 PASO 2: Generando archivo Moodle XML...
✅ Archivo Moodle XML generado exitosamente
   📄 Tamaño: 18.7 KB

🌐 PASO 3: Generando 5 demos HTML...
  Demo 1/5... ✅
  Demo 2/5... ✅
  Demo 3/5... ✅
  Demo 4/5... ✅
  Demo 5/5... ✅

╔════════════════════════════════════════════════════════╗
║  VERIFICACIÓN DE ARCHIVOS GENERADOS                   ║
╚════════════════════════════════════════════════════════╝

📄 PDFs generados: 1
   ✓ muestra_10_versiones_consumo_telefonico1.pdf

🎓 Archivos Moodle XML: 1
   ✓ consumo_telefonico_moodle.xml

🌐 Demos HTML: 5
   ✓ demo_consumo_telefonico_v1.html
   ✓ demo_consumo_telefonico_v2.html
   ✓ demo_consumo_telefonico_v3.html
   ✓ demo_consumo_telefonico_v4.html
   ✓ demo_consumo_telefonico_v5.html

✨ PROCESO COMPLETADO
```

---

## 🔧 SOLUCIÓN DE PROBLEMAS COMUNES

### Problema 1: "No se generó el PDF"

**Causa**: Falta LaTeX o configuración incorrecta

**Solución automática**: El script copia el PDF existente de `salida/`

**Solución manual**:

```bash
# Copiar PDF existente
cp salida/consumo_telefonico_adicional_n2_v1_1.pdf \
   Estrategia-LinkedIn/recursos-descargables/muestra_10_versiones_consumo_telefonico1.pdf
```

### Problema 2: "Error al generar demos HTML"

**Causa**: Problemas con Python/matplotlib

**Solución**:

```r
# Verificar configuración de Python
library(reticulate)
use_python("/usr/bin/python3", required = TRUE)
py_config()

# Verificar matplotlib
py_run_string("import matplotlib.pyplot as plt")
```

### Problema 3: "No se encuentra SemilleroUnico_v2.R"

**Causa**: Directorio de trabajo incorrecto

**Solución**:

```r
# Verificar directorio actual
getwd()

# Cambiar al directorio correcto
setwd("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/En-Desarrollo/consumo_telefonico_adicional")

# Verificar que existen los archivos
file.exists("SemilleroUnico_v2.R")
file.exists("SemilleroMoodle_v2.R")
```

### Problema 4: "Archivos generados pero vacíos"

**Causa**: Error en compilación del .Rmd

**Solución**:

```r
# Probar compilación individual
library(knitr)
knit("consumo_telefonico_adicional_n2_v1.Rmd")

# Revisar errores en chunks de Python
library(reticulate)
use_python("/usr/bin/python3", required = TRUE)
```

---

## 🌐 PRÓXIMOS PASOS: CONFIGURAR GITHUB PAGES

### Paso 1: Crear estructura docs/

```bash
cd /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams

# Crear directorios
mkdir -p docs/demos
mkdir -p docs/recursos
mkdir -p docs/assets/img

# Copiar index.html
cp A-Produccion/En-Desarrollo/consumo_telefonico_adicional/Estrategia-LinkedIn/templates/index.html docs/

# Verificar
ls -lh docs/index.html
```

### Paso 2: Copiar archivos generados

```bash
# Ejecutar script de copia
chmod +x A-Produccion/En-Desarrollo/consumo_telefonico_adicional/Estrategia-LinkedIn/scripts/copiar_a_docs.sh

./A-Produccion/En-Desarrollo/consumo_telefonico_adicional/Estrategia-LinkedIn/scripts/copiar_a_docs.sh
cp A-Produccion/En-Desarrollo/consumo_telefonico_adicional/Estrategia-LinkedIn/recursos-descargables/*.pdf docs/recursos/
cp A-Produccion/En-Desarrollo/consumo_telefonico_adicional/Estrategia-LinkedIn/recursos-descargables/*.xml docs/recursos/
```

### 3.3 Crear Página Index

Crear archivo: `docs/index.html`

```html
<!DOCTYPE html>
<html lang="es">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Demos Interactivos - Matemáticas ICFES R/exams</title>
    <style>
        body { font-family: Arial, sans-serif; max-width: 800px; margin: 50px auto; padding: 20px; }
        h1 { color: #2c3e50; }
        .demo-link { display: block; padding: 15px; margin: 10px 0; background: #3498db; color: white; text-decoration: none; border-radius: 5px; }
        .demo-link:hover { background: #2980b9; }
    </style>
</head>
<body>
    <h1>🎯 Demos Interactivos - Consumo Telefónico</h1>
    <p>Explora diferentes versiones del ejercicio dinámico:</p>
    
    <a href="demos/demo_consumo_telefonico_v1.html" class="demo-link">📊 Demo Versión 1</a>
    <a href="demos/demo_consumo_telefonico_v2.html" class="demo-link">📊 Demo Versión 2</a>
    <a href="demos/demo_consumo_telefonico_v3.html" class="demo-link">📊 Demo Versión 3</a>
    <a href="demos/demo_consumo_telefonico_v4.html" class="demo-link">📊 Demo Versión 4</a>
    <a href="demos/demo_consumo_telefonico_v5.html" class="demo-link">📊 Demo Versión 5</a>
</body>
</html>
```

### 3.4 Configurar GitHub Pages en el Repositorio

1. **Ir a Settings del repositorio en GitHub**
2. **Pages → Source**: Seleccionar rama `gh-pages` y carpeta `/docs`
3. **Save** y esperar deployment (2-3 minutos)
4. **URL resultante**: `https://alvaretto.github.io/proyecto-r-exams-icfes-matematicas-optimizado/`

---

## ✅ VERIFICACIÓN DE FUNCIONAMIENTO

### Checklist de Validación

```r
# Script de verificación
verificar_demos <- function() {
  dir_demos <- "Estrategia-LinkedIn/demos-html"

  # Verificar que existen 5 archivos HTML
  archivos_html <- list.files(dir_demos, pattern = "\\.html$", full.names = TRUE)

  cat(sprintf("✓ Archivos HTML encontrados: %d/5\n", length(archivos_html)))

  # Verificar tamaño de archivos (deben ser > 10KB)
  for (archivo in archivos_html) {
    tamano <- file.info(archivo)$size / 1024
    cat(sprintf("  - %s: %.1f KB\n", basename(archivo), tamano))
  }

  # Verificar que contienen elementos interactivos
  for (archivo in archivos_html) {
    contenido <- readLines(archivo, warn = FALSE)
    tiene_webex <- any(grepl("webex", contenido))
    tiene_mathjax <- any(grepl("MathJax", contenido))

    cat(sprintf("\n%s:\n", basename(archivo)))
    cat(sprintf("  - Elementos interactivos (webex): %s\n", ifelse(tiene_webex, "✓", "✗")))
    cat(sprintf("  - Fórmulas matemáticas (MathJax): %s\n", ifelse(tiene_mathjax, "✓", "✗")))
  }
}

verificar_demos()
```

---

## 🎨 PERSONALIZACIÓN AVANZADA (OPCIONAL)

### Modificar Estilos CSS

Los demos generados incluyen archivos CSS que pueden personalizarse:

```css
/* Personalizar colores de botones */
.webex-button {
  background-color: #3498db;  /* Azul personalizado */
  color: white;
}

.webex-button:hover {
  background-color: #2980b9;
}

/* Personalizar feedback de respuestas */
.webex-correct {
  background-color: #2ecc71;  /* Verde para correctas */
}

.webex-incorrect {
  background-color: #e74c3c;  /* Rojo para incorrectas */
}
```

---

## 📊 MÉTRICAS DE ÉXITO

### Indicadores Clave

- ✅ **5 demos HTML** generados correctamente
- ✅ **Tamaño promedio**: 50-100 KB por demo
- ✅ **Interactividad**: Botones ✓, ?, ↺ funcionales
- ✅ **Compatibilidad móvil**: Responsive design automático
- ✅ **Fórmulas matemáticas**: Renderizadas con MathJax
- ✅ **Gráficos**: Imágenes PNG/PDF embebidas correctamente

---

## 🔧 SOLUCIÓN DE PROBLEMAS COMUNES

### Error: "Python not found"

```r
# Solución: Configurar Python explícitamente
library(reticulate)
use_python("/usr/bin/python3", required = TRUE)
```

### Error: "TikZ compilation failed"

```r
# Solución: Verificar paquetes LaTeX
system("pdflatex --version")  # Debe estar instalado
```

### Demos no se visualizan correctamente

```r
# Solución: Verificar encoding
exams2webquiz(..., encoding = "UTF-8")
```

---

## 📚 RECURSOS ADICIONALES

- **Documentación oficial exams2forms**: https://www.r-exams.org/tutorials/exams2forms/
- **Ejemplos interactivos**: https://www.r-exams.org/
- **Repositorio GitHub**: https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado

---

## 🎯 PRÓXIMOS PASOS

1. ✅ Generar demos HTML (esta guía)
2. ⏭️ Configurar GitHub Pages → Ver `02-CONFIGURACION_GitHub_Pages.md`
3. ⏭️ Crear publicación LinkedIn → Ver `03-TEMPLATE_Publicacion_LinkedIn.md`
4. ⏭️ Automatizar publicaciones diarias → Ver `04-AUTOMATIZACION_Publicaciones_Diarias.md`

---

**FECHA DE CREACIÓN**: Diciembre 2025
**TECNOLOGÍA**: exams2forms v0.1-0+
**AUTOR**: Sistema ICFES R-Exams 2025

