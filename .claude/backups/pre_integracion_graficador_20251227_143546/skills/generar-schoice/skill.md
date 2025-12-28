---
description: Genera ejercicio R-exams tipo SCHOICE (selección única) a partir del análisis ICFES.
---

# Generador SCHOICE

Genera un archivo .Rmd de tipo **schoice** (selección única) siguiendo la estructura 
del proyecto.

## Parámetros de entrada
- **$ARGUMENTS**: Ruta de imagen o descripción del ejercicio

## Ruta de generación
**Carpeta destino**: `/A-Produccion/En-Desarrollo/`

Una vez testeado, usar `/promover-ejercicio` para mover a `/A-Produccion/Nuevos-Ejercicios/`

## Flujo de generación

### Paso 1: Verificar clasificación
Confirma que el ejercicio fue clasificado con `/analizar-icfes`.

### Paso 2: Consultar ejemplos funcionales
```bash
# Ejemplos en producción
ls /A-Produccion/En-Produccion/*.Rmd | head -5

# Ejemplos en pre-desarrollo (también funcionales)
ls /A-Produccion/En-PreDesarrollo/**/*.Rmd | head -5
```

### Paso 2.5: Verificar necesidad de gráficas TikZ (NUEVO)

Si el análisis ICFES indica que el ejercicio requiere gráficas:

1. **Invocar skill `consultar-grafica-tikz`** con:
   - Categoría del análisis (geometria, estadistica, probabilidad)
   - Subcategoría detectada
   - Componente ICFES
   - Tags relevantes

2. **Procesar resultados**:
   - Si encuentra opciones: Listar y permitir selección
   - Si no encuentra: Sugerir `/generar-grafica-nueva`
   - Cargar código TikZ seleccionado con placeholders

3. **Preparar integración**:
   - Extraer parámetros de metadata JSON
   - Preparar función `generar_tikz_[tipo]()` parametrizable
   - Mapear parámetros a variables aleatorias del ejercicio

### Paso 3: Estructura obligatoria del .Rmd

1. **Encabezado YAML** con `output: pdf_document`, `header-includes` para TikZ/babel
2. **Chunk inicio**: Librerías (exams, tidyverse, knitr, reticulate)
3. **Chunk data_generation**: 
   - Función `generar_datos()` con aleatorización
   - **Si hay gráfica TikZ**: Función `generar_tikz_[tipo]()` parametrizable
4. **Chunk version_diversity_test**: Test de 300+ versiones únicas
5. **Sección Question**: Enunciado + Answerlist (4 opciones mínimo)
   - **Si hay gráfica TikZ**: Chunk de renderizado condicional
6. **Sección Solution**: Explicación detallada + Answerlist (Verdadero/Falso)
7. **Meta-information**:
   - `extype: schoice`
   - `exsolution: 1000` (posición de respuesta correcta)
   - `exshuffle: TRUE`

### Paso 3.5: Integración TikZ (Solo si hay gráfica)

**Template de función TikZ parametrizable**:

```r
generar_tikz_[tipo] <- function(param1, param2, ...) {
  # Cargar template del repositorio
  ruta_template <- "Repositorio-Graficas-TikZ/[categoria]/[subcategoria]/[nombre].tikz"
  tikz_template <- readLines(ruta_template)
  tikz_code <- paste(tikz_template, collapse = "\n")
  
  # Reemplazar placeholders
  tikz_code <- gsub("%%PARAM1%%", as.character(param1), tikz_code)
  tikz_code <- gsub("%%PARAM2%%", as.character(param2), tikz_code)
  # ... más reemplazos según parámetros
  
  return(tikz_code)
}
```

**Validación de parámetros**:
- Verificar que todos los placeholders del template tienen valores
- Asegurar tipos de datos correctos (numéricos, strings)
- Manejar casos especiales (colores, unidades)

**Renderizado condicional** (OBLIGATORIO):
- Usar `knitr::is_latex_output()` para detectar formato
- PDF: Insertar código TikZ directamente
- HTML: Usar `include_tikz()` de exams

### Paso 4: Metadatos ICFES obligatorios
Incluir en comentarios YAML:
```yaml
# icfes:
#   competencia: [interpretacion_representacion|formulacion_ejecucion|argumentacion]
#   nivel_dificultad: [1|2|3|4]
#   componente: [geometrico_metrico|numerico_variacional|aleatorio]
```

### Paso 5: Guardar en carpeta de desarrollo
```bash
# Guardar en /A-Produccion/En-Desarrollo/
# Nombre: [ejercicio]_[componente]_[competencia]_n[nivel]_v1.Rmd
```

### Paso 6: Validación
Ejecutar skill `validar-diversidad-300` para confirmar aleatorización.

### Paso 7: Promoción (después de testear)
Una vez validado, usar `/promover-ejercicio [nombre.Rmd]` para mover a `/A-Produccion/Nuevos-Ejercicios/`

## Regla de Oro
**NUNCA improvises**. Consulta `/A-Produccion/Ejemplos-Funcionales-Rmd/` antes de escribir.

## Integración con Repositorio TikZ

- **Consultar antes de generar**: Usar `consultar-grafica-tikz` para reutilizar gráficas existentes
- **Validar parámetros**: Asegurar que todos los placeholders tienen valores
- **Renderizado condicional**: SIEMPRE usar patrón LaTeX/HTML condicional
- **Referencias**: `Repositorio-Graficas-TikZ/README.md` y `.claude/skills/consultar-grafica-tikz/skill.md`

