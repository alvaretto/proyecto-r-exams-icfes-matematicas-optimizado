---
description: Genera ejercicio R-exams tipo CLOZE (pregunta compuesta) a partir del análisis ICFES.
---

# Generador CLOZE

Genera un archivo .Rmd de tipo **cloze** (pregunta compuesta con múltiples gaps) 
siguiendo la estructura del proyecto.

## Parámetros de entrada
- **$ARGUMENTS**: Ruta de imagen o descripción del ejercicio

## Ruta de generación
**Carpeta destino**: `/A-Produccion/En-Desarrollo/`

Una vez testeado, usar `/promover-ejercicio` para mover a `/A-Produccion/Nuevos-Ejercicios/`

## Flujo de generación

### Paso 1: Verificar clasificación
Confirma que el ejercicio fue clasificado con `/analizar-icfes`.

### Paso 2: Consultar ejemplos funcionales CLOZE
```bash
ls /06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/09-Probabilidad-Condicionada_Independencia-De-Sucesos/
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

### Paso 3: Estructura obligatoria del .Rmd CLOZE

1. **Encabezado YAML** con configuración completa
2. **Chunk inicio**: Librerías + configuración numérica
3. **Chunk data_generation**: 
   - Función `generar_datos()` con aleatorización completa
   - `options(scipen = 999)` para evitar notación científica
   - Funciones `formatear_entero()` y `formato_estandar()`
   - **Si hay gráfica TikZ**: Función `generar_tikz_[tipo]()` parametrizable
4. **Chunk version_diversity_test**: Test de 300+ versiones
5. **Sección Question**: 
   - Enunciado con gaps: `##ANSWER1##`, `##ANSWER2##`, etc.
   - **Si hay gráfica TikZ**: Chunk de renderizado condicional
   - Answerlist para cada gap
6. **Sección Solution**: Explicación detallada
7. **Meta-information CRÍTICA**:
   - `extype: cloze`
   - `exclozetype: schoice|num|string` (separados por `|`)
   - `extol: 0|1|0` (tolerancias: 0 para schoice, ≥1 para numéricos grandes)
   - `exsolution: 1000|42.5|texto`

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

### Paso 4: Configuración de tolerancias
- **schoice**: tolerancia = 0 (exactitud requerida)
- **num con valores grandes**: tolerancia ≥ 1
- **num con decimales pequeños**: tolerancia 0.01-0.1

### Paso 5: Metadatos ICFES obligatorios
```yaml
# icfes:
#   competencia: [interpretacion_representacion|formulacion_ejecucion|argumentacion]
#   nivel_dificultad: [1|2|3|4]
#   componente: [geometrico_metrico|numerico_variacional|aleatorio]
```

### Paso 6: Guardar en carpeta de desarrollo
```bash
# Guardar en /A-Produccion/En-Desarrollo/
# Nombre: [ejercicio]_[componente]_[competencia]_n[nivel]_v1.Rmd
```

### Paso 7: Validación
Ejecutar skill `validar-diversidad-300` y `validar-metadatos-icfes`.

### Paso 8: Promoción (después de testear)
Una vez validado, usar `/promover-ejercicio [nombre.Rmd]` para mover a `/A-Produccion/Nuevos-Ejercicios/`

## Regla de Oro
**NUNCA improvises**. Consulta ejemplos funcionales en:
- `/A-Produccion/Ejemplos-Funcionales-Rmd/` (FUENTE DE VERDAD)

Para ejemplos CLOZE específicos también revisa:
`/06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/09-Probabilidad-Condicionada_Independencia-De-Sucesos/`

## Integración con Repositorio TikZ

- **Consultar antes de generar**: Usar `consultar-grafica-tikz` para reutilizar gráficas existentes
- **Validar parámetros**: Asegurar que todos los placeholders tienen valores
- **Renderizado condicional**: SIEMPRE usar patrón LaTeX/HTML condicional
- **Referencias**: `Repositorio-Graficas-TikZ/README.md` y `.claude/skills/consultar-grafica-tikz/skill.md`

