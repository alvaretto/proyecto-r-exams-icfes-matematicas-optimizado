---
description: Genera ejercicio R-exams tipo CLOZE (pregunta compuesta) - Después requiere Ciclo de Validación.
---

# Generador CLOZE

Genera un archivo .Rmd de tipo **cloze** (pregunta compuesta con múltiples gaps)
siguiendo la estructura del proyecto.

## ⚡ IMPORTANTE: Después de generar, ejecutar Ciclo de Validación

```
Generación del archivo .Rmd
    │
    ▼
🔄 FASE 1: /validar-renderizado
    │
    ▼
🔍 FASE 2: /validar-coherencia
    │
    ▼
⚡ FASE 3: /diagnosticar-errores (si hay errores)
```

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

1. **Detectar tipo de gráfica necesaria**:
   - Categoría: geometria, estadistica, probabilidad
   - Subcategoría: cilindros, barras, arboles_decision, etc.
   - Componente ICFES: geometrico_metrico, aleatorio, etc.
   - Tags relevantes del análisis

2. **Invocar skill `consultar-grafica-tikz`**:
   - Buscar en `Repositorio-Graficas-TikZ/` por criterios del análisis
   - Listar opciones disponibles con previews
   - Mostrar metadata (descripción, parámetros, tags)

3. **Selección de gráfica**:
   - Si usuario selecciona existente:
     - Cargar código TikZ del repositorio
     - Extraer información de parámetros
     - Preparar para integración en `data_generation`
   - Si no existe o usuario prefiere nueva:
     - Sugerir usar `/generar-grafica-nueva`
     - Esperar confirmación antes de continuar
     - Si genera nueva, se guardará automáticamente en repositorio

4. **Si no se requiere gráfica**: Continuar con Paso 3

### Paso 3: Estructura obligatoria del .Rmd CLOZE

1. **Encabezado YAML** con configuración completa
2. **Chunk inicio**: Librerías + configuración numérica
3. **Chunk data_generation**: 
   - Función `generar_datos()` con aleatorización completa
   - `options(scipen = 999)` para evitar notación científica
   - Funciones `formatear_entero()` y `formato_estandar()`
   - **Si hay gráfica TikZ**: Función `generar_tikz_[tipo]()` que usa template del repositorio
   - Parametrizar TikZ con variables aleatorias del ejercicio
4. **Chunk version_diversity_test**: Test de 300+ versiones
5. **Sección Question**: 
   - Enunciado con gaps: `##ANSWER1##`, `##ANSWER2##`, etc.
   - **Si hay gráfica TikZ**: Chunk de renderizado condicional (LaTeX vs HTML)
   - Answerlist para cada gap
6. **Sección Solution**: Explicación detallada
7. **Meta-information CRÍTICA**:
   - `extype: cloze`
   - `exclozetype: schoice|num|string` (separados por `|`)
   - `extol: 0|1|0` (tolerancias: 0 para schoice, ≥1 para numéricos grandes)
   - `exsolution: 1000|42.5|texto`

### Paso 3.5: Integrar código TikZ en .Rmd (MODIFICADO - Solo si hay gráfica)

Si se seleccionó una gráfica TikZ en el Paso 2.5:

**En el chunk `data_generation`**:

```r
# Función para generar código TikZ parametrizable
generar_tikz_[tipo] <- function(param1, param2, ...) {
  # Cargar template del repositorio
  tikz_template <- readLines("Repositorio-Graficas-TikZ/[categoria]/[subcategoria]/[nombre].tikz")
  tikz_code <- paste(tikz_template, collapse = "\n")
  
  # Reemplazar placeholders con valores del ejercicio
  tikz_code <- gsub("%%PARAM1%%", param1, tikz_code)
  tikz_code <- gsub("%%PARAM2%%", param2, tikz_code)
  # ... más reemplazos según parámetros de la gráfica
  
  return(tikz_code)
}

# Generar código TikZ con valores aleatorios
tikz_[nombre] <- generar_tikz_[tipo](valor1, valor2, ...)
```

**En la sección Question, después del enunciado**:

```r
```{r mostrar_[nombre]_tikz, echo=FALSE, results='asis', fig.align='center'}
# Renderizado condicional según formato de salida
es_latex <- knitr::is_latex_output()

if (es_latex) {
  # Para PDF/LaTeX: insertar código TikZ directamente
  cat("\\begin{center}\n")
  cat(tikz_[nombre])
  cat("\n\\end{center}\n\n")
} else {
  # Para HTML: usar include_tikz
  include_tikz(tikz_[nombre],
               name = "[nombre]_imagen",
               markup = "markdown",
               format = typ,
               packages = c("tikz", "xcolor", "amsmath"),
               width = "8cm")
  cat("\n\n")
}
```
```

**CRÍTICO**: Mantener patrón de renderizado condicional para compatibilidad PDF/HTML.

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

## ⛔ CONDICIONES CRÍTICAS

1. ✓ **SIEMPRE** consultar ejemplos funcionales ANTES de escribir código
2. ✓ **SIEMPRE** ejecutar Ciclo de Validación después de generar
3. ✓ **SIEMPRE** configurar tolerancias apropiadas (0 para schoice, ≥1 para numéricos grandes)
4. ✓ **Ejemplos funcionales** = Fuente de verdad ABSOLUTA
5. ❌ **NUNCA** promover sin completar validación

## Regla de Oro
**NUNCA improvises**. Consulta ejemplos funcionales en:
- `/A-Produccion/Ejemplos-Funcionales-Rmd/` (FUENTE DE VERDAD)

Para ejemplos CLOZE específicos también revisa:
`/06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/09-Probabilidad-Condicionada_Independencia-De-Sucesos/`

## Referencias

- `/A-Produccion/Ejemplos-Funcionales-Rmd/` (FUENTE DE VERDAD)
- `Repositorio-Graficas-TikZ/` - Repositorio de gráficas TikZ reutilizables
- `.claude/skills/consultar-grafica-tikz/skill.md` - Consultar repositorio TikZ
- `.claude/commands/generar-grafica-nueva.md` - Generar nuevas gráficas
- `.claude/Mermaid_Chart.txt` (diagrama de flujo oficial)

