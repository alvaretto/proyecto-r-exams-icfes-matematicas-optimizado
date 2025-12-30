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

## ⚠️ NOMENCLATURA OBLIGATORIA

**Todo archivo .Rmd DEBE seguir este formato:**

```
[ejercicio]_[componente]_[competencia]_n[nivel]_v[version].Rmd
```

| Parte | Valores |
|-------|---------|
| `[componente]` | `geometrico_metrico` \| `numerico_variacional` \| `aleatorio` |
| `[competencia]` | `interpretacion_representacion` \| `formulacion_ejecucion` \| `argumentacion` |
| `n[nivel]` | `n1` \| `n2` \| `n3` \| `n4` |

**Ejemplo:** `series_temporales_poblacion_aleatorio_interpretacion_representacion_n2_v1.Rmd`

**Documentación:** `.claude/docs/NOMENCLATURA_ARCHIVOS_RMD.md`

## Flujo de generación

### Paso 0: ⚠️ SELECCIÓN OBLIGATORIA DE VERSIÓN GRÁFICA

**Si el ejercicio incluye gráficos del workflow graficador:**

```
¿Cuál versión usar para el .Rmd?
1. TikZ (imagen externa)
2. Python (reticulate)
3. R/ggplot2 (RECOMENDADO - nativo)
```

**NO continuar sin respuesta del usuario.**

### Paso 1: Verificar clasificación
Confirma que el ejercicio fue clasificado con `/analizar-icfes`.

### Paso 2: Consultar ejemplos funcionales
```bash
# Ejemplos en producción
ls /A-Produccion/En-Produccion/*.Rmd | head -5

# Ejemplos en pre-desarrollo (también funcionales)
ls /A-Produccion/En-PreDesarrollo/**/*.Rmd | head -5
```

### Paso 3: Estructura obligatoria del .Rmd

1. **Encabezado YAML** con `output: pdf_document`, `header-includes` para TikZ/babel
2. **Chunk inicio**: Librerías (exams, tidyverse, knitr, reticulate)
3. **Chunk data_generation**: Función `generar_datos()` con aleatorización
4. **Chunk version_diversity_test**: Test de 300+ versiones únicas
5. **Sección Question**: Enunciado + Answerlist (4 opciones mínimo)
6. **Sección Solution**: Explicación detallada + Answerlist (Verdadero/Falso)
7. **Meta-information**:
   - `extype: schoice`
   - `exsolution: 1000` (posición de respuesta correcta)
   - `exshuffle: TRUE`

### Paso 4: Metadatos ICFES obligatorios
Incluir en comentarios YAML:
```yaml
# icfes:
#   competencia: [interpretacion_representacion|formulacion_ejecucion|argumentacion]
#   nivel_dificultad: [1|2|3|4]
#   componente: [geometrico_metrico|numerico_variacional|aleatorio]
```

### Paso 5: Crear CARPETA y Guardar con NOMENCLATURA OBLIGATORIA

**CRÍTICO:** Crear carpeta Y aplicar nomenclatura oficial.

```bash
# 1. Nombre: [ejercicio]_[componente]_[competencia]_n[nivel]_v[version]
# 2. Crear carpeta: mkdir -p outputs/$NOMBRE
# 3. Mover archivos relacionados a la carpeta
# 4. Guardar .Rmd DENTRO de la carpeta
```

**Estructura obligatoria:**
```
outputs/[nombre]/
├── [nombre].Rmd
├── output_tikz.tex, output_python.py, output_r.R
├── tikz_final.png, python_final.png, r_final.png
└── analisis_inicial.json, workflow_state.json
```

Ver `.claude/docs/NOMENCLATURA_ARCHIVOS_RMD.md`

### Paso 6: Validación
Ejecutar skill `validar-diversidad-300` para confirmar aleatorización.

### Paso 7: Promoción (después de testear)
Una vez validado, usar `/promover-ejercicio [nombre.Rmd]` para mover a `/A-Produccion/Nuevos-Ejercicios/`

## ⚠️ ERRORES COMUNES DE COMPILACIÓN

### Inclusión de gráficos (PATRÓN OBLIGATORIO)

```r
# En chunk data generation:
p <- ggplot(...) + ...
ggsave("grafico.png", plot = p, width = 8, height = 5, dpi = 150)
include_supplement("grafico.png")
```

```markdown
# En sección Question:
![](grafico.png)
```

**NUNCA** usar `{r grafico}` con `print(p)` - R-exams no lo captura.

### Formato de números (locale español)

```r
# ✅ SIEMPRE especificar ambos separadores
format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
```

## ⛔ CONDICIONES CRÍTICAS

1. ✓ **SIEMPRE** consultar ejemplos funcionales ANTES de escribir código
2. ✓ **SIEMPRE** consultar ejemplos funcionales ANTES de corregir errores
3. ✓ **SIEMPRE** verificar VISUALMENTE cada gráfico después de renderizar
4. ✓ **Ejemplos funcionales** = Fuente de verdad ABSOLUTA
5. ❌ **NUNCA** asumir que lógica matemática correcta = visualización correcta

## ⚠️ COHERENCIA MATEMÁTICA EN GRÁFICOS

### Gráficos con cruces de líneas

**PROBLEMA COMÚN**: Factores de escala o ajustes rompen el cruce visual calculado.

**SOLUCIÓN** (ver `poblaciones_paises_graficas_lineas_*.Rmd`):

1. Definir PRIMERO el punto de intersección (x, y)
2. Generar trayectorias que PASEN por ese punto
3. NO aplicar factores diferenciales después del cálculo

### Verificación visual OBLIGATORIA

Después de CADA renderización:
1. Abrir PDF/DOCX generado
2. Verificar cruce visual
3. Si hay error → consultar ejemplo funcional → corregir

## Regla de Oro
**NUNCA improvises**. Consulta `/A-Produccion/En-Produccion/` antes de escribir o corregir.

