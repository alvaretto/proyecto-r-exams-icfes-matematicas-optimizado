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

### Paso 5: Guardar con NOMENCLATURA OBLIGATORIA

**CRÍTICO:** Aplicar nomenclatura oficial. Ver `.claude/docs/NOMENCLATURA_ARCHIVOS_RMD.md`

```bash
# Formato: [ejercicio]_[componente]_[competencia]_n[nivel]_v[version].Rmd
# Guardar en /A-Produccion/En-Desarrollo/
```

**El campo `exname` DEBE coincidir con el nombre del archivo (sin .Rmd)**

### Paso 6: Validación
Ejecutar skill `validar-diversidad-300` para confirmar aleatorización.

### Paso 7: Promoción (después de testear)
Una vez validado, usar `/promover-ejercicio [nombre.Rmd]` para mover a `/A-Produccion/Nuevos-Ejercicios/`

## Regla de Oro
**NUNCA improvises**. Consulta `/A-Produccion/En-Produccion/` antes de escribir.

