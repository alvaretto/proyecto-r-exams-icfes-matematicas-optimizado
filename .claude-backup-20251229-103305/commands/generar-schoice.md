---
description: Genera ejercicio R-exams tipo SCHOICE (selección única) - Después requiere Ciclo de Validación.
---

# Generador SCHOICE

Genera un archivo .Rmd de tipo **schoice** (selección única) siguiendo la estructura
del proyecto.

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

### Paso 5: Guardar en carpeta de desarrollo
```bash
# Guardar en /A-Produccion/En-Desarrollo/
# Nombre: [ejercicio]_[componente]_[competencia]_n[nivel]_v1.Rmd
```

### Paso 6: Validación
Ejecutar skill `validar-diversidad-300` para confirmar aleatorización.

### Paso 7: Promoción (después de testear)
Una vez validado, usar `/promover-ejercicio [nombre.Rmd]` para mover a `/A-Produccion/Nuevos-Ejercicios/`

## ⛔ CONDICIONES CRÍTICAS

1. ✓ **SIEMPRE** consultar ejemplos funcionales ANTES de escribir código
2. ✓ **SIEMPRE** ejecutar Ciclo de Validación después de generar
3. ✓ **Ejemplos funcionales** = Fuente de verdad ABSOLUTA
4. ❌ **NUNCA** promover sin completar validación

## Regla de Oro
**NUNCA improvises**. Consulta `/A-Produccion/Ejemplos-Funcionales-Rmd/` antes de escribir.

## Referencias

- `/A-Produccion/Ejemplos-Funcionales-Rmd/` (FUENTE DE VERDAD)
- `.claude/Mermaid_Chart.txt` (diagrama de flujo oficial)

