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

### Paso 3: Estructura obligatoria del .Rmd CLOZE

1. **Encabezado YAML** con configuración completa
2. **Chunk inicio**: Librerías + configuración numérica
3. **Chunk data_generation**: 
   - Función `generar_datos()` con aleatorización completa
   - `options(scipen = 999)` para evitar notación científica
   - Funciones `formatear_entero()` y `formato_estandar()`
4. **Chunk version_diversity_test**: Test de 300+ versiones
5. **Sección Question**: 
   - Enunciado con gaps: `##ANSWER1##`, `##ANSWER2##`, etc.
   - Answerlist para cada gap
6. **Sección Solution**: Explicación detallada
7. **Meta-information CRÍTICA**:
   - `extype: cloze`
   - `exclozetype: schoice|num|string` (separados por `|`)
   - `extol: 0|1|0` (tolerancias: 0 para schoice, ≥1 para numéricos grandes)
   - `exsolution: 1000|42.5|texto`

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

- `/A-Produccion/En-Produccion/`
- `/A-Produccion/En-PreDesarrollo/`

Para ejemplos CLOZE específicos también revisa:
`/06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/09-Probabilidad-Condicionada_Independencia-De-Sucesos/`

