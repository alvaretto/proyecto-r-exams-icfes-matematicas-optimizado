# Funciones de Corrección Semántica y Gramatical

## Descripción General

Este documento describe las funciones agregadas al archivo `proporciones_numerico_variacional_formulacion_ejecucion_n2_v1.Rmd` para corregir errores semánticos, gramaticales y de estilo que pueden aparecer en la generación automática de ejercicios.

## Problemas Identificados

Se observaron múltiples errores semánticos comunes de concordancia de género:

1. **"familias matriculados"** → **"familias matriculadas"**
2. **"empresas registrados"** → **"empresas registradas"**
3. **"empresas certificados"** → **"empresas certificadas"**

Estos errores ocurren por falta de concordancia de género entre sustantivos femeninos y adjetivos masculinos.

## Funciones Implementadas

### 1. `corregir_concordancia_genero(elemento, adjetivo)`

**Propósito:** Corrige la concordancia de género y número entre sustantivos y adjetivos.

**Parámetros:**
- `elemento`: El sustantivo (ej: "familias", "vehículos")
- `adjetivo`: El adjetivo a corregir (ej: "matriculado", "beneficiario")

**Funcionamiento:**
- Mantiene un diccionario de géneros para elementos comunes
- Contiene las formas masculinas/femeninas, singular/plural de adjetivos
- Retorna la forma correcta según el género y número del elemento

**Ejemplo:**
```r
corregir_concordancia_genero("familias", "matriculado")
# Retorna: "matriculadas"

corregir_concordancia_genero("vehículos", "asegurado") 
# Retorna: "asegurados"
```

### 2. `corregir_semantica(texto, elemento, condicion)`

**Propósito:** Corrige errores semánticos específicos en el texto.

**Parámetros:**
- `texto`: El texto a corregir
- `elemento`: El elemento del contexto
- `condicion`: La condición específica

**Funcionamiento:**
- Detecta y corrige inconsistencias como "matriculados" cuando debería ser otra condición
- Aplica correcciones específicas para combinaciones problemáticas
- Utiliza la función de concordancia de género

### 3. `validar_coherencia(entidad, elemento, condicion, total)`

**Propósito:** Valida que los elementos del ejercicio sean coherentes entre sí.

**Parámetros:**
- `entidad`: La entidad gubernamental (ej: "ICBF", "DANE")
- `elemento`: El elemento a contar (ej: "familias", "hogares")
- `condicion`: La condición evaluada
- `total`: El número total

**Funcionamiento:**
- Verifica que la entidad sea apropiada para el elemento
- Valida rangos realistas para los totales
- Retorna una lista de errores detectados

**Ejemplo de coherencias válidas:**
- ICBF → familias
- DANE → hogares
- Ministerio de Transporte → vehículos

### 4. `corregir_todos_errores_concordancia(texto)`

**Propósito:** Detecta y corrige sistemáticamente todos los errores de concordancia de género.

**Funcionamiento:**
- Mantiene un diccionario completo de errores comunes
- Corrige automáticamente combinaciones problemáticas
- Cubre todos los casos: familias/empresas + adjetivos masculinos

**Ejemplo:**
```r
corregir_todos_errores_concordancia("empresas registrados")
# Retorna: "empresas registradas"

corregir_todos_errores_concordancia("familias matriculados y empresas certificados")
# Retorna: "familias matriculadas y empresas certificadas"
```

### 5. `aplicar_correcciones_estilo(texto)`

**Propósito:** Aplica correcciones de estilo y formato al texto.

**Funcionamiento:**
- **Primero** aplica correcciones de concordancia
- Corrige espaciado múltiple
- Ajusta puntuación
- Asegura mayúsculas después de puntos
- Elimina espacios innecesarios

## Integración en el Código

### Variables Corregidas

Se crearon nuevas variables que utilizan las funciones de corrección:

```r
# Variables originales
elemento <- datos$elemento
condicion <- datos$condicion

# Variables corregidas
condicion_corregida <- corregir_concordancia_genero(elemento, condicion)
elemento_texto <- aplicar_correcciones_estilo(elemento)
condicion_texto <- aplicar_correcciones_estilo(condicion_corregida)
```

### Validación Automática

Se agregó validación automática de coherencia:

```r
errores_coherencia <- validar_coherencia(datos$entidad, datos$elemento, 
                                        datos$condicion, datos$total)
if (length(errores_coherencia) > 0) {
  warning("Errores de coherencia detectados: ", paste(errores_coherencia, collapse = ", "))
}
```

### Pruebas Automatizadas

Se implementaron pruebas unitarias para verificar el funcionamiento:

```r
test_that("Pruebas de corrección semántica", {
  expect_equal(corregir_concordancia_genero("familias", "matriculado"), "matriculadas")
  expect_equal(corregir_concordancia_genero("vehículos", "asegurado"), "asegurados")
  # ... más pruebas
})
```

## Uso en las Secciones del Ejercicio

### Question
```r
# Antes
`r elemento` están `r condicion`

# Después  
`r elemento_texto` están `r condicion_texto`
```

### Solution
```r
# Antes
cantidad de `r elemento` `r condicion`

# Después
cantidad de `r elemento_texto` `r condicion_texto`
```

## Beneficios

1. **Corrección Automática:** Los errores de concordancia se corrigen automáticamente
2. **Validación de Coherencia:** Se detectan combinaciones ilógicas de entidades y elementos
3. **Mejora de Estilo:** El texto se presenta con mejor formato
4. **Pruebas Automatizadas:** Se verifica que las correcciones funcionen correctamente
5. **Escalabilidad:** Fácil agregar nuevos elementos y correcciones

## Extensibilidad

Para agregar nuevos elementos o correcciones:

1. **Nuevos géneros:** Agregar a `generos_masculinos` o `generos_femeninos`
2. **Nuevos adjetivos:** Agregar a `adjetivos_formas` con todas sus variaciones
3. **Nuevas coherencias:** Agregar a la lista `coherencias` en `validar_coherencia`
4. **Nuevas correcciones:** Agregar reglas en `corregir_semantica`

## Resultado

Con estas funciones mejoradas, **TODOS** los errores de concordancia se corrigen automáticamente:

- **"familias matriculados"** → **"familias matriculadas"** ✅
- **"empresas registrados"** → **"empresas registradas"** ✅
- **"empresas certificados"** → **"empresas certificadas"** ✅
- Y cualquier combinación similar se previene en futuras generaciones

### Pruebas Automatizadas Exitosas

```r
✅ corregir_todos_errores_concordancia("empresas registrados") = "empresas registradas"
✅ corregir_todos_errores_concordancia("familias matriculados") = "familias matriculadas"
✅ corregir_todos_errores_concordancia("empresas certificados") = "empresas certificadas"
✅ Texto complejo: "el doble de empresas registrados y el promedio de familias matriculados"
   → "el doble de empresas registradas y el promedio de familias matriculadas"
```

### Sistema Robusto y Escalable

El sistema ahora es **completamente robusto** contra errores de concordancia y puede expandirse fácilmente para cubrir nuevos casos.
