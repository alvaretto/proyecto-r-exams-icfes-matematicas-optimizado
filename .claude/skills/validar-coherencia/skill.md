---
name: validar-coherencia
description: Ejecuta 🔍 FASE 2 del Ciclo de Validación Automática - Validación Visual y Funcional.
---

# Skill: 🔍 FASE 2 - Validación Visual y Funcional

## ⚡ CONTEXTO: Ciclo de Validación y Corrección Automática

Este skill ejecuta la **FASE 2: VALIDACIÓN VISUAL Y FUNCIONAL** del ciclo obligatorio:

```
🔄 FASE 1: Renderizado Inicial
    │
    ▼
🔍 FASE 2: VALIDACIÓN VISUAL Y FUNCIONAL ← ESTE SKILL
    │
    ▼
⚡ FASE 3: Decisión y Acción
    ├── 📚 SUBFASE 3A: Corrección basada en ejemplos
    ├── 🔄 SUBFASE 3B: Revalidación (volver a FASE 1)
    └── 📊 SUBFASE 3C: Documentar solución
```

## Propósito
Verificar coherencia en TODOS los aspectos del ejercicio antes de tomar decisiones
en FASE 3.

## Tipos de Coherencia a Verificar

### 1. Coherencia Matemática (ERR_C1)

**Verificar**:
- Fórmulas aplicadas correctamente
- Cálculos intermedios válidos
- Resultado final correcto
- Opciones de respuesta coherentes

**Checklist**:
```
□ La fórmula usada es correcta para el problema
□ Los valores numéricos están en rangos válidos
□ La respuesta correcta coincide con el cálculo
□ Los distractores son matemáticamente plausibles
□ No hay errores de redondeo significativos
```

**Ejemplo de error**:
```r
# Problema: Calcular área de círculo
radio <- 5
area <- pi * radio  # ❌ Error: debería ser pi * radio^2
```

**Corrección**:
```r
radio <- 5
area <- pi * radio^2  # ✅ Correcto
```

### 2. Coherencia Imagen-Texto (ERR_C2)

**Verificar**:
- Descripción textual coincide con el gráfico
- Valores etiquetados en imagen = valores en texto
- Colores/formas mencionados = colores/formas mostrados

**Checklist**:
```
□ Las dimensiones en el texto coinciden con el gráfico
□ Los colores descritos son los colores mostrados
□ La orientación/posición descrita es correcta
□ Las etiquetas del gráfico son legibles
□ La escala del gráfico es apropiada para los valores
```

**Ejemplo de error**:
```r
# Texto dice: "un cilindro de radio 5 cm"
# TikZ genera: \def\radio{3}  # ❌ Incoherencia
```

**Corrección**:
```r
radio <- 5  # Variable R
tikz_code <- paste0("\\def\\radio{", radio, "}")  # ✅ Sincronizado
```

### 3. Coherencia de Código (ERR_C3)

**Verificar**:
- Variables R sincronizadas con Python
- Variables R sincronizadas con TikZ
- Formato de números consistente
- Tipos de datos correctos

**Checklist**:
```
□ Variables numéricas usadas antes de formatear
□ Transferencia R→Python correcta (r.variable)
□ Transferencia R→TikZ correcta (paste0)
□ No hay funciones matemáticas sobre strings
□ Semilla aleatoria genera datos válidos
```

**Ejemplo de error**:
```r
b <- -2.5
b_formateado <- sprintf("%.1f", b)
ecuacion <- paste0("y = x - ", abs(b_formateado))  # ❌ abs() sobre string
```

**Corrección**:
```r
b <- -2.5
b_abs <- abs(b)  # ✅ abs() sobre número
b_formateado <- sprintf("%.1f", b_abs)
ecuacion <- paste0("y = x - ", b_formateado)
```

## Algoritmo de Validación

```r
validar_coherencia <- function(archivo_rmd) {
  contenido <- readLines(archivo_rmd)
  errores <- list()
  
  # 1. Buscar funciones matemáticas sobre variables formateadas
  patron_abs <- "abs\\([^)]*formateado"
  if (any(grepl(patron_abs, contenido))) {
    errores <- c(errores, "ERR_C3: abs() sobre variable formateada")
  }
  
  # 2. Verificar sincronización R-TikZ
  vars_r <- extraer_variables_r(contenido)
  vars_tikz <- extraer_variables_tikz(contenido)
  if (!all(vars_tikz %in% names(vars_r))) {
    errores <- c(errores, "ERR_C2: Variables TikZ no definidas en R")
  }
  
  # 3. Verificar metadatos
  if (!any(grepl("^exsolution:", contenido))) {
    errores <- c(errores, "ERR_C1: exsolution no definida")
  }
  
  return(errores)
}
```

## Flujo de Validación

```
Cargar archivo .Rmd
    ↓
Extraer chunks de código (R, Python, TikZ)
    ↓
Verificar coherencia matemática
    ↓
Verificar coherencia imagen-texto
    ↓
Verificar coherencia de código
    ↓
Generar reporte de coherencia
    ↓
Si hay errores → Sugerir correcciones
```

## Reporte de Coherencia

```
╔════════════════════════════════════════╗
║     REPORTE DE COHERENCIA              ║
╠════════════════════════════════════════╣
║ Coherencia Matemática:    ✅ OK        ║
║ Coherencia Imagen-Texto:  ⚠️ 1 error   ║
║   → Línea 45: radio=5 vs TikZ radio=3  ║
║ Coherencia de Código:     ✅ OK        ║
╠════════════════════════════════════════╣
║ Estado: REQUIERE CORRECCIÓN            ║
╚════════════════════════════════════════╝
```

## Flujo de Decisión Post-FASE 2

```
Coherencia 100% + Renderizado 100%
    → FASE 3: ❌ SIN ERRORES → Aprobar para producción

Cualquier error detectado
    → FASE 3: ✓ CON ERRORES → Ejecutar subfases:
        ├── 📚 SUBFASE 3A: Consultar ejemplos funcionales
        ├── 🔄 SUBFASE 3B: Volver a FASE 1
        └── 📊 SUBFASE 3C: Documentar solución
```

## ⛔ CONDICIONES CRÍTICAS

1. ✓ SIEMPRE verificar los 4 tipos de coherencia
2. ✓ SIEMPRE registrar errores con clasificación ERR_XX
3. ✓ SIEMPRE continuar a FASE 3 (decisión)
4. ❌ NUNCA omitir verificaciones
5. ❌ NUNCA terminar con errores sin resolver

## Integración con Ciclo Completo

- **validar-renderizado** → Ejecuta FASE 1 (antes de este skill)
- **Este skill** → Ejecuta FASE 2
- **diagnosticar-errores** → Inicia FASE 3 si hay errores
- **SUBFASE 3B** → Vuelve a FASE 1 (validar-renderizado)

## Referencias

- `.claude/Mermaid_Chart.txt` (diagrama de flujo oficial)
- `/A-Produccion/Ejemplos-Funcionales-Rmd/` (fuente de verdad)
- `.claude/docs/patrones-errores-conocidos.md` (Error 2)
- `.augment/rules/reglas-generales.md` (criterios ICFES)

## Ejecución

Cuando el usuario invoca `/validar-coherencia`:

1. Cargar archivo .Rmd objetivo
2. Parsear chunks R, Python, TikZ
3. Verificar coherencia matemática
4. Verificar coherencia imagen-texto
5. Verificar coherencia de código
6. Consolidar resultados de renderizado (FASE 1)
7. Generar reporte de FASE 2
8. Continuar automáticamente a FASE 3 (decisión)

