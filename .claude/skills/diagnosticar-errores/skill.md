---
name: diagnosticar-errores
description: Ejecuta ⚡ FASE 3 del Ciclo de Validación Automática - Decisión y Acción con subfases.
---

# Skill: ⚡ FASE 3 - Decisión y Acción

## ⚡ CONTEXTO: Ciclo de Validación y Corrección Automática

Este skill ejecuta la **FASE 3: DECISIÓN Y ACCIÓN** del ciclo obligatorio:

```
🔄 FASE 1: Renderizado Inicial
    │
    ▼
🔍 FASE 2: Validación Visual y Funcional
    │
    ▼
⚡ FASE 3: DECISIÓN Y ACCIÓN ← ESTE SKILL
    │
    ├── ❌ SIN ERRORES → Aprobar para producción
    │
    └── ✓ CON ERRORES:
            ├── 📚 SUBFASE 3A: Corrección basada en ejemplos
            ├── 🔄 SUBFASE 3B: Revalidación (volver a FASE 1)
            └── 📊 SUBFASE 3C: Documentar solución
```

## Propósito
Analizar errores detectados en FASE 2, clasificarlos, y ejecutar las subfases de corrección.

## Categorías de Errores

### 1. ERRORES DE GRÁFICOS (ERR_G)

| Código | Error | Patrón de Detección | Solución |
|--------|-------|---------------------|----------|
| ERR_G1 | No visualizadas | `File '*.png' not found` | Verificar include_tikz y rutas |
| ERR_G2 | Solapadas | Inspección visual requerida | Ajustar posicionamiento/márgenes |
| ERR_G3 | Renderizado incorrecto | Gráfico visible pero distorsionado | Revisar código TikZ/Python/R |
| ERR_G4 | Tamaño inadecuado | Gráfico muy grande/pequeño | Ajustar scale/width/height |

### 2. ERRORES DE TEXTO (ERR_T)

| Código | Error | Patrón de Detección | Solución |
|--------|-------|---------------------|----------|
| ERR_T1 | LaTeX no compila | `LaTeX failed to compile` | Revisar sintaxis LaTeX |
| ERR_T2 | Encoding incorrecto | Caracteres especiales mal renderizados | Configurar UTF-8 |
| ERR_T3 | Metadatos faltantes | `exname`, `extype` ausentes | Completar Meta-information |

### 3. ERRORES DE ESTRUCTURA (ERR_S)

| Código | Error | Patrón de Detección | Solución |
|--------|-------|---------------------|----------|
| ERR_S1 | Opciones incorrectas | Menos de 4 opciones, duplicados | Regenerar opciones |
| ERR_S2 | Solución no coincide | exsolution no corresponde | Recalcular respuesta correcta |

### 4. ERRORES DE COHERENCIA (ERR_C)

| Código | Error | Patrón de Detección | Solución |
|--------|-------|---------------------|----------|
| ERR_C1 | Coherencia matemática | Fórmulas/cálculos incorrectos | Revisar lógica matemática |
| ERR_C2 | Coherencia imagen-texto | Descripción no coincide con gráfico | Alinear texto y visual |
| ERR_C3 | Coherencia de código | Variables R/Python/TikZ desincronizadas | Sincronizar parámetros |

## Algoritmo de Diagnóstico

```r
diagnosticar_error <- function(mensaje_error, archivo_rmd) {
  
  # Paso 1: Clasificar por patrones conocidos
  if (grepl("not found|File.*png", mensaje_error)) {
    return(list(categoria = "GRAFICOS", codigo = "ERR_G1"))
  }
  
  if (grepl("LaTeX failed|undefined control sequence", mensaje_error)) {
    return(list(categoria = "TEXTO", codigo = "ERR_T1"))
  }
  
  if (grepl("non-numeric argument|abs\\(.*formateado", mensaje_error)) {
    return(list(categoria = "COHERENCIA", codigo = "ERR_C3"))
  }
  
  # Paso 2: Si no hay patrón conocido, analizar archivo
  contenido <- readLines(archivo_rmd)
  
  # Verificar estructura
  if (!any(grepl("^exname:", contenido))) {
    return(list(categoria = "TEXTO", codigo = "ERR_T3"))
  }
  
  # Paso 3: Error no clasificado
  return(list(categoria = "DESCONOCIDO", codigo = "ERR_X"))
}
```

## Flujo de Diagnóstico

```
Error detectado
    ↓
Analizar mensaje de error
    ↓
┌─────────────────────────────────────┐
│ ¿Patrón conocido?                   │
│   Sí → Clasificar automáticamente   │
│   No → Analizar archivo .Rmd        │
└─────────────────────────────────────┘
    ↓
Generar diagnóstico:
  - Categoría del error
  - Código específico
  - Solución recomendada
  - Skill a activar
    ↓
Activar skill de corrección apropiado
```

## Salida del Diagnóstico

```
╔════════════════════════════════════════╗
║     DIAGNÓSTICO DE ERROR               ║
╠════════════════════════════════════════╣
║ Categoría: GRÁFICOS                    ║
║ Código: ERR_G1                         ║
║ Descripción: Gráficas no visualizadas  ║
║ Causa: include_tikz en chunk incorrecto║
║                                        ║
║ Solución recomendada:                  ║
║ → Usar renderizado condicional         ║
║ → Ver: patrones-errores-conocidos.md   ║
║                                        ║
║ Skill a activar: corregir-graficos     ║
╚════════════════════════════════════════╝
```

## 📚 SUBFASE 3A: Corrección Basada en Ejemplos

**OBLIGATORIO: Consultar ejemplos funcionales ANTES de cualquier corrección**

```bash
# Consultar ejemplos funcionales
ls /A-Produccion/Ejemplos-Funcionales-Rmd/

# Buscar patrones similares al error
grep -l "include_tikz" /A-Produccion/Ejemplos-Funcionales-Rmd/*.Rmd
grep -l "py_run_string" /A-Produccion/Ejemplos-Funcionales-Rmd/*.Rmd
```

**Proceso:**
1. Identificar ejemplo funcional similar
2. Extraer patrón de solución
3. Aplicar corrección basada en ejemplo
4. Continuar a SUBFASE 3B

## 🔄 SUBFASE 3B: Revalidación Obligatoria

**OBLIGATORIO: Volver automáticamente a FASE 1**

```
⚠️ DESPUÉS DE APLICAR CORRECCIONES:
→ Ejecutar validar-renderizado (FASE 1)
→ Ejecutar validar-coherencia (FASE 2)
→ Volver a FASE 3 para verificar
→ REPETIR hasta resolver TODOS los errores
```

## 📊 SUBFASE 3C: Documentar Solución (Solo si éxito)

**Solo después de revalidación exitosa:**

1. Documentar error y solución en `.claude/docs/patrones-errores-conocidos.md`
2. Incluir ejemplo funcional utilizado
3. Registrar código antes/después

## ⛔ CONDICIONES CRÍTICAS

1. ❌ **NO terminar** el ciclo con errores sin resolver
2. ❌ **NUNCA** proceder con errores pendientes
3. ✓ **SIEMPRE** consultar ejemplos funcionales ANTES de corregir
4. ✓ **SIEMPRE** ejecutar SUBFASE 3B después de correcciones
5. ✓ **Ejemplos funcionales** = Fuente de verdad ABSOLUTA

## Integración con Ciclo Completo

- **validar-renderizado** → Ejecuta FASE 1
- **validar-coherencia** → Ejecuta FASE 2
- **Este skill** → Ejecuta FASE 3 y subfases
- **SUBFASE 3B** → Vuelve a FASE 1 (ciclo obligatorio)

## Referencias

- `.claude/Mermaid_Chart.txt` (diagrama de flujo oficial)
- `/A-Produccion/Ejemplos-Funcionales-Rmd/` (FUENTE DE VERDAD)
- `.claude/docs/patrones-errores-conocidos.md`

## Ejecución

Cuando el usuario invoca `/diagnosticar-errores`:

1. Recibir errores de FASE 2
2. Clasificar errores (ERR_G, ERR_T, ERR_S, ERR_C)
3. Ejecutar SUBFASE 3A: Consultar ejemplos funcionales
4. Aplicar correcciones basadas en ejemplos
5. Ejecutar SUBFASE 3B: Volver a FASE 1 (revalidación)
6. Si éxito → SUBFASE 3C: Documentar solución
7. Si falla → Repetir desde SUBFASE 3A con solución alternativa

