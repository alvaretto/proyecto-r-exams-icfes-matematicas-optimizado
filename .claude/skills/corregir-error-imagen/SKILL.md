---
name: corregir-error-imagen
description: >
  Ejecuta SUBFASE 3A para ERR_G1 (imagenes faltantes). Aplica patron de renderizado condicional
  para resolver errores "File not found" con include_tikz().
  Usa cuando aparece "File not found" en renderizado, "imagen faltante", "ERR_G1", "corregir include_tikz".
license: Proyecto Educativo - IE Pedacito de Cielo
compatibility: Requiere R-exams con include_tikz(). Linux/macOS.
metadata:
  author: alvaretto
  version: "2.1"
  language: es
  model_recommendation: sonnet
allowed-tools:
  - Read
  - Write
  - Edit
  - Bash(ls:*)
  - Bash(grep:*)
---

> **ROUTING**: Este skill tiene `model_recommendation: sonnet`. Claude DEBE delegarlo via `Task(subagent_type="general-purpose", model="sonnet")` pasando las instrucciones completas, el archivo .Rmd y el mensaje de error "File not found" como contexto. Ver regla `.claude/rules/modelo-routing-obligatorio.md`.

# SUBFASE 3A - Correccion de Imagenes Faltantes (ERR_G1)

## Decision Tree

```
Error detectado: "File '*.png' not found"?
    |
    +-> NO: Usar skill corregir-graficos (ERR_G2-G4)
    |
    +-> SI: Localizar chunk con include_tikz()
            |
            +-> Consultar ejemplos funcionales
            |
            +-> Aplicar renderizado condicional
            |
            +-> SUBFASE 3B: Revalidar
                 |
                 +-> Corregido? SI -> SUBFASE 3C: Documentar
                 +-> Corregido? NO -> Revisar sintaxis
```

## Proceso paso a paso

### PASO 0: Consultar ejemplos funcionales (OBLIGATORIO)

```bash
ls /A-Produccion/Ejemplos-Funcionales-Rmd/

# NO buscar "is_latex_output" como ejemplo a copiar: el patrón que lo usa para
# renderizado condicional fue RETIRADO el 2026-08-15 (pierde la figura en el PDF).
# Buscar en cambio el sustituto vigente:
grep -l 'markup = "markdown"' /A-Produccion/Ejemplos-Funcionales-Rmd/*.Rmd

grep -l "include_tikz" /A-Produccion/Ejemplos-Funcionales-Rmd/*.Rmd
```

### PASO 1: Detectar el error

Buscar en log de compilacion:

```
File '*.png' not found
```

### PASO 2: Identificar chunk problematico

Localizar chunks con `include_tikz()` que generan el archivo PNG faltante.

### PASO 3: Aplicar renderizado condicional

Ver [patron de renderizado condicional](references/patron-renderizado-condicional.md) para:

- Codigo antes/despues completo
- Separar generacion de visualizacion
- Usar `include_tikz(..., markup = "markdown")` en una sola llamada, SIN ramificar por
  `knitr::is_latex_output()` (retirado 2026-08-15: es SIEMPRE FALSE bajo R/exams y el
  condicional pierde la figura en el PDF — ver `.claude/rules/codigo-rmd.md` regla #1)

### PASO 4: SUBFASE 3B - Revalidar

```
OBLIGATORIO: Volver automaticamente a FASE 1
-> exams2pdf() debe compilar sin errores
-> exams2html() debe mostrar imagen
-> REPETIR si persisten errores
```

### PASO 5: SUBFASE 3C - Documentar (solo si exito)

1. Documentar en `patrones-errores-conocidos.md`
2. Incluir ejemplo funcional utilizado
3. Registrar codigo antes/despues

## Algoritmo de correccion

1. Eliminar `include_tikz()` del chunk de generacion (dejar solo la construccion del `tikz_code`)
2. Crear nuevo chunk `results='asis'` con UNA sola llamada a
   `include_tikz(tikz_code, ..., markup = "markdown")`, SIN condicional (NO usar
   `is_latex_output()`: es SIEMPRE FALSE bajo R/exams, ver regla #1 de `codigo-rmd.md`)
3. `markup = "markdown"` es el que hace que pandoc enrute la figura a los 5 destinos
   (html/pdf/docx/nops/moodle) sin necesidad de ramificar por formato

## Condiciones criticas

- NO terminar con ERR_G1 sin resolver
- SIEMPRE consultar ejemplos funcionales ANTES de corregir
- SIEMPRE ejecutar SUBFASE 3B despues de correcciones
- Ejemplos funcionales = Fuente de verdad ABSOLUTA

## Referencias

- [Patron renderizado condicional](references/patron-renderizado-condicional.md) - Codigo completo
- Fuente de verdad: `/A-Produccion/Ejemplos-Funcionales-Rmd/`
- Documentacion: `.claude/docs/patrones-errores-conocidos.md`
- Skill general: corregir-graficos (ERR_G1-G4)

## Integracion con Ciclo de Validacion

```
diagnosticar-errores
    |
    +-> Detecta ERR_G1 (File not found)
    |
    v
corregir-error-imagen <- ESTE SKILL
    |
    +-> SUBFASE 3A: Aplica renderizado condicional
    |
    v
validar-renderizado (FASE 1)
    |
    +-> SUBFASE 3B: Revalida en PDF y HTML
    |
    +-> Si corregido -> SUBFASE 3C: Documenta
    +-> Si falla -> Revisar sintaxis TikZ
```
