---
name: generar-schoice
description: >
  Genera ejercicio R-exams tipo SCHOICE (seleccion unica).
  Usa cuando el analisis ICFES indica schoice, necesites ejercicio de opciones multiples,
  o quieras crear pregunta con 1 respuesta correcta y 3+ distractores.
  SIEMPRE consulta ejemplos funcionales ANTES de generar codigo.
license: Proyecto Educativo - IE Pedacito de Cielo
compatibility: Requiere R (>= 4.0), tinytex, paquetes exams y tidyverse. Linux/macOS.
metadata:
  author: alvaretto
  version: "2.1"
  language: es
allowed-tools:
  - Read
  - Write
  - Edit
  - Grep
  - Glob
  - Bash(ls:*)
  - Bash(mkdir:*)
  - Bash(Rscript:*)
---

# Generador de Ejercicios SCHOICE

## Decision Tree

```
User task -> Tiene analisis ICFES?
    |-- No -> Ejecutar /analizar-icfes primero
    +-- Si -> Tiene graficos?
        |-- Si -> Preguntar version grafica (TikZ/Python/R)
        |        +-- Consultar ejemplos funcionales similares con grafico
        +-- No -> Consultar ejemplos funcionales similares sin grafico
                 +-- Generar .Rmd con nomenclatura oficial
                    +-- Validar: Rscript scripts/validar-renderizado.R
```

## Proceso paso a paso

### PASO 0: Seleccion de version grafica (si aplica)

Preguntar al usuario:

1. TikZ (imagen externa)
2. Python (reticulate)
3. R/ggplot2 (RECOMENDADO - nativo)

NO continuar sin respuesta del usuario.

### PASO 1: Verificar analisis ICFES

Confirmar que existe clasificacion previa: Nivel, Competencia, Componente, Tipo = schoice.

### PASO 2: Consultar ejemplos funcionales

NUNCA generar codigo sin consultar ejemplos primero.

```bash
ls /A-Produccion/Ejemplos-Funcionales-Rmd/*.Rmd
grep -l "Componente.*[similar]" /A-Produccion/Ejemplos-Funcionales-Rmd/*.Rmd
```

Leer ejemplo completo y copiar patrones.

### PASO 3: Generar nombre con nomenclatura

Formato: `[ejercicio]_[componente]_[competencia]_n[nivel]_v[version].Rmd`

| Parte | Valores |
|-------|---------|
| `[ejercicio]` | Descriptivo snake_case |
| `[componente]` | `geometrico_metrico` / `numerico_variacional` / `aleatorio` |
| `[competencia]` | `interpretacion_representacion` / `formulacion_ejecucion` / `argumentacion` |
| `n[nivel]` | `n1` / `n2` / `n3` / `n4` |
| `v[version]` | `v1`, `v2`, ... |

Ver: .claude/docs/NOMENCLATURA_ARCHIVOS_RMD.md

### PASO 4: Crear carpeta en En-Desarrollo

```bash
mkdir -p /A-Produccion/En-Desarrollo/[nombre_ejercicio]
```

### PASO 5: Generar codigo .Rmd

Copiar estructura del ejemplo funcional similar.
Ver [anatomia completa del .Rmd](references/anatomia-rmd.md) para las 7 secciones obligatorias.

### PASO 6: Validar renderizado

Run `scripts/validar-renderizado.R --help` first, then:

```bash
Rscript .claude/skills/generar-schoice/scripts/validar-renderizado.R ejercicio.Rmd
```

### PASO 7: Ciclo de correccion

Si hay errores, consultar [errores comunes](references/errores-comunes.md).
Buscar solucion en ejemplos funcionales. Volver a PASO 6.

### PASO 8: Promocion

```bash
/promover-ejercicio [nombre_ejercicio]
```

## Condiciones criticas

### Pre-generacion

- Analisis ICFES completado
- Ejemplo funcional similar identificado y leido
- Nomenclatura calculada
- Carpeta destino creada

### Durante generacion

- Funcion `generar_datos()` con aleatorizacion
- Distractores basados en errores conceptuales
- Formato espanol en todos los numeros: `format(x, big.mark = ".", decimal.mark = ",")`
- Metadatos ICFES completos (6 dimensiones)
- `exshuffle: TRUE` obligatorio

### Post-generacion

- Renderizado exitoso en 4 formatos (HTML, PDF, DOCX, NOPS)
- Coherencia matematica pregunta-respuesta-distractores
- Test de diversidad > 250 versiones unicas

NO terminar con errores pendientes.

## Referencias

- [Anatomia .Rmd](references/anatomia-rmd.md) - Las 7 secciones obligatorias
- [Errores comunes](references/errores-comunes.md) - Patrones incorrecto/correcto
- [Ejemplos completos](references/ejemplos.md) - Nivel 1 aritmetica + Nivel 3 estadistica
- Ejemplos Funcionales: /A-Produccion/Ejemplos-Funcionales-Rmd/
- Nomenclatura: .claude/docs/NOMENCLATURA_ARCHIVOS_RMD.md
- Ciclo Validacion: .claude/rules/ciclo-validacion.md
- Metadatos: .claude/rules/codigo-rmd.md

## Integracion con otros skills

```
analizar-icfes -> generar-schoice -> validar-renderizado -> promover-ejercicio
```
