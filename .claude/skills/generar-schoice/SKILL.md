---
name: generar-schoice
description: >
  Genera ejercicio R-exams tipo SCHOICE (seleccion unica) METACOGNITIVO.
  TODO ejercicio debe aplicar Progressive Disclosure y analisis de errores conceptuales.
  Usa cuando el analisis ICFES indica schoice, necesites ejercicio de opciones multiples,
  o quieras crear pregunta con 1 respuesta correcta y 3+ distractores.
  SIEMPRE consulta ejemplos funcionales ANTES de generar codigo.
license: Proyecto Educativo - IE Pedacito de Cielo
compatibility: Requiere R (>= 4.0), tinytex, paquetes exams y tidyverse. Linux/macOS.
metadata:
  author: alvaretto
  version: "3.0"
  language: es
  model_recommendation: opus
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

# Generador de Ejercicios SCHOICE Metacognitivos

## REGLA CRÍTICA

**⚠️ TODO ejercicio SCHOICE DEBE ser metacognitivo con Progressive Disclosure.**

Ver regla completa: `.claude/rules/ejercicios-metacognitivos.md`

## Decision Tree

```
User task -> Tiene analisis ICFES?
    |-- No -> Ejecutar /analizar-icfes primero
    +-- Si -> Tiene graficos?
        |-- Si -> Preguntar version grafica (TikZ/Python/R)
        |        +-- Consultar ejemplos funcionales similares con grafico
        +-- No -> Consultar ejemplos funcionales similares sin grafico
                 +-- Seleccionar PATRON METACOGNITIVO
                    |-- Patron 1: Analisis de Error Ajeno
                    |-- Patron 2: Evaluacion de Afirmacion
                    +-- Patron 3: Comparacion de Procedimientos
                 +-- Generar .Rmd con nomenclatura oficial
                    +-- Validar: Rscript scripts/validar-renderizado.R
```

## Proceso paso a paso

### PASO 0: Seleccionar patron metacognitivo (OBLIGATORIO)

**ANTES de cualquier otra accion, definir el patron metacognitivo:**

| Patron | Cuando usar | Bloom | DOK |
|--------|-------------|-------|-----|
| **Analisis de Error Ajeno** | Ejercicios de calculo donde hay errores comunes | Analizar/Evaluar | 3 |
| **Evaluacion de Afirmacion** | Ejercicios conceptuales sobre propiedades | Evaluar | 3 |
| **Comparacion de Procedimientos** | Ejercicios con multiples metodos de solucion | Analizar | 3 |

**Ejemplos:**

```
Patron 1 (Error Ajeno):
"Juan calculo el area de un circulo con radio 5 y obtuvo 10π.
 ¿Cual error conceptual cometio Juan?"

Patron 2 (Afirmacion):
"Maria afirma: 'Si duplico el radio, el area se duplica'.
 ¿Por que esta afirmacion es INCORRECTA?"

Patron 3 (Comparacion):
"Tres estudiantes resolvieron 2x + 5 = 13:
 Estudiante A: x = 4
 Estudiante B: x = 9
 Estudiante C: x = 6
 ¿Cual aplico correctamente el despeje?"
```

### PASO 0.5: Seleccion de version grafica (si aplica)

Preguntar al usuario:

1. TikZ (imagen externa)
2. Python (reticulate)
3. R/ggplot2 (RECOMENDADO - nativo)

NO continuar sin respuesta del usuario.

### PASO 1: Verificar analisis ICFES

Confirmar que existe clasificacion previa: Nivel, Competencia, Componente, Tipo = schoice.

### PASO 2: Consultar ejemplos funcionales METACOGNITIVOS

NUNCA generar codigo sin consultar ejemplos primero.

```bash
# Buscar ejemplos metacognitivos existentes
ls A-Produccion/03-En-Produccion/**/*metacognitivo*.Rmd

# O ejemplos funcionales generales
ls A-Produccion/Ejemplos-Funcionales-Rmd/*.Rmd
```

Leer ejemplo completo y copiar patrones.

### PASO 3: Definir pool de errores conceptuales (OBLIGATORIO)

**ANTES de generar el codigo, documentar minimo 4-6 errores:**

```r
errores_conceptuales <- list(
  list(
    codigo = "XXX-YYY-01",          # Ej: ALG-OPE-01
    nombre = "Nombre descriptivo",
    descripcion_corta = "...",       # Para opciones (max 80 chars)
    descripcion_larga = "...",       # Para solucion (detallada)
    causa_raiz = "...",              # Diagnostico pedagogico
    calcula = function(...) { ... }  # Funcion que produce el distractor
  ),
  # ... mas errores
)
```

**Taxonomia de codigos:**

| Prefijo | Area |
|---------|------|
| ALG | Algebra |
| ARI | Aritmetica |
| EST | Estadistica |
| GEO | Geometria |
| FUN | Funciones |

### PASO 4: Generar nombre con nomenclatura

Formato: `[ejercicio]_metacognitivo_[competencia]_n[nivel]_schoice_v[version].Rmd`

| Parte | Valores |
|-------|---------|
| `[ejercicio]` | Descriptivo snake_case |
| `metacognitivo` | OBLIGATORIO en el nombre |
| `[competencia]` | `interpretacion` / `formulacion` / `argumentacion` |
| `n[nivel]` | `n2` / `n3` / `n4` (metacognitivo = minimo n2) |
| `v[version]` | `v1`, `v2`, ... |

Ver: .claude/docs/NOMENCLATURA_ARCHIVOS_RMD.md

### PASO 5: Crear carpeta en En-Desarrollo

```bash
mkdir -p A-Produccion/02-En-Desarrollo/[nombre_ejercicio]
```

### PASO 6: Generar codigo .Rmd METACOGNITIVO

Ver [anatomia metacognitiva del .Rmd](references/anatomia-metacognitiva.md) para las 8 secciones obligatorias.

**Estructura OBLIGATORIA:**

```
1. ENCABEZADO YAML (con taxonomias cognitivas)
2. CHUNK setup
3. CHUNK data_generation (con pool de errores)
4. CHUNK version_diversity_test
5. CHUNK validaciones_matematicas
6. SECCION Question (patron metacognitivo)
7. SECCION Solution (analisis de error + reflexion)
8. META-INFORMATION (DOK, Bloom, SOLO obligatorios)
```

### PASO 7: Validar renderizado

```bash
Rscript .claude/skills/generar-schoice/scripts/validar-renderizado.R ejercicio.Rmd
```

### PASO 8: Verificar checklist metacognitivo

**Pre-promocion, verificar:**

- [ ] Pool de errores conceptuales con codigos
- [ ] Respuesta erronea ≠ respuesta correcta
- [ ] Distractores unicos
- [ ] Solucion incluye analisis de error
- [ ] Solucion incluye reflexion metacognitiva
- [ ] Metadatos DOK, Bloom, SOLO presentes
- [ ] DOK >= 2 (preferible 3)

### PASO 9: Promocion

```bash
/promover-ejercicio [nombre_ejercicio]
```

## Condiciones criticas

### Pre-generacion

- Analisis ICFES completado
- **Patron metacognitivo seleccionado**
- **Pool de errores conceptuales definido (minimo 4)**
- Ejemplo funcional similar identificado y leido
- Nomenclatura calculada (incluye "metacognitivo")
- Carpeta destino creada

### Durante generacion

- Funcion `generar_datos()` con aleatorizacion
- **Pool de errores con funciones `calcula`**
- **Pool de reflexiones metacognitivas**
- Distractores basados en errores conceptuales (NUNCA aleatorios)
- Formato espanol en todos los numeros
- Metadatos ICFES completos (6 dimensiones)
- **Metadatos cognitivos: DOK, Bloom, SOLO**
- `exshuffle: TRUE` obligatorio (excepción: FALSE en SCHOICE con opciones gráficas PNG — ver `graficos-como-opciones.md`)

### Post-generacion

- Renderizado exitoso en 4 formatos (HTML, PDF, DOCX, NOPS)
- Coherencia matematica pregunta-respuesta-distractores
- **Respuesta erronea diferente de correcta**
- Test de diversidad > 250 versiones unicas
- **Solucion incluye todas las subsecciones obligatorias**

NO terminar con errores pendientes.

## Antipatrones PROHIBIDOS

### 1. Ejercicio puramente procedimental

```markdown
❌ "Calcula el area de un rectangulo con base 8 cm."
```

**Correccion:** Convertir a analisis de error:
```markdown
✓ "Un estudiante calculo 8 + 5 = 13 como area. ¿Cual fue su error?"
```

### 2. Distractores aleatorios

```r
❌ distractores <- respuesta + sample(-10:10, 3)
```

**Correccion:** Usar pool de errores:
```r
✓ distractores <- sapply(errores_conceptuales, function(e) e$calcula(...))
```

### 3. Solucion sin analisis

```markdown
❌ Solution
========
La respuesta correcta es 40.
```

**Correccion:** Incluir analisis completo:
```markdown
✓ ### Analisis del Error
✓ **Error identificado:** ...
✓ **Causa raiz:** ...
✓ ### Procedimiento Correcto
✓ **Paso 1:** ...
✓ ### Reflexion Metacognitiva
✓ ...
```

## Referencias

- [Anatomia Metacognitiva .Rmd](references/anatomia-metacognitiva.md) - Las 8 secciones obligatorias
- [Anatomia .Rmd basica](references/anatomia-rmd.md) - Estructura general
- [Errores comunes](references/errores-comunes.md) - Patrones incorrecto/correcto
- [Ejemplos completos](references/ejemplos.md) - Nivel 1 aritmetica + Nivel 3 estadistica
- Regla Metacognitiva: .claude/rules/ejercicios-metacognitivos.md
- Ejemplos Funcionales: A-Produccion/Ejemplos-Funcionales-Rmd/
- Nomenclatura: .claude/docs/NOMENCLATURA_ARCHIVOS_RMD.md
- Ciclo Validacion: .claude/rules/ciclo-validacion.md
- Metadatos: .claude/rules/codigo-rmd.md

## Integracion con otros skills

```
analizar-icfes -> generar-schoice -> validar-renderizado -> promover-ejercicio
                       ↑
                       |
              Regla ejercicios-metacognitivos.md OBLIGATORIA
```
