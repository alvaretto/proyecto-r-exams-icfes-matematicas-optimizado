---
name: generar-cloze
description: >
  Genera ejercicio R-exams tipo CLOZE (pregunta compuesta) METACOGNITIVO.
  TODO ejercicio CLOZE DEBE aplicar Progressive Disclosure con minimo 4 partes.
  Usa cuando el analisis ICFES indica tipo cloze, necesites pregunta con multiples partes,
  o quieras combinar opciones multiples + respuestas numericas en un solo ejercicio.
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

# Generador de Ejercicios CLOZE Metacognitivos

## REGLA CRÍTICA

**⚠️ TODO ejercicio CLOZE DEBE ser metacognitivo con Progressive Disclosure de minimo 4 partes.**

Ver regla completa: `.claude/rules/ejercicios-metacognitivos.md`

## Decision Tree

```
User task -> Tiene analisis ICFES?
    |-- No -> Ejecutar /analizar-icfes primero
    +-- Si -> Tipo = cloze confirmado?
        |-- No -> Usar /generar-schoice
        +-- Si -> Definir estructura Progressive Disclosure
            |-- Parte 1: schoice (Identificar error)
            |-- Parte 2: num (Calcular correcto)
            |-- Parte 3: mchoice (Evaluar afirmaciones)
            +-- Parte 4: schoice V/F (Transferir)
            +-- Consultar ejemplos funcionales similares
                 +-- Generar .Rmd con nomenclatura oficial
                    +-- Validar: Rscript scripts/validar-renderizado.R
```

## Cuando usar CLOZE vs SCHOICE

**Usa CLOZE (obligatorio) cuando:**

- Problema requiere multiples niveles cognitivos en secuencia
- Necesitas Progressive Disclosure completo (identificar → calcular → evaluar → transferir)
- Ejercicio tiene varios pasos a responder por separado
- Nivel de dificultad 3 o 4
- Competencia = Argumentacion

**Usa SCHOICE cuando:**

- Solo hay 1 aspecto a evaluar
- Nivel de dificultad 1 o 2 (pero SIEMPRE metacognitivo)

## Proceso paso a paso

### PASO 0: Definir estructura Progressive Disclosure (OBLIGATORIO)

**ANTES de cualquier otra accion, planificar las 4 partes minimas:**

```
Parte 1 (schoice): IDENTIFICAR el error conceptual
    ↓ Bloom: Analizar | DOK: 3
Parte 2 (num): CALCULAR la respuesta correcta
    ↓ Bloom: Aplicar | DOK: 2
Parte 3 (mchoice): EVALUAR afirmaciones sobre el concepto
    ↓ Bloom: Evaluar | DOK: 3
Parte 4 (schoice V/F): TRANSFERIR a caso especifico
    | Bloom: Analizar/Evaluar | DOK: 3
```

**Tipos de gap disponibles:**

| Tipo | Cuando usar | Ejemplo |
|------|-------------|---------|
| schoice | Seleccion unica (errores, V/F) | A, B, C, D |
| mchoice | Seleccion multiple (afirmaciones) | Checkbox |
| num | Respuesta numerica | 42.5 |
| string | Texto libre | "exponencial" |

### PASO 1: Verificar analisis ICFES

Confirmar: Nivel, Competencia, Componente, Tipo = cloze.

### PASO 2: Consultar ejemplos funcionales METACOGNITIVOS

NUNCA generar codigo sin consultar ejemplos primero.

```bash
# Buscar ejemplos metacognitivos CLOZE existentes
ls A-Produccion/03-En-Produccion/**/*metacognitivo*cloze*.Rmd

# O el ejemplo canónico
cat A-Produccion/03-En-Produccion/06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/04-Medidas-De-Tendencia-Central/Media/Promedios-Borrados/promedios_borrados_metacognitivo_argumentacion_n3_cloze_v1.Rmd
```

### PASO 3: Definir pool de errores conceptuales (OBLIGATORIO)

**ANTES de generar el codigo, documentar minimo 4-6 errores:**

```r
errores_conceptuales <- list(
  list(
    codigo = "XXX-YYY-01",
    nombre = "Nombre descriptivo",
    descripcion_corta = "...",
    descripcion_larga = "...",
    causa_raiz = "...",
    calcula = function(promedio, suma_total, suma_conocidas, n_total, n_desconocidos) {
      # Retorna el valor erroneo que produciria este error
    }
  ),
  # ... minimo 4-6 errores
)
```

### PASO 4: Definir pool de afirmaciones (OBLIGATORIO para Parte 3)

```r
pool_afirmaciones_verdaderas <- list(
  "Afirmacion verdadera 1 sobre el concepto",
  "Afirmacion verdadera 2 sobre el concepto",
  # ... minimo 6
)

pool_afirmaciones_falsas <- list(
  "Afirmacion falsa 1 (error conceptual comun)",
  "Afirmacion falsa 2 (error conceptual comun)",
  # ... minimo 6
)
```

### PASO 5: Definir pool de enunciados V/F (OBLIGATORIO para Parte 4)

```r
pool_vf <- list(
  list(enunciado = "...", es_verdadero = TRUE),
  list(enunciado = "...", es_verdadero = FALSE),
  # ... minimo 4
)
```

### PASO 6: Generar nombre con nomenclatura

Formato: `[ejercicio]_metacognitivo_[competencia]_n[nivel]_cloze_v[version].Rmd`

| Parte | Valores |
|-------|---------|
| `[ejercicio]` | Descriptivo snake_case |
| `metacognitivo` | OBLIGATORIO en el nombre |
| `[competencia]` | `argumentacion` (tipico para CLOZE) |
| `n[nivel]` | `n3` / `n4` (CLOZE metacognitivo = minimo n3) |
| `v[version]` | `v1`, `v2`, ... |

Ver: .claude/docs/NOMENCLATURA_ARCHIVOS_RMD.md

### PASO 7: Crear carpeta en En-Desarrollo

```bash
mkdir -p A-Produccion/02-En-Desarrollo/[nombre_ejercicio]
```

### PASO 8: Generar codigo .Rmd CLOZE METACOGNITIVO

Ver [anatomia CLOZE](references/anatomia-cloze.md) para estructura de GAPS.
Ver [anatomia metacognitiva](references/anatomia-metacognitiva.md) para secciones obligatorias.

**Estructura OBLIGATORIA del Question:**

```markdown
Question
========

[Contexto realista con datos dinámicos]

[Tabla o gráfico con datos]

[Descripcion del error cometido por otro estudiante]

**Parte 1.** ¿Cual error conceptual cometio [estudiante]?

##ANSWER1##

**Parte 2.** ¿Cual es el valor correcto?

##ANSWER2##

**Parte 3.** Seleccione las afirmaciones correctas sobre [concepto].

##ANSWER3##

**Parte 4.** Determine si es verdadera o falsa: [enunciado especifico]

##ANSWER4##
```

**Estructura OBLIGATORIA del Solution:**

```markdown
Solution
========

### Analisis del Error (Parte 1)
**Error identificado:** [descripcion_larga]
**Codigo de error:** [codigo]
**Causa raiz:** [causa_raiz]

### Procedimiento Correcto (Parte 2)
**Paso 1:** [descripcion + formula LaTeX]
$$...$$
**Paso 2:** ...

### Propiedades del Concepto (Parte 3)
- Afirmacion 1: [VERDADERA/FALSA] porque...
- Afirmacion 2: ...

### Caso Especifico (Parte 4)
[enunciado] → **[Verdadero/Falso]** porque...

### Reflexion Metacognitiva
[reflexion aleatoria del pool]
```

### PASO 9: Validar renderizado

```bash
Rscript .claude/skills/generar-schoice/scripts/validar-renderizado.R ejercicio.Rmd
```

NOTA: NOPS fallara si hay gaps tipo num/string (esperado).

### PASO 10: Verificar checklist metacognitivo CLOZE

**Pre-promocion, verificar:**

- [ ] Minimo 4 partes con Progressive Disclosure
- [ ] Pool de errores conceptuales con codigos (minimo 4)
- [ ] Pool de afirmaciones V/F (minimo 6 de cada)
- [ ] Pool de enunciados V/F (minimo 4)
- [ ] Respuesta erronea ≠ respuesta correcta
- [ ] Solucion incluye analisis de error
- [ ] Solucion incluye reflexion metacognitiva
- [ ] Metadatos exclozetype, exsolution, extol consistentes
- [ ] Metadatos DOK, Bloom, SOLO presentes
- [ ] DOK >= 3

### PASO 11: Promocion

```bash
/promover-ejercicio [nombre_ejercicio]
```

## Condiciones criticas

### Pre-generacion

- Analisis ICFES completado con tipo = cloze
- **Estructura Progressive Disclosure planificada (4 partes)**
- **Pool de errores conceptuales definido (minimo 4)**
- **Pool de afirmaciones V/F definido (minimo 6+6)**
- **Pool de enunciados V/F definido (minimo 4)**
- Ejemplo funcional CLOZE metacognitivo identificado y leido
- Nomenclatura calculada (incluye "metacognitivo" y "cloze")
- Carpeta destino creada

### Durante generacion

- Funcion `generar_datos()` con aleatorizacion completa
- **Pool de errores con funciones `calcula`**
- **Pool de reflexiones metacognitivas**
- GAPS numerados secuencialmente (1, 2, 3, 4)
- exclozetype con tipos por gap separados por `|`
- exsolution con respuestas por gap separadas por `|`
- extol con tolerancias por gap separadas por `|`
- **Metadatos cognitivos: DOK, Bloom, SOLO**
- Formato espanol en todos los numeros

### Post-generacion

- HTML, PDF, DOCX: OK
- NOPS: Puede fallar (esperado si hay gaps num/string)
- **Respuesta erronea diferente de correcta**
- Test de diversidad > 250 versiones unicas
- **Solucion incluye todas las subsecciones obligatorias**

NO terminar con errores inesperados.

## Antipatrones PROHIBIDOS

### 1. CLOZE con menos de 4 partes

```markdown
❌ Parte 1: Calcule X
   Parte 2: Calcule Y
```

**Correccion:** Siempre 4 partes con Progressive Disclosure:
```markdown
✓ Parte 1: Identificar error
✓ Parte 2: Calcular correcto
✓ Parte 3: Evaluar afirmaciones
✓ Parte 4: Transferir V/F
```

### 2. Partes sin progresion cognitiva

```markdown
❌ Parte 1: Calcule area
   Parte 2: Calcule perimetro
   Parte 3: Calcule volumen
   Parte 4: Calcule diagonal
```

**Correccion:** Cada parte sube nivel cognitivo:
```markdown
✓ Parte 1: Identificar error en calculo de area (Analizar)
✓ Parte 2: Calcular area correcta (Aplicar)
✓ Parte 3: Evaluar propiedades del area (Evaluar)
✓ Parte 4: Aplicar a caso especifico (Transferir)
```

### 3. Afirmaciones sin base conceptual

```markdown
❌ pool_afirmaciones_falsas <- list(
     "El resultado es 42",
     "La respuesta es incorrecta"
   )
```

**Correccion:** Afirmaciones basadas en errores conceptuales:
```markdown
✓ pool_afirmaciones_falsas <- list(
     "El promedio siempre es uno de los valores del conjunto",
     "Si se duplica cada dato, el promedio se mantiene igual"
   )
```

## Metadatos OBLIGATORIOS CLOZE Metacognitivo

```yaml
exname: [nombre]_metacognitivo_argumentacion_n3_cloze_v1
extype: cloze
exclozetype: schoice|num|mchoice|schoice
exsolution: [sol_p1]|[sol_p2]|[sol_p3]|[sol_p4]
exshuffle: TRUE  # Nota: FALSE solo aplica a SCHOICE con PNGs gráficos (ver graficos-como-opciones.md)
extol: 0|0.01|0|0

exextra[DOK]: 3
exextra[Bloom]: Evaluar
exextra[SOLO]: Relacional-Extendido
exextra[TipoMetacognicion]: progressive_disclosure
```

## Referencias

- [Anatomia CLOZE](references/anatomia-cloze.md) - Estructura GAPS y metadatos
- [Anatomia Metacognitiva](references/anatomia-metacognitiva.md) - Las 8 secciones obligatorias
- Regla Metacognitiva: .claude/rules/ejercicios-metacognitivos.md
- generar-schoice: .claude/skills/generar-schoice/SKILL.md (estructura base)
- Ejemplo Canonico: A-Produccion/03-En-Produccion/.../promedios_borrados_metacognitivo_argumentacion_n3_cloze_v1.Rmd
- Nomenclatura: .claude/docs/NOMENCLATURA_ARCHIVOS_RMD.md
- Ciclo Validacion: .claude/rules/ciclo-validacion.md

## Integracion con otros skills

```
analizar-icfes -> generar-cloze -> validar-renderizado -> promover-ejercicio
                       ↑
                       |
              Regla ejercicios-metacognitivos.md OBLIGATORIA
              + Progressive Disclosure de 4 partes MINIMO
```
