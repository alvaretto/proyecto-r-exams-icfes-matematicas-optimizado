# Agente Personalizado: Validador de Estilo ICFES

**Nivel**: Intermedio  
**Tipo**: Agente Especializado con Instrucciones de Sistema  
**Propósito**: Comparar archivos .Rmd en edición contra guia_estilo_icfes.md y sugerir correcciones automáticas

---

## Configuración del Agente

```yaml
# .claudecode/agents/validator_icfes.yml
name: "Validador Estilo ICFES"
description: "Agente especializado en validar y corregir archivos .Rmd según estándares ICFES"
temperature: 0.1
model: "claude-3-5-sonnet-20241022"

system_instructions: |
  Eres un validador experto de archivos R-exams ICFES. Tu función es:
  
  1. Leer el archivo .Rmd proporcionado
  2. Consultar la guía de estilo en .claudedoc/guia_estilo_icfes.md
  3. Comparar el archivo contra los estándares definidos
  4. Identificar desviaciones y errores
  5. Sugerir correcciones específicas con código exacto
  
  ÁREAS DE VALIDACIÓN PRIORITARIA:
  - Estructura YAML (header-includes, output, metadatos)
  - Metadatos ICFES (completitud y valores válidos)
  - Chunk de configuración inicial (options, set.seed, use_python)
  - Caracteres especiales LaTeX sin escape
  - Estructura de secciones (Question, Solution, Meta-information)
  - Configuración TikZ/Python si aplica
  
  FORMATO DE RESPUESTA:
  - Lista numerada de problemas encontrados
  - Código antes/después para cada corrección
  - Nivel de severidad (ERROR, ADVERTENCIA, SUGERENCIA)
  - Referencia a sección específica de guia_estilo_icfes.md

context_files:
  - ".claudedoc/guia_estilo_icfes.md"
  - ".claudecode/config.yml"

capabilities:
  - read_files
  - suggest_edits
  - validate_syntax
```

---

## Instrucciones de Uso

### Activación Manual
```bash
# Ejecutar validador en un archivo específico
claude-code agent validate validator_icfes /ruta/al/archivo.Rmd
```

### Integración con Editor
```json
// .vscode/settings.json (si usas VSCode)
{
  "claudeCode.agents": {
    "validator_icfes": {
      "onSave": true,
      "scope": ["**/*.Rmd"],
      "severity": ["ERROR", "ADVERTENCIA"]
    }
  }
}
```

---

## Prompt Template para el Agente

```
Analiza el siguiente archivo .Rmd y valídalo contra la guía de estilo ICFES:

ARCHIVO A VALIDAR:
{{file_content}}

INSTRUCCIONES ESPECÍFICAS:
1. Verifica estructura YAML completa según guia_estilo_icfes.md sección 1
2. Valida metadatos ICFES según sección 2 (todos los campos presentes y válidos)
3. Revisa chunk {r inicio} según sección 3 (options, set.seed, use_python)
4. Detecta caracteres especiales sin escape según sección "ERRORES COMUNES"
5. Verifica secciones Question, Solution, Meta-information según secciones 4-6
6. Si usa Python/TikZ, valida según secciones específicas

FORMATO DE SALIDA:
- Para cada problema encontrado:
  [SEVERIDAD] Problema: Descripción breve
  Ubicación: Línea X / Sección Y
  Código actual: [código problemático]
  Corrección sugerida: [código corregido]
  Referencia: guia_estilo_icfes.md sección Z

- Resumen final:
  Total errores: X
  Total advertencias: Y
  Total sugerencias: Z
```

---

## Ejemplo de Respuesta del Agente

```
VALIDACIÓN COMPLETA - ejercicio_estadistica_n2_v1.Rmd
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

[ERROR] Problema: Campo 'latex_engine' ausente en output.pdf_document
Ubicación: Línea 3 / YAML header
Código actual:
  pdf_document:
    keep_tex: true
Corrección sugerida:
  pdf_document:
    latex_engine: xelatex
    keep_tex: true
Referencia: guia_estilo_icfes.md sección 1

[ADVERTENCIA] Problema: set.seed() con valor fijo
Ubicación: Línea 45 / Chunk inicio
Código actual:
  set.seed(12345)
Corrección sugerida:
  set.seed(sample(1:100000, 1))
Referencia: guia_estilo_icfes.md sección 3

[ADVERTENCIA] Problema: Carácter '&' sin escape en texto
Ubicación: Línea 127 / Sección Question
Código actual:
  La empresa A & B reportó...
Corrección sugerida:
  La empresa A \& B reportó...
Referencia: guia_estilo_icfes.md sección "ERRORES COMUNES"

[SUGERENCIA] Problema: Falta opción 'exshuffle' en Meta-information
Ubicación: Línea 298 / Meta-information
Código actual:
  exsolution: 1000
Corrección sugerida:
  exsolution: 1000
  exshuffle: TRUE
Referencia: guia_estilo_icfes.md sección 6

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
RESUMEN:
Total errores: 1
Total advertencias: 2
Total sugerencias: 1
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

---

## Integración con Flujo de Trabajo

Este agente puede activarse automáticamente:

1. **Al guardar archivo** (.Rmd) en editor
2. **Pre-commit hook** (antes de commit git)
3. **Manual** mediante comando específico
4. **CI/CD** en pipeline de validación
