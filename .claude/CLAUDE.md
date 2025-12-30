# Sistema de Generacion Automatizada de Ejercicios ICFES R/exams

Este proyecto automatiza la creacion y validacion de ejercicios tipo ICFES usando R/exams.

## 🎯 Proposito del Proyecto

Generar ejercicios matematicos de seleccion multiple (SCHOICE) y compuestos (CLOZE) que:
- Cumplan los 6 estandares ICFES (competencias, componentes, afirmaciones, etc.)
- Se rendericen correctamente en 4 formatos: HTML, PDF, DOCX, NOPS
- Generen 250+ versiones unicas aleatorias
- Incluyan graficos dinamicos (TikZ, Python/matplotlib, R/ggplot2)

## ⛔ REGLAS CRITICAS (OBLIGATORIAS)

### Flujo B (Graficador Experto) - OBLIGATORIO si hay graficos
@.claude/rules/flujo-b-obligatorio.md

**Principio**: Si se detectan graficos en el ejercicio, el Flujo B es OBLIGATORIO.
- Deteccion automatica de graficos en enunciado y opciones
- Bloqueo de generacion de .Rmd sin completar Flujo B
- NO hay excepciones

### Proceso Secuencial del Graficador
@.claude/rules/graficador-secuencial.md

**Principio**: Las versiones TikZ, Python, R se generan SECUENCIALMENTE, no simultaneamente.
```
1. TikZ → iterar >=95% + coherencias + aprobacion usuario
2. Python → iterar >=95% + coherencias + aprobacion usuario
3. R → iterar >=95% + coherencias + aprobacion usuario
4. Usuario selecciona version final
```

### 5 Coherencias a Verificar
1. **Coherencia Semantica** - Gramatica correcta
2. **Coherencia Visual-Texto** - Grafico coincide con enunciado
3. **Coherencia Matematica** - Formulas y proporciones correctas
4. **Coherencia de Codigo** - Dinamico, compatible con R-exams
5. **Coherencia General** - Legible, estilo ICFES

## 📚 Documentacion y Reglas

### Workflows y Guias
@.claude/docs/WORKFLOW_PASO_A_PASO.md
@.claude/docs/TROUBLESHOOTING.md
@.claude/docs/TRES_NIVELES_VALIDACION.md

### Reglas Modulares (OBLIGATORIAS)
@.claude/rules/flujo-b-obligatorio.md
@.claude/rules/graficador-secuencial.md
@.claude/rules/ciclo-validacion.md
@.claude/rules/codigo-rmd.md
@.claude/rules/documentacion-verificada.md

## 🛠️ Comandos Manuales y Skills Automáticos

### Commands Manuales (Invocación Explícita)

**Workflow Principal:**
- `/analizar-icfes` - Iniciar análisis ICFES manual
- `/generar-schoice` - Generar ejercicio de selección única
- `/generar-cloze` - Generar ejercicio compuesto
- `/promover-ejercicio` - Promover ejercicio validado a producción

**Graficador Experto (Flujo B - SECUENCIAL):**
- `/auto-refinar-grafico tikz` - Iniciar con TikZ (primero)
- `/auto-refinar-grafico python` - Continuar con Python (despues de TikZ aprobado)
- `/auto-refinar-grafico r` - Finalizar con R (despues de Python aprobado)
- `/estado-graficador` - Consultar estado del workflow grafico
- `/exportar-graficos` - Exportar resultados finales

### Skills Automaticos (Claude los usa segun contexto)

**Validacion (se ejecutan automaticamente):**
- Validar renderizado (FASE 1)
- Validar coherencia (FASE 2)
- Diagnosticar errores (FASE 3)

**Correccion (se ejecutan automaticamente en errores):**
- Corregir graficos (SUBFASE 3A)
- Corregir errores de imagen TikZ

**Graficador (se ejecutan SECUENCIALMENTE - ver reglas):**
- Analizar imagen matematica → Detectar si requiere Flujo B
- Generar codigo TikZ → Iterar → Coherencias → Aprobacion
- Generar codigo Python → Iterar → Coherencias → Aprobacion
- Generar codigo R → Iterar → Coherencias → Aprobacion
- Usuario selecciona version final

## 📁 Estructura del Repositorio

```
A-Produccion/
├── Nuevos-Ejercicios/           # Ejercicios validados listos
├── En-Desarrollo/               # Ejercicios en creación/validación
└── Ejemplos-Funcionales-Rmd/    # FUENTE DE VERDAD para SUBFASE 3A

outputs/                         # Archivos temporales de renderizado
├── output_tikz.tex
├── output_python.py
└── output_r.R

.claude/
├── CLAUDE.md                    # Este archivo (memory principal)
├── rules/                       # Reglas modulares (OBLIGATORIAS)
├── skills/                      # Agent Skills (invocación automática)
├── commands/                    # Slash Commands (invocación manual)
├── agents/                      # Agentes especializados
├── docs/                        # Documentación técnica
└── hooks/                       # Hooks de validación
```

## ⚙️ Hooks Configurados

Ver configuración completa en @.claude/settings.json

### Pre-Edit/Write: Regla de Oro
@.claude/rules/codigo-rmd.md

### Post-Bash: Validación exams2*
@.claude/rules/ciclo-validacion.md

## 🔗 Referencias Rápidas

- **Errores conocidos**: @.claude/docs/patrones-errores-conocidos.md
- **Casos resueltos**: @.claude/docs/casos-resueltos/
- **Nomenclatura**: @.claude/docs/NOMENCLATURA_ARCHIVOS_RMD.md
- **Python/reticulate**: @.claude/docs/MEJORES_PRACTICAS_PYTHON_RETICULATE.md

## 📊 Metadatos y Reglas de Código

Ver especificaciones completas en @.claude/rules/codigo-rmd.md

---

**Ultima actualizacion**: 2025-12-30
**Version**: 2.2 (Flujo B obligatorio + Secuencial)
**Basado en**: Documentacion oficial Claude Code (nov 2025)

## Cambios v2.2
- Flujo B (Graficador Experto) ahora es OBLIGATORIO cuando hay graficos
- Proceso SECUENCIAL: TikZ → Python → R (no simultaneo)
- 5 coherencias a verificar antes de aprobacion
- Bloqueo de generacion .Rmd si Flujo B incompleto
