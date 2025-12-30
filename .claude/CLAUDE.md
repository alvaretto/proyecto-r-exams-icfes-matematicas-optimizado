# Sistema de Generación Automatizada de Ejercicios ICFES R/exams

Este proyecto automatiza la creación y validación de ejercicios tipo ICFES usando R/exams.

## 🎯 Propósito del Proyecto

Generar ejercicios matemáticos de selección múltiple (SCHOICE) y compuestos (CLOZE) que:
- Cumplan los 6 estándares ICFES (competencias, componentes, afirmaciones, etc.)
- Se rendericen correctamente en 4 formatos: HTML, PDF, DOCX, NOPS
- Generen 250+ versiones únicas aleatorias
- Incluyan gráficos dinámicos (TikZ, Python/matplotlib, R/ggplot2)

## 📚 Documentación y Reglas

### Workflows y Guías
@.claude/docs/WORKFLOW_PASO_A_PASO.md
@.claude/docs/TROUBLESHOOTING.md
@.claude/docs/TRES_NIVELES_VALIDACION.md

### Reglas Modulares (OBLIGATORIAS)
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

**Graficador Experto:**
- `/auto-refinar-grafico` - Iteración automática hasta umbral
- `/estado-graficador` - Consultar estado del workflow gráfico
- `/exportar-graficos` - Exportar resultados finales

### Skills Automáticos (Claude los usa según contexto)

**Validación (se ejecutan automáticamente):**
- Validar renderizado (FASE 1)
- Validar coherencia (FASE 2)
- Diagnosticar errores (FASE 3)

**Corrección (se ejecutan automáticamente en errores):**
- Corregir gráficos (SUBFASE 3A)
- Corregir errores de imagen TikZ

**Graficador (se ejecutan según análisis):**
- Analizar imagen matemática
- Generar código TikZ/Python/R
- Comparar similitud visual
- Refinar código iterativamente

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

**Última actualización**: 2025-12-30
**Versión**: 2.1 (Estructura modular)
**Basado en**: Documentación oficial Claude Code (nov 2025)
