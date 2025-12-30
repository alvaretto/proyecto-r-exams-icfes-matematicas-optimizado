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

## 🛠️ Skills Disponibles

### Workflow Principal
- `/analizar-icfes` - Analizar ejercicio según 6 dimensiones ICFES
- `/generar-schoice` - Generar ejercicio de selección única
- `/generar-cloze` - Generar ejercicio compuesto

### Validación
- `/validar-renderizado` - Ejecutar FASE 1 del ciclo
- `/validar-coherencia` - Ejecutar FASE 2 del ciclo
- `/diagnosticar-errores` - Ejecutar FASE 3 del ciclo

### Corrección
- `/corregir-graficos` - Corregir errores gráficos (SUBFASE 3A)
- `/corregir-error-imagen` - Corregir errores TikZ/include_tikz()

### Producción
- `/promover-ejercicio` - Mover ejercicio validado a producción

### Graficador Experto
- `/analizar-imagen-grafica` - Analizar imagen matemática para replicación
- `/generar-codigo-tikz` - Generar código TikZ
- `/generar-codigo-python` - Generar código Python/matplotlib
- `/generar-codigo-r` - Generar código R/ggplot2
- `/comparar-similitud-visual` - Comparar imagen generada vs original
- `/refinar-codigo-grafico` - Refinar código según feedback visual

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
