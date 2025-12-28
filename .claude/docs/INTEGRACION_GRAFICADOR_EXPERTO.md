# Integracion del Graficador-Experto en Workflow ICFES

## Resumen

Documentacion de la integracion del modulo Graficador-Experto en el workflow principal de generacion de ejercicios ICFES R-Exams.

**Fecha de integracion:** 2025-12-27
**Version:** 1.0

---

## Objetivo

Integrar automaticamente el sistema Graficador-Experto cuando se detecten imagenes matematicas, generando codigo triple (TikZ, Python, R) optimizado para R-exams.

---

## Componentes Creados

### 1. Skills Nuevos

#### `/detectar-imagen-matematica`
- **Ubicacion:** `.claude/skills/detectar-imagen-matematica/skill.md`
- **Funcion:** Detecta automaticamente imagenes con contenido matematico ICFES
- **Activacion:** Automatica al compartir imagen

#### `/generar-codigo-triple`
- **Ubicacion:** `.claude/skills/generar-codigo-triple/skill.md`
- **Funcion:** Genera codigo en 3 lenguajes (TikZ, Python, R)
- **Proceso:**
  1. Analizar imagen con Claude Vision
  2. Generar TikZ/LaTeX
  3. Generar Python (matplotlib)
  4. Generar R (ggplot2)
  5. Validar fidelidad visual (objetivo: ≥98%)
  6. Presentar opciones al usuario

### 2. Hook Nuevo

#### `post-imagen-matematica-detectada`
- **Ubicacion:** `.claude/hooks/post-imagen-matematica-detectada.md`
- **Evento:** Se activa al detectar imagen matematica
- **Accion:** Inicia workflow del Graficador-Experto

### 3. Script de Rollback

- **Ubicacion:** `.claude/scripts/rollback_integracion_graficador.sh`
- **Funcion:** Revierte todos los cambios de la integracion
- **Uso:** `./rollback_integracion_graficador.sh`

---

## Flujo de Trabajo Modificado

```
Usuario comparte imagen ICFES
          ↓
    ¿Contiene elementos matematicos?
          ↓
    SI → [Hook] post-imagen-matematica-detectada
          ↓
    [Skill] /generar-codigo-triple
          ↓
    Graficador-Experto genera:
    ├── TikZ/LaTeX
    ├── Python (matplotlib/numpy)
    └── R (ggplot2)
          ↓
    Validar fidelidad visual con Claude Vision
          ↓
    Presentar opciones al usuario
          ↓
    Usuario selecciona codigo
          ↓
    Integrar en .Rmd
          ↓
    Continuar workflow ICFES normal
```

---

## Archivos Modificados

### settings.local.json
Agregados permisos para nuevos skills:
```json
"Skill(detectar-imagen-matematica)",
"Skill(generar-codigo-triple)"
```

### Mermaid_Chart.txt
Agregado subgrafo `GRAFICADOR_EXPERTO` con:
- Nodos de generacion triple
- Nodos de validacion
- Nodos de seleccion de usuario
- Estilos visuales

---

## Ubicacion de Outputs

Los codigos generados se guardan en:

```
Graficador-Experto/outputs/
├── output_tikz.tex       # Codigo TikZ/LaTeX
├── output_python.py      # Codigo Python matplotlib
├── output_r.R            # Codigo R ggplot2
├── reporte_matematico.md # Reporte consolidado
└── renders/
    ├── render_tikz.png   # Render TikZ
    ├── render_python.png # Render Python
    └── render_r.png      # Render R
```

---

## Integracion en .Rmd

### Opcion TikZ
```{r grafico-tikz, echo=FALSE, results='asis'}
include_tikz(tikz_code, knitr::opts_knit$get("rmarkdown.pandoc.to"))
```

### Opcion Python (via Reticulate)
```{r setup-python, include=FALSE}
library(reticulate)
```

```{python grafico, echo=FALSE}
# Codigo Python
```

### Opcion R (nativo)
```{r grafico, echo=FALSE}
# Codigo ggplot2
```

---

## Backup y Rollback

### Backup Creado
```
.claude/backups/pre_integracion_graficador_20251227_143546/
├── agents/
├── commands/
├── docs/
├── hooks/
├── skills/
├── Mermaid_Chart.txt
├── settings.json
└── settings.local.json
```

### Rollback
Si algo falla, ejecutar:
```bash
cd /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams
.claude/scripts/rollback_integracion_graficador.sh
```

---

## Verificacion Post-Integracion

### Checklist
- [ ] Skills existentes funcionan (`/analizar-icfes`, `/generar-schoice`)
- [ ] Nuevo skill `/generar-codigo-triple` responde
- [ ] Hook detecta imagenes matematicas
- [ ] Graficador-Experto genera codigo correctamente
- [ ] Codigos compilan/ejecutan sin errores
- [ ] Integracion en .Rmd funciona
- [ ] Ciclo de Validacion no se rompe

### Comandos de Prueba
```bash
# Verificar estructura
ls -la .claude/skills/detectar-imagen-matematica/
ls -la .claude/skills/generar-codigo-triple/
ls -la .claude/hooks/post-imagen-matematica-detectada.md

# Verificar permisos en settings.local.json
grep "detectar-imagen" .claude/settings.local.json
grep "generar-codigo-triple" .claude/settings.local.json
```

---

## Dependencias

### Software Requerido
- LaTeX con TikZ y pgfplots
- Python 3.8+ con matplotlib, numpy
- R 4.0+ con ggplot2, dplyr
- ImageMagick (para conversion PDF→PNG)

### Verificar Dependencias
```bash
# LaTeX
pdflatex --version

# Python
python3 -c "import matplotlib; import numpy; print('OK')"

# R
Rscript -e "library(ggplot2); library(dplyr); print('OK')"

# ImageMagick
convert --version
```

---

## Notas Importantes

1. **Fidelidad Visual:** El objetivo es ≥98%. Si no se alcanza, el sistema itera hasta 5 veces.

2. **Seleccion de Usuario:** SIEMPRE se pregunta al usuario cual codigo implementar. No se hace automaticamente.

3. **Repositorio TikZ:** Si se selecciona TikZ y es validado, se guarda automaticamente en `Repositorio-Graficas-TikZ/`.

4. **Compatibilidad:** Los codigos estan optimizados para integracion en archivos .Rmd de R-exams.

---

**Documentacion creada:** 2025-12-27
**Autor:** Sistema Claude Code
**Estado:** Integracion completada
