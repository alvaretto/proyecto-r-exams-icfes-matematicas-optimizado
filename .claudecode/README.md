# 🤖 Infraestructura de Automatización Claude Code - R-Exams ICFES

Sistema completo de automatización para validación y corrección de archivos .Rmd del proyecto R-Exams ICFES.

---

## 📁 Estructura del Sistema

```
.claudecode/
├── config.yml                          # Configuración base del sistema
├── README.md                           # Este archivo
├── GUIA_USO_CLAUDE_CODE.md            # Guía de uso dentro de Claude Code
├── PROMPTS_LISTOS_USAR.md             # Prompts pre-configurados listos para usar
├── examples/
│   ├── basico/                         # Nivel Básico: Hooks y Validación Estática
│   │   ├── 01-pre-commit-yaml-validator.sh
│   │   ├── 02-latex-escape-validator.sh
│   │   └── 03-metadata-icfes-validator.sh
│   ├── intermedio/                     # Nivel Intermedio: Agentes Personalizados
│   │   ├── 04-agente-validador-estilo.md
│   │   ├── 05-agente-corrector-automatico.md
│   │   └── 06-agente-comparador-estructura.md
│   └── avanzado/                       # Nivel Avanzado: Skills y Workflows
│       ├── 07-skill-render-validator.sh
│       ├── 08-skill-error-analyzer.md
│       └── 09-workflow-validation-chain.md
└── workflows/
    └── validation_chain.sh             # Workflow completo de validación

.claudedoc/
└── guia_estilo_icfes.md                # Fuente de Verdad - Reglas y Estándares
```

---

## 🎯 Fase de Preparación

### Fuente de Verdad

La guía de estilo está ubicada en `.claudedoc/guia_estilo_icfes.md` y actúa como **fuente única de verdad** para todas las validaciones automáticas.

**Contenido:**
- Estructura obligatoria de archivos .Rmd
- Metadatos ICFES requeridos
- Patrones de error comunes
- Criterios de calidad
- Checklist de validación

**Uso:**
Todos los agentes y validadores consultan este archivo para validar conformidad.

---

## 📊 Niveles de Complejidad

### 🟢 Nivel Básico: Hooks y Validación Estática

Scripts bash ejecutables que validan aspectos específicos de archivos .Rmd.

#### 01 - Validador YAML Pre-commit
```bash
# Validar sintaxis YAML y campos obligatorios
.claudecode/examples/basico/01-pre-commit-yaml-validator.sh archivo.Rmd
```

**Validaciones:**
- Bloque YAML presente
- Campos obligatorios (output, header-includes)
- `latex_engine: xelatex` configurado
- Paquetes LaTeX críticos presentes

#### 02 - Validador de Caracteres Especiales LaTeX
```bash
# Detectar caracteres sin escape que rompen renderizado
.claudecode/examples/basico/02-latex-escape-validator.sh archivo.Rmd
```

**Validaciones:**
- Caracteres `&`, `%`, `$`, `#`, `_`, `{`, `}` sin escape
- Sugerencias de corrección por línea

#### 03 - Validador de Metadatos ICFES
```bash
# Validar metadatos ICFES completos y correctos
.claudecode/examples/basico/03-metadata-icfes-validator.sh archivo.Rmd
```

**Validaciones:**
- Todos los campos ICFES presentes
- Valores válidos según enumeraciones
- Coherencia entre campos relacionados

---

### 🟡 Nivel Intermedio: Agentes Personalizados

Agentes especializados con instrucciones de sistema para análisis y corrección.

#### 04 - Agente Validador de Estilo
Compara archivos .Rmd contra `guia_estilo_icfes.md` y sugiere correcciones.

**Características:**
- Análisis completo de estructura
- Comparación contra estándares
- Sugerencias específicas con código antes/después
- Niveles de severidad (ERROR, ADVERTENCIA, SUGERENCIA)

**Uso:**
```bash
claude-code agent validate validator_icfes archivo.Rmd
```

#### 05 - Agente Corrector Automático
Corrige automáticamente errores comunes sin intervención manual.

**Correcciones Automáticas:**
- Agregar campos YAML faltantes
- Corregir `set.seed()` fijo a aleatorio
- Escapar caracteres especiales LaTeX
- Agregar opciones faltantes (scipen, OutDec)

**Uso:**
```bash
claude-code agent fix auto_fixer_icfes archivo.Rmd --auto-safe
```

#### 06 - Agente Comparador de Estructura
Compara estructura de archivos contra ejemplos funcionales validados.

**Características:**
- Análisis comparativo de estructura
- Identificación de desviaciones de patrones
- Referencias a ejemplos funcionales específicos
- Recomendaciones de alineación

**Uso:**
```bash
claude-code agent compare structure_comparator archivo.Rmd
```

---

### 🔴 Nivel Avanzado: Skills y Validación en Tiempo Real

Skills especializados y workflows completos con chaining.

#### 07 - Skill: Validador de Renderizado
Ejecuta renderizado real de archivos .Rmd y captura errores.

**Funcionalidad:**
- Renderiza con `exams2html()` (rápido)
- Captura logs de error estructurados
- Genera reportes de éxito/fallo
- Compatible con análisis posterior

**Uso:**
```bash
.claudecode/examples/avanzado/07-skill-render-validator.sh archivo.Rmd /tmp/output
```

#### 08 - Skill: Analizador de Errores
Analiza logs de error y extrae información estructurada para corrección.

**Funcionalidad:**
- Identifica tipo de error (YAML, LaTeX, TikZ, R, etc.)
- Extrae ubicación (línea, chunk)
- Genera sugerencias de corrección
- Clasifica por severidad

**Uso:**
```python
from claudecode.skills import ErrorAnalyzer

analyzer = ErrorAnalyzer()
analysis = analyzer.analyze_error_log("error_log.txt", "archivo.Rmd")
```

#### 09 - Workflow: Cadena de Validación Completa
Workflow que combina todos los componentes para validación iterativa hasta éxito.

**Flujo:**
1. Validación inicial (YAML, metadatos)
2. Corrección automática
3. Renderizado de prueba
4. Análisis de errores
5. Corrección dirigida
6. Iteración hasta éxito (máx. 5 iteraciones)

**Uso:**
```bash
.claudecode/workflows/validation_chain.sh archivo.Rmd
```

---

## 📖 Documentación Adicional

- **GUIA_USO_CLAUDE_CODE.md**: Guía completa de cómo usar estos componentes dentro del entorno dinámico de Claude Code (Cursor)
- **PROMPTS_LISTOS_USAR.md**: Colección de prompts pre-configurados para copiar y pegar directamente

## 🚀 Guía de Inicio Rápido

### 1. Validación Básica (Recomendado para empezar)

```bash
# Validar un archivo .Rmd antes de commit
cd /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams

# Validar YAML
.claudecode/examples/basico/01-pre-commit-yaml-validator.sh ejercicio.Rmd

# Validar caracteres especiales
.claudecode/examples/basico/02-latex-escape-validator.sh ejercicio.Rmd

# Validar metadatos ICFES
.claudecode/examples/basico/03-metadata-icfes-validator.sh ejercicio.Rmd
```

### 2. Validación Avanzada (Renderizado Real)

```bash
# Ejecutar workflow completo de validación
.claudecode/workflows/validation_chain.sh ejercicio.Rmd
```

### 3. Integración con Git Pre-commit

```bash
# Crear pre-commit hook
cat > .git/hooks/pre-commit << 'EOF'
#!/bin/bash
for file in $(git diff --cached --name-only --diff-filter=ACM | grep '\.Rmd$'); do
    .claudecode/examples/basico/01-pre-commit-yaml-validator.sh "$file" || exit 1
    .claudecode/examples/basico/03-metadata-icfes-validator.sh "$file" || exit 1
done
EOF

chmod +x .git/hooks/pre-commit
```

---

## 🔧 Configuración del Entorno

### Requisitos

- **Sistema Operativo**: Manjaro/Arch Linux
- **R**: Versión 4.0+ con paquetes `exams` y `rmarkdown`
- **Python**: 3.8+ (para skills avanzados)
- **LaTeX**: XeLaTeX con paquetes TikZ

### Instalación de Dependencias

```bash
# Instalar R y dependencias
sudo pacman -S r

# Instalar paquetes R necesarios
Rscript -e "install.packages(c('exams', 'rmarkdown', 'knitr', 'reticulate'))"

# Instalar LaTeX y TikZ
sudo pacman -S texlive-most texlive-lang

# Instalar Python (si no está instalado)
sudo pacman -S python
```

---

## 📚 Documentación de Referencia

- **Fuente de Verdad**: `.claudedoc/guia_estilo_icfes.md`
- **Reglas Generales**: `Auxiliares/rules_full/reglas-generales.md`
- **Ejemplos Funcionales**: `A-Produccion/Ejemplos-Funcionales-Rmd/`
- **Documentación R-exams**: https://www.r-exams.org/

---

## 🔄 Integración con Flujo de Trabajo

### Desarrollo Local

1. Editar archivo .Rmd en RStudio
2. Ejecutar validadores básicos antes de commit
3. Si hay errores, usar agente corrector automático
4. Validar renderizado completo antes de push

### CI/CD Pipeline

1. Pre-commit hooks ejecutan validadores básicos
2. Push activa workflow completo de validación
3. Si falla, análisis automático de errores
4. Reportes de error en PR/MR

---

## ⚠️ Notas Importantes

1. **Backups Automáticos**: Los scripts de corrección automática crean backups (`.backup`)
2. **Iteraciones Limitadas**: El workflow limita a 5 iteraciones para evitar bucles infinitos
3. **Renderizado Rápido**: El validador usa `exams2html()` (más rápido que PDF) para validación
4. **Compatibilidad**: Todos los cambios mantienen compatibilidad con RStudio

---

## 🐛 Solución de Problemas

### Script no ejecutable
```bash
chmod +x .claudecode/examples/basico/*.sh
chmod +x .claudecode/examples/avanzado/*.sh
```

### R no encontrado
```bash
# Verificar instalación
which Rscript

# Agregar a PATH si es necesario
export PATH="/usr/bin:$PATH"
```

### Paquetes R faltantes
```bash
Rscript -e "install.packages(c('exams', 'rmarkdown', 'knitr'))"
```

---

## 📝 Contribuciones y Extensión

Para agregar nuevos validadores:

1. Crear script en nivel apropiado (basico/intermedio/avanzado)
2. Seguir formato de los ejemplos existentes
3. Documentar en este README
4. Integrar con workflows si es necesario

---

**Última actualización**: 2025-01-XX  
**Versión**: 1.0.0  
**Mantenido por**: Sistema ICFES R-Exams
