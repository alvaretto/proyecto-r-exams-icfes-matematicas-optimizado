# 📚 RepositorioMatematicasICFES_R_Exams

[![Estado](https://img.shields.io/badge/Estado-Activo-brightgreen)](https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado)
[![Gemini CLI](https://img.shields.io/badge/Gemini%20CLI-0.2.0--preview.2-blue)](https://github.com/google-gemini/gemini-cli)
[![R-exams](https://img.shields.io/badge/R--exams-Compatible-orange)](https://www.r-exams.org/)
[![VSCode](https://img.shields.io/badge/VSCode-Insiders-purple)](https://code.visualstudio.com/insiders/)

**Desarrollo de ejercicios matemáticos para preparación ICFES usando R-exams con integración avanzada de IA**

Sistema completo para la creación, validación y optimización de ejercicios matemáticos alineados con las competencias ICFES, utilizando R-exams para aleatorización masiva y Gemini CLI con funcionalidades avanzadas para desarrollo asistido por IA.

---

## 🎯 **Características Principales**

- **📊 Aleatorización Masiva**: Generación de 300+ versiones únicas por ejercicio
- **🎨 Gráficos TikZ**: Visualizaciones matemáticas con fidelidad del 98%
- **🧠 IA Integrada**: Gemini CLI 0.2.0-preview.2 con comandos avanzados
- **📋 Competencias ICFES**: Alineación completa con estándares oficiales
- **🔄 Workflows Automatizados**: Desarrollo, testing y validación integrados
- **💻 IDE Optimizado**: VSCode Insiders con extensiones especializadas
## 🚀 **Estado Actual**

✅ **Sistema Completamente Operativo**

- Gemini CLI 0.2.0-preview.2 con funcionalidades avanzadas
- MCPs (Model Context Protocols) configurados y funcionales
- Integración directa con VSCode Insiders
- Documentación completa y actualizada
- Scripts de automatización optimizados

## 🛠️ **Tecnologías Utilizadas**

### **Core del Proyecto**
- **R** - Motor principal para R-exams
- **Python** - Análisis de datos y matplotlib
- **LaTeX/TikZ** - Generación de gráficos matemáticos
- **HTML/CSS** - Exportación web de ejercicios

### **IA y Automatización**
- **Gemini CLI 0.2.0-preview.2** - Desarrollo asistido por IA
- **MCPs (Model Context Protocols)** - 9 MCPs especializados instalados
- **Thinking MCP** - Análisis y razonamiento estructurado
- **Playwright MCP** - Testing automático de ejercicios web
- **LaTeX Validator MCP** - Validación de código LaTeX/TikZ
- **Image Analysis MCP** - Análisis de imágenes matemáticas
- **Augment AI** - Desarrollo rápido en VSCode
- **Brave Search MCP** - Investigación automática
- **Memory MCP** - Gestión de conocimiento persistente

### **Entorno de Desarrollo**
- **VSCode Insiders** - IDE principal con extensiones
- **Manjaro Plasma KDE** - Sistema operativo optimizado
- **Git** - Control de versiones con LFS para imágenes
- **Bash** - Scripts de automatización

## 📁 **Estructura del Proyecto**

```
RepositorioMatematicasICFES_R_Exams/
├── 📚 Auxiliares/                          # Recursos y herramientas
│   ├── 🎯 Ejemplos-Funcionales-Rmd/        # Ejercicios de referencia
│   ├── 📖 TikZ-Documentation/               # Documentación y ejemplos TikZ
│   ├── 🔧 Instalaciones/Ais/Gemini_CLI/    # Configuración Gemini CLI
│   ├── 📋 METODOLOGIA_*.md                 # Metodologías de desarrollo
│   └── 🎨 Estrategia-Avanzada-de-Replicas-de-Imagenes/
├── 🧪 Lab-Manjaro/                         # Ejercicios en desarrollo
│   └── 📅 01-S1-2024B/                     # Ejercicios por semestre
├── ⚙️ .vscode/                             # Configuración VSCode
├── 🔐 .gemini/                             # Contexto y reglas Gemini CLI
├── 📄 GEMINI.md                            # Contexto principal del proyecto
└── 📖 README.md                            # Este archivo
```

## 🚀 **Instalación y Configuración**

### **Instalación Rápida**
```bash
# Clonar el repositorio
git clone https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado.git
cd RepositorioMatematicasICFES_R_Exams

# Ejecutar configuración automática
bash Auxiliares/Instalaciones/Ais/Gemini_CLI/actualizar-gemini-cli-avanzado.sh

# Verificar instalación
bash Auxiliares/Instalaciones/Ais/Gemini_CLI/verificar-funcionalidades-avanzadas.sh
```

### **Configuración Manual**
```bash
# Instalar Gemini CLI con funcionalidades avanzadas
npm install -g @google/gemini-cli@preview

# Configurar MCPs
bash Auxiliares/Instalaciones/Ais/Gemini_CLI/install-mcps.sh

# Probar funcionalidades
bash Auxiliares/Instalaciones/Ais/Gemini_CLI/probar-comandos-avanzados.sh
```

## 💻 **Uso Básico**

### **Desarrollo de Ejercicios**
```bash
# Abrir VSCode Insiders en el proyecto
code-insiders .

# En terminal integrado, activar Gemini CLI avanzado
/id install
/init

# Desarrollar ejercicio con asistencia IA
/memory add "Ejercicio álgebra: ecuaciones cuadráticas, competencia formulación"
/chat save "desarrollo_algebra_ecuaciones"
```

### **Comandos MCPs Especializados**
```bash
# Cargar aliases de MCPs (una vez por sesión)
source Auxiliares/Instalaciones/Ais/Gemini_CLI/gemini-aliases.sh

# Análisis estructurado de problemas
gemini-thinking "analizar optimización de ejercicios R-exams ICFES"

# Validación de código LaTeX/TikZ
gemini-validate "validar código del ejercicio actual"

# Testing automático de ejercicios
gemini-test "testing del ejercicio compilado en HTML"

# Análisis de imágenes para replicación TikZ
gemini-image "analizar imagen PNG para código TikZ fidelidad 98%"
```

### **Comandos Esenciales**
```bash
# Iniciar Gemini CLI con MCPs
gemini-icfes --mcps

# Análisis de ejercicio existente
gemini --context-file "ejercicio.Rmd" --context-file ".gemini/rules-gemini.md"

# Generación de gráfico TikZ
gemini --image "imagen.png" "Genera código TikZ con fidelidad 98%"
```

## 🎯 **Funcionalidades Avanzadas**

### **Comandos Gemini CLI Avanzados**
- **`/id install`** - Activar integración con VSCode
- **`/init`** - Construir memoria semántica del proyecto
- **`/memory show/add/refresh`** - Gestión de conocimiento persistente
- **`/chat save/list/resume/delete`** - Gestión de conversaciones
- **`/code [pregunta]`** - Análisis específico de código
- **`/edit [archivo]`** - Edición directa de archivos
- **`/test [pregunta]`** - Testing y validación

### **MCPs Integrados (9 MCPs Especializados)**
- **🧠 Thinking** - Análisis y razonamiento estructurado paso a paso
- **🎭 Playwright** - Testing automático de ejercicios web y capturas
- **📐 LaTeX Validator** - Validación de código LaTeX/TikZ
- **🖼️ Image Analysis** - Análisis de imágenes matemáticas para TikZ
- **🔍 Brave Search** - Investigación automática de estándares ICFES
- **📚 Context7** - Documentación técnica especializada
- **💾 Memory** - Persistencia de mejores prácticas
- **📁 Filesystem** - Acceso directo a archivos del proyecto

### **Workflows Optimizados**
```bash
# Desarrollo completo de ejercicio
/id install → /init → /memory add "contexto" → desarrollo → /chat save "sesion"

# Optimización de ejercicio existente
/chat resume "sesion" → /memory refresh → optimización → /memory add "mejoras"

# Generación TikZ desde imagen
investigación → documentación → acceso a imagen → generación → testing → memoria
```

## 📖 **Documentación**

### **Guías Principales**
- **[Manual de Usuario](Auxiliares/Instalaciones/Ais/Gemini_CLI/manual-usuario-gemini-cli-r-exams-icfes.md)** - Guía completa para usuarios (15-20 min)
- **[Tutorial Técnico](Auxiliares/Instalaciones/Ais/Gemini_CLI/gemini-cli-r-exams.md)** - Instalación y configuración detallada
- **[Guía de MCPs](Auxiliares/Instalaciones/Ais/Gemini_CLI/MCPs_GUIA_COMPLETA.md)** - Configuración de Model Context Protocols
- **[README Gemini CLI](Auxiliares/Instalaciones/Ais/Gemini_CLI/README.md)** - Guía específica de MCPs y comandos especializados

### **Referencias Técnicas**
- **[GEMINI.md](GEMINI.md)** - Contexto completo del proyecto
- **[Reglas Gemini](.gemini/rules-gemini.md)** - Configuración específica para R-exams
- **[Comandos de Ejemplo](Auxiliares/Instalaciones/Ais/Gemini_CLI/comandos-ejemplo-gemini-cli.sh)** - Scripts listos para usar

### **Metodologías**
- **[Desarrollo de Ejercicios](Auxiliares/METODOLOGIA_DESARROLLO_EJERCICIOS.md)** - Proceso completo
- **[Validación ICFES](Auxiliares/METODOLOGIA_VALIDACION_ICFES.md)** - Estándares y competencias
- **[Optimización TikZ](Auxiliares/METODOLOGIA_TIKZ_FIDELIDAD.md)** - Gráficos de alta calidad

## 🤝 **Contribución y Mantenimiento**

### **Para Colaboradores**
1. **Configurar entorno**: Seguir guías de instalación
2. **Familiarizarse**: Leer manual de usuario y tutorial técnico
3. **Usar herramientas**: Aprovechar Gemini CLI con funcionalidades avanzadas
4. **Documentar**: Mantener actualizada la documentación de cambios

### **Estándares de Calidad**
- **Competencias ICFES**: Todos los ejercicios deben alinearse con estándares oficiales
- **Aleatorización**: Mínimo 300 versiones únicas por ejercicio
- **Gráficos TikZ**: Fidelidad visual del 98% respecto a imagen original
- **Testing**: Validación automática de compilación HTML/PDF
- **Documentación**: Cambios deben reflejarse en archivos correspondientes

### **Herramientas de Desarrollo**
```bash
# Verificar estado del proyecto
bash Auxiliares/Instalaciones/Ais/Gemini_CLI/verificar-funcionalidades-avanzadas.sh

# Probar nuevas funcionalidades
bash Auxiliares/Instalaciones/Ais/Gemini_CLI/probar-comandos-avanzados.sh

# Testing de MCPs
bash Auxiliares/Instalaciones/Ais/Gemini_CLI/test-mcps.sh
```

---

## 📊 **Información del Proyecto**

- **Autor**: Álvaro Ángel Molina
- **Institución**: IE Pedacito de Cielo
- **Propósito**: Preparación ICFES Matemáticas
- **Licencia**: Proyecto Educativo
- **Última Actualización**: Agosto 2025

**🎯 Objetivo**: Democratizar el acceso a ejercicios matemáticos de alta calidad para preparación ICFES mediante herramientas automatizadas y asistencia de IA avanzada.

---

*Para soporte técnico o consultas, revisar la documentación en `Auxiliares/Instalaciones/Ais/Gemini_CLI/` o utilizar los comandos de ayuda integrados en Gemini CLI.*
