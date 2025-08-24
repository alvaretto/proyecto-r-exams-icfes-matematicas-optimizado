# 🤖 Gemini CLI con MCPs para Desarrollo R-exams ICFES

[![Gemini CLI](https://img.shields.io/badge/Gemini%20CLI-0.2.0--preview.2-blue)](https://github.com/google-gemini/gemini-cli)
[![MCPs](https://img.shields.io/badge/MCPs-9%20Instalados-green)](.)
[![Estado](https://img.shields.io/badge/Estado-Completamente%20Funcional-brightgreen)](.)
[![R-exams](https://img.shields.io/badge/R--exams-Compatible-orange)](https://www.r-exams.org/)

**Configuración completa de Gemini CLI 0.2.0-preview.2 con 9 MCPs especializados para desarrollo optimizado de ejercicios matemáticos ICFES usando R-exams.**

Sistema integrado que combina IA avanzada, análisis estructurado, validación automática y testing para crear ejercicios de máxima calidad con workflows automatizados.

---

## 🎯 **Estado Actual**

✅ **Sistema Completamente Operativo**

- **Gemini CLI 0.2.0-preview.2** con funcionalidades avanzadas
- **9 MCPs instalados y configurados** (4 nuevos + 5 existentes)
- **Integración completa con VSCode Insiders**
- **Scripts automatizados** para todos los workflows
- **Documentación actualizada** y funcional

## 📦 **MCPs Disponibles**

### **🆕 MCPs Adicionales Especializados**

#### **🧠 Thinking MCP**
- **Función**: Análisis y razonamiento estructurado paso a paso
- **Comando**: `gemini-thinking`
- **Uso**: Análisis completo de problemas de optimización de ejercicios
- **Herramientas**: `structured_thinking` - Análisis sistemático con contexto

#### **🎭 Playwright MCP (Corregido)**
- **Función**: Testing automático de ejercicios web y capturas de pantalla
- **Comando**: `gemini-test`
- **Uso**: Testing de ejercicios R-exams compilados en HTML
- **Herramientas**: `test_web_exercise`, `screenshot_exercise`

#### **📐 LaTeX Validator MCP**
- **Función**: Validación de código LaTeX/TikZ para ejercicios R-exams
- **Comando**: `gemini-validate`
- **Uso**: Verificación de sintaxis LaTeX/TikZ, detección de errores
- **Herramientas**: `validate_latex`, `compile_tikz`

#### **🖼️ Image Analysis MCP**
- **Función**: Análisis de imágenes matemáticas para replicación TikZ
- **Comando**: `gemini-image`
- **Uso**: Análisis de imágenes PNG para generar código TikZ (fidelidad 98%)
- **Herramientas**: `analyze_math_image`, `extract_image_metadata`

### **📚 MCPs Base Mantenidos**

- **🔍 Brave Search MCP** - Búsqueda web privada y actualizada
- **📚 Context7 MCP** - Documentación de librerías y APIs
- **💾 Memory MCP** - Gestión de memoria persistente
- **📁 Filesystem MCP** - Acceso a archivos locales del proyecto

---

## 🚀 **Instalación Rápida**

### **Instalación Completa desde Cero**
```bash
# 1. Instalar todos los MCPs
bash install-mcps.sh

# 2. Configurar integración con Gemini CLI
bash configure-gemini-mcps.sh

# 3. Verificar instalación
bash test-mcps.sh

# 4. Cargar aliases para uso rápido
source gemini-aliases.sh
```

### **Verificación de Estado**
```bash
# Verificar MCPs instalados
bash test-mcps.sh

# Listar MCPs configurados en Gemini CLI
gemini mcp list

# Probar funcionalidad básica
gemini-icfes
```

---

## 💻 **Comandos de Uso**

### **🎯 Comandos Principales**
```bash
# Modo interactivo completo con todos los MCPs
gemini-icfes

# Análisis estructurado de problemas
gemini-thinking "analizar optimización de ejercicios R-exams ICFES"

# Validación de código LaTeX/TikZ
gemini-validate "validar código del ejercicio actual"

# Testing automático de ejercicios
gemini-test "testing del ejercicio compilado en HTML"

# Análisis de imágenes para replicación TikZ
gemini-image "analizar imagen PNG para código TikZ"
```

### **🔧 Scripts Directos**
```bash
# Script principal con MCPs
bash gemini-icfes-mcps.sh

# Cargar aliases (una vez por sesión)
source gemini-aliases.sh

# Testing completo del sistema
bash test-mcps.sh

# Configurar variables de entorno
source mcp-env-setup.sh
```

---

## 📁 **Archivos del Directorio**

### **📄 Scripts Principales**
- **`install-mcps.sh`** - Instalación automatizada de todos los MCPs
- **`configure-gemini-mcps.sh`** - Configuración de integración con Gemini CLI
- **`test-mcps.sh`** - Testing y verificación de funcionalidad
- **`gemini-icfes-mcps.sh`** - Script optimizado para usar MCPs
- **`gemini-aliases.sh`** - Aliases para comandos rápidos

### **⚙️ Configuración**
- **`.mcp-config.json`** - Configuración base de MCPs (en raíz del proyecto)
- **`.gemini-mcp-config.json`** - Configuración específica para Gemini CLI
- **`vscode-mcp-tasks.json`** - Tareas integradas para VSCode Insiders
- **`mcp-env-setup.sh`** - Configuración de variables de entorno

### **📚 Documentación**
- **`README.md`** - Este archivo (guía principal)
- **`MCPs_GUIA_COMPLETA.md`** - Guía detallada de uso de MCPs
- **`manual-usuario-gemini-cli-r-exams-icfes.md`** - Manual completo de usuario
- **`gemini-cli-r-exams.md`** - Tutorial técnico de instalación
- **`MCPs_INSTALACION_COMPLETADA_ADICIONALES.md`** - Registro de instalación

### **🗂️ Archivos de Estado**
- **`LIMPIEZA_ARCHIVOS_OBSOLETOS_20250824.md`** - Registro de limpieza
- **`backup-20250824-173253/`** - Backup de configuración anterior

---

## 🔄 **Workflows Integrados**

### **Workflow 1: Desarrollo Completo de Ejercicio**
```bash
# 1. Análisis estructurado del problema
gemini-thinking "analizar ejercicio de geometría ICFES nivel 3"

# 2. Desarrollo con acceso a archivos y documentación
gemini-icfes
# En sesión interactiva: "leer ejemplos similares y desarrollar ejercicio"

# 3. Validación de código generado
gemini-validate "validar código LaTeX y TikZ del ejercicio"

# 4. Testing del resultado
gemini-test "compilar y testing visual del ejercicio"
```

### **Workflow 2: Replicación TikZ desde Imagen**
```bash
# 1. Análisis de imagen matemática
gemini-image "analizar imagen PNG para identificar elementos"

# 2. Estrategia de replicación
gemini-thinking "estrategia para replicar imagen con TikZ fidelidad 98%"

# 3. Validación del código TikZ
gemini-validate "validar sintaxis del código TikZ generado"

# 4. Comparación visual
gemini-test "captura de pantalla para comparar con original"
```

### **Workflow 3: Optimización de Ejercicio Existente**
```bash
# 1. Análisis del ejercicio actual
gemini-thinking "analizar problemas en ejercicio existente"

# 2. Validación de código actual
gemini-validate "identificar errores en código LaTeX"

# 3. Testing del estado actual
gemini-test "verificar compilación y funcionamiento"

# 4. Desarrollo de mejoras
gemini-icfes
# En sesión: "aplicar mejoras identificadas"
```

---

## 💻 **Integración VSCode Insiders**

### **🔧 Tareas Disponibles**
Presiona `Ctrl+Shift+P` → `Tasks: Run Task` → Seleccionar:

- **Gemini: Análisis Estructurado** - Ejecuta thinking MCP
- **Gemini: Validar LaTeX/TikZ** - Ejecuta validación de código
- **Gemini: Testing Automático** - Ejecuta testing de ejercicios
- **Gemini: Analizar Imagen** - Ejecuta análisis de imágenes

### **⚙️ Configuración Automática**
```bash
# Las tareas se configuran automáticamente al ejecutar:
bash configure-gemini-mcps.sh
```

### **🔗 Extensiones Recomendadas**
- **Gemini CLI Companion** - Integración directa con Gemini CLI
- **LaTeX Workshop** - Soporte para archivos LaTeX/TikZ
- **R Extension** - Soporte para archivos R y R-exams

---

## 🔧 **Troubleshooting**

### **❌ Problemas Comunes**

#### **MCPs no se conectan**
```bash
# Verificar configuración
gemini mcp list

# Reconfigurar si es necesario
bash configure-gemini-mcps.sh

# Verificar permisos
chmod +x *.sh
```

#### **Comandos alias no funcionan**
```bash
# Cargar aliases nuevamente
source gemini-aliases.sh

# Verificar que el archivo existe
ls -la gemini-aliases.sh

# Ejecutar directamente si es necesario
bash gemini-icfes-mcps.sh
```

#### **Errores de Node.js**
```bash
# Verificar versión de Node.js
node --version  # Debe ser >= 18

# Reinstalar dependencias si es necesario
cd ../../.mcps/thinking-mcp && npm install
```

#### **Variables de entorno**
```bash
# Configurar variables básicas
export GEMINI_API_KEY="tu_api_key"

# Cargar configuración completa
source mcp-env-setup.sh

# Verificar variables
echo $GEMINI_API_KEY
```

### **🔍 Diagnóstico Completo**
```bash
# Ejecutar testing completo
bash test-mcps.sh

# Verificar logs de Gemini CLI
gemini --debug

# Verificar estructura de archivos
ls -la ../../../.mcps/
```

---

## 📚 **Referencias**

### **📖 Documentación Técnica**
- **[Manual de Usuario](manual-usuario-gemini-cli-r-exams-icfes.md)** - Guía completa (15-20 min)
- **[Tutorial Técnico](gemini-cli-r-exams.md)** - Instalación y configuración detallada
- **[Guía de MCPs](MCPs_GUIA_COMPLETA.md)** - Uso avanzado de MCPs

### **🔗 Enlaces Externos**
- **[Gemini CLI GitHub](https://github.com/google-gemini/gemini-cli)** - Repositorio oficial
- **[Model Context Protocol](https://modelcontextprotocol.io/)** - Especificación MCP
- **[R-exams](https://www.r-exams.org/)** - Documentación R-exams

### **🎯 Contexto del Proyecto**
- **[GEMINI.md](../../../../GEMINI.md)** - Contexto completo del proyecto
- **[README.md](../../../../README.md)** - Información general del proyecto

---

## 📊 **Información del Directorio**

- **Ubicación**: `Auxiliares/Instalaciones/Ais/Gemini_CLI/`
- **Propósito**: Configuración de Gemini CLI con MCPs para desarrollo R-exams ICFES
- **Estado**: ✅ Completamente funcional y actualizado
- **Última actualización**: Agosto 2025
- **Versión Gemini CLI**: 0.2.0-preview.2
- **MCPs instalados**: 9 (4 nuevos + 5 existentes)

**🎯 Objetivo**: Proporcionar herramientas de IA avanzada para desarrollo eficiente de ejercicios matemáticos ICFES con máxima calidad y workflows automatizados.

---

*Para soporte técnico, revisar la sección Troubleshooting o consultar la documentación técnica en los archivos de referencia.*
