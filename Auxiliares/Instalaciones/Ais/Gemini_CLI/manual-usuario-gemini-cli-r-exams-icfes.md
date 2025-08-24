# Manual de Usuario: Gemini CLI para R-exams ICFES

**Versión:** 1.0 | **Fecha:** Agosto 2025 | **Proyecto:** RepositorioMatematicasICFES_R_Exams

---

## 📚 **ÍNDICE DE CONTENIDOS**

1. [**Introducción Rápida**](#-1-introducción-rápida) - Qué es y por qué usarlo
2. [**Requisitos Previos**](#-2-requisitos-previos) - Verificar configuración
3. [**Comandos Básicos Esenciales**](#-3-comandos-básicos-esenciales) - Los 5 comandos más importantes
4. [**Casos de Uso Frecuentes**](#-4-casos-de-uso-frecuentes) - Ejemplos paso a paso
5. [**Solución de Problemas**](#-5-solución-de-problemas-comunes) - Errores típicos y soluciones
6. [**Referencias Rápidas**](#-6-referencias-rápidas) - Tabla de comandos y atajos
7. [**Ejemplos Prácticos**](#-7-ejemplos-prácticos-paso-a-paso) - Casos completos
8. [**Consejos Avanzados**](#-8-consejos-avanzados) - Optimización y productividad
9. [**Checklist de Calidad**](#-9-checklist-de-calidad) - Verificación antes de usar
10. [**Soporte y Recursos**](#-10-soporte-y-recursos) - Ayuda adicional

**⏱️ Tiempo estimado de lectura:** 15-20 minutos | **🎯 Objetivo:** Uso productivo inmediato

---

## ⚡ **INICIO RÁPIDO (2 MINUTOS)**

**¿Tienes prisa? Estos 3 comandos te permiten empezar inmediatamente:**

```bash
# 1. Verificar que todo funciona
.gemini/scripts/verify_setup.sh

# 2. Iniciar Gemini CLI
gemini-icfes --basic

# 3. Analizar un ejercicio (ejemplo)
gemini --context-file ".gemini/rules-gemini.md" "Explica las competencias ICFES matemáticas"
```

**Si los comandos funcionan, ¡ya puedes usar Gemini CLI!** Continúa leyendo para casos de uso específicos.

---

## 🎯 **1. INTRODUCCIÓN RÁPIDA**

### **¿Qué es Gemini CLI?**
Gemini CLI es una herramienta de inteligencia artificial que te permite analizar, generar y optimizar ejercicios matemáticos para el ICFES usando el poder de Google Gemini 2.5 Pro directamente desde tu terminal.

### **¿Por qué usarlo para R-exams ICFES?**
- 🧠 **Contexto masivo**: 1M tokens (5x mayor que otras herramientas IA)
- 🎨 **Generación TikZ**: Código TikZ con fidelidad visual del 98%
- 📊 **Validación ICFES**: Verifica automáticamente estándares y competencias
- ⚡ **Análisis profundo**: Evalúa ejercicios completos con metodologías del proyecto
- 🔍 **Análisis de imágenes**: Convierte gráficos matemáticos en código TikZ

### **Tiempo de aprendizaje:** 15-20 minutos para uso productivo

---

## ✅ **2. REQUISITOS PREVIOS**

### **Verificar Configuración Completa**

Antes de empezar, ejecuta el script de verificación:

```bash
# Desde el directorio raíz del proyecto
.gemini/scripts/verify_setup.sh
```

**Debes ver estos resultados:**
- ✅ Gemini CLI instalado
- ✅ API Key configurada
- ✅ Archivos de contexto creados
- ✅ Configuración Pro activa (1M tokens)

### **Si hay problemas:**
- Consulta el tutorial completo: `Auxiliares/Instalaciones/Ais/Gemini_CLI/gemini-cli-r-exams.md`
- Revisa la configuración: `~/.config/gemini/icfes-config.json`

---

## 🚀 **3. COMANDOS BÁSICOS ESENCIALES**

### **Comando 1: Iniciar Gemini CLI**
```bash
# Modo básico (recomendado para empezar)
gemini-icfes --basic

# Modo optimizado (con verificaciones avanzadas)
gemini-icfes --optimized
```

### **Comando 2: Análisis con Contexto del Proyecto**
```bash
# Cargar contexto completo del proyecto
gemini --context-file "GEMINI.md" "Tu pregunta aquí"
```

### **Comando 3: Análisis de Ejercicio Específico**
```bash
# Analizar un ejercicio R-exams
gemini --context-file "ruta/ejercicio.Rmd" \
       --context-file ".gemini/rules-gemini.md" \
       "Analiza este ejercicio según estándares ICFES"
```

### **Comando 4: Análisis de Imagen para TikZ**
```bash
# Generar código TikZ desde imagen
gemini --image "ruta/imagen.png" \
       --context-file ".gemini/rules-gemini.md" \
       "Genera código TikZ con fidelidad 98%"
```

### **Comando 5: Ayuda y Comandos Disponibles**
```bash
# Ver ayuda del script unificado
gemini-icfes --help

# Ver ayuda de Gemini CLI
gemini --help
```

---

## 📚 **4. CASOS DE USO FRECUENTES**

### **CASO 1: Analizar un Ejercicio R-exams Existente**

**Objetivo:** Evaluar la calidad y alineación ICFES de un ejercicio.

**Pasos:**
1. Navega al directorio del ejercicio:
   ```bash
   cd Auxiliares/Ejemplos-Funcionales-Rmd/
   ```

2. Ejecuta el análisis:
   ```bash
   gemini --context-file "ejercicio.Rmd" \
          --context-file "../.gemini/rules-gemini.md" \
          "Analiza este ejercicio R-exams: estructura técnica, competencia ICFES, nivel de dificultad y calidad de distractores"
   ```

**Resultado esperado:**
- Evaluación de sintaxis R-exams
- Verificación de competencia ICFES
- Análisis de nivel de dificultad
- Sugerencias de mejora

### **CASO 2: Generar Código TikZ desde Imagen**

**Objetivo:** Convertir una imagen matemática en código TikZ reutilizable.

**Pasos:**
1. Prepara tu imagen (PNG, JPG, SVG):
   ```bash
   # Verifica que la imagen esté en el proyecto
   ls ruta/a/tu/imagen.png
   ```

2. Genera el código TikZ:
   ```bash
   gemini --image "ruta/a/tu/imagen.png" \
          --context-file ".gemini/rules-gemini.md" \
          "Analiza esta imagen matemática y genera código TikZ equivalente con fidelidad 98%. Usa elementos en negrita cursiva y escala apropiada."
   ```

**Resultado esperado:**
- Código TikZ completo y compilable
- Fidelidad visual del 98%
- Elementos de texto en negrita cursiva
- Comentarios explicativos

### **CASO 3: Validar Estándares ICFES**

**Objetivo:** Verificar que un ejercicio cumple con los estándares ICFES.

**Pasos:**
1. Carga el contexto de competencias ICFES:
   ```bash
   gemini --context-file "GEMINI.md" \
          --context-file "ejercicio.Rmd" \
          "Valida este ejercicio contra los estándares ICFES matemáticas: competencia, nivel, contexto colombiano y distractores"
   ```

**Resultado esperado:**
- Verificación de competencia (interpretación/formulación/argumentación)
- Validación de nivel (1-4)
- Evaluación de contexto colombiano
- Análisis de calidad de distractores

### **CASO 4: Optimizar Código R-exams**

**Objetivo:** Mejorar la aleatorización y estructura de un ejercicio.

**Pasos:**
1. Analiza el ejercicio actual:
   ```bash
   gemini --context-file "ejercicio.Rmd" \
          --context-file ".gemini/rules-gemini.md" \
          "Optimiza este ejercicio R-exams: mejora la aleatorización para generar 300+ versiones únicas, optimiza el código R y sugiere mejoras en la estructura"
   ```

**Resultado esperado:**
- Código R optimizado
- Mejor aleatorización de parámetros
- Sugerencias de estructura
- Validación de 300+ versiones únicas

---

## 🔧 **5. SOLUCIÓN DE PROBLEMAS COMUNES**

### **Error: "API key not found"**
```bash
# Verificar variable de entorno
echo $GEMINI_API_KEY

# Si está vacía, configurar:
export GEMINI_API_KEY="tu_api_key_aqui"
```

### **Error: "Context too large"**
```bash
# Usar archivos de contexto más específicos
gemini --context-file ".gemini/rules-gemini.md" "Tu pregunta"

# En lugar de cargar todo el proyecto
```

### **Error: "Command not found: gemini-icfes"**
```bash
# Verificar enlaces simbólicos
ls -la ~/.local/bin/gemini-icfes

# Recrear si es necesario
ln -sf "$(pwd)/Auxiliares/Instalaciones/Ais/Gemini_CLI/gemini-icfes-unified.sh" ~/.local/bin/gemini-icfes
```

### **Respuestas inconsistentes**
```bash
# Verificar configuración de temperatura
cat ~/.config/gemini/icfes-config.json | grep temperature

# Debe ser 0.1 para máxima consistencia
```

### **Problemas con imágenes**
```bash
# Verificar que las imágenes estén incluidas en Git
git ls-files '*.png' '*.jpg' | head -5

# Verificar configuración de .gitignore
.gemini/scripts/verify_gitignore_images.sh
```

---

## 📋 **6. REFERENCIAS RÁPIDAS**

### **Tabla de Comandos Esenciales**

| **Acción** | **Comando** |
|------------|-------------|
| Iniciar modo básico | `gemini-icfes --basic` |
| Analizar ejercicio | `gemini --context-file "ejercicio.Rmd" --context-file ".gemini/rules-gemini.md" "Analiza este ejercicio"` |
| Generar TikZ | `gemini --image "imagen.png" "Genera código TikZ"` |
| Validar ICFES | `gemini --context-file "GEMINI.md" "Valida estándares ICFES"` |
| Verificar setup | `.gemini/scripts/verify_setup.sh` |
| Ayuda | `gemini-icfes --help` |

### **Archivos de Contexto Importantes**

| **Archivo** | **Propósito** |
|-------------|---------------|
| `GEMINI.md` | Contexto completo del proyecto |
| `.gemini/rules-gemini.md` | Reglas específicas para R-exams |
| `.gemini/task-list-gemini.md` | Lista de tareas del proyecto |

### **Atajos de Teclado en Gemini CLI**

| **Tecla** | **Acción** |
|-----------|------------|
| `Ctrl+C` | Salir de Gemini CLI |
| `Ctrl+G` | Ver archivos cargados |
| `@archivo.md` | Cargar archivo específico |
| `/help` | Ayuda dentro de la sesión |

### **Mejores Prácticas**

1. **Siempre usa contexto específico**: Carga `.gemini/rules-gemini.md` para análisis de R-exams
2. **Sé específico en tus preguntas**: "Analiza competencia ICFES" vs "Analiza este ejercicio"
3. **Usa ejemplos del proyecto**: Referencia ejercicios en `Auxiliares/Ejemplos-Funcionales-Rmd/`
4. **Verifica antes de implementar**: Siempre revisa el código generado antes de usarlo
5. **Documenta tus workflows**: Guarda comandos útiles para reutilizar

### **Recursos Adicionales**

- **Tutorial completo**: `Auxiliares/Instalaciones/Ais/Gemini_CLI/gemini-cli-r-exams.md`
- **Ejemplos funcionales**: `Auxiliares/Ejemplos-Funcionales-Rmd/`
- **Documentación TikZ**: `Auxiliares/TikZ-Documentation/`
- **Metodologías**: `Auxiliares/METODOLOGIA_*.md`

---

## 🎨 **7. EJEMPLOS PRÁCTICOS PASO A PASO**

### **EJEMPLO COMPLETO: Crear Ejercicio de Función Cuadrática**

**Escenario:** Necesitas crear un ejercicio sobre funciones cuadráticas con gráfico TikZ.

**Paso 1: Analizar imagen de referencia**
```bash
# Si tienes una imagen de función cuadrática
gemini --image "Auxiliares/Ejemplos-Funcionales-Rmd/funcion_cuadratica.png" \
       --context-file ".gemini/rules-gemini.md" \
       "Analiza esta función cuadrática y genera código TikZ equivalente"
```

**Paso 2: Crear ejercicio R-exams**
```bash
gemini --context-file "GEMINI.md" \
       --context-file ".gemini/rules-gemini.md" \
       "Crea un ejercicio R-exams sobre función cuadrática f(x)=ax²+bx+c con:
       - Aleatorización de parámetros a, b, c
       - 4 opciones de respuesta sobre vértice
       - Competencia: interpretación y representación
       - Nivel 2-3 ICFES
       - Contexto colombiano apropiado"
```

**Paso 3: Validar el ejercicio generado**
```bash
# Guardar el ejercicio generado como ejercicio_cuadratica.Rmd
gemini --context-file "ejercicio_cuadratica.Rmd" \
       --context-file ".gemini/rules-gemini.md" \
       "Valida este ejercicio: sintaxis R-exams, aleatorización, estándares ICFES"
```

### **FLUJO DE TRABAJO TÍPICO**

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   1. ANÁLISIS   │───▶│  2. GENERACIÓN  │───▶│ 3. VALIDACIÓN   │
│                 │    │                 │    │                 │
│ • Imagen/tema   │    │ • Código R-exams│    │ • Estándares    │
│ • Competencia   │    │ • Código TikZ   │    │ • Compilación   │
│ • Nivel ICFES   │    │ • Metadatos     │    │ • Optimización  │
└─────────────────┘    └─────────────────┘    └─────────────────┘
         │                       │                       │
         └───────────────────────┼───────────────────────┘
                                 ▼
                    ┌─────────────────┐
                    │ 4. REFINAMIENTO │
                    │                 │
                    │ • Iteraciones   │
                    │ • Mejoras       │
                    │ • Testing       │
                    └─────────────────┘
```

---

## 💡 **8. CONSEJOS AVANZADOS**

### **Optimización de Prompts**

**❌ Prompt genérico:**
```bash
gemini "Analiza este ejercicio"
```

**✅ Prompt específico:**
```bash
gemini --context-file "ejercicio.Rmd" \
       --context-file ".gemini/rules-gemini.md" \
       "Analiza este ejercicio R-exams enfocándote en:
       1. Sintaxis y compilación
       2. Competencia ICFES específica
       3. Calidad de distractores
       4. Aleatorización de parámetros
       5. Contexto colombiano apropiado"
```

### **Uso Eficiente del Contexto**

**Para análisis rápido:**
```bash
gemini --context-file ".gemini/rules-gemini.md" "Tu pregunta"
```

**Para análisis profundo:**
```bash
gemini --context-file "GEMINI.md" \
       --context-file ".gemini/rules-gemini.md" \
       --context-file "ejercicio.Rmd" \
       "Tu pregunta detallada"
```

### **Comandos de Productividad**

**Crear alias útiles:**
```bash
# Agregar a ~/.bashrc o ~/.zshrc
alias gicfes='gemini-icfes --basic'
alias ganalizar='gemini --context-file ".gemini/rules-gemini.md"'
alias gvalidar='gemini --context-file "GEMINI.md" --context-file ".gemini/rules-gemini.md"'
```

**Scripts personalizados:**
```bash
# Crear script para análisis rápido
cat > analizar_ejercicio.sh << 'EOF'
#!/bin/bash
if [ $# -eq 0 ]; then
    echo "Uso: ./analizar_ejercicio.sh archivo.Rmd"
    exit 1
fi
gemini --context-file "$1" \
       --context-file ".gemini/rules-gemini.md" \
       "Analiza este ejercicio R-exams: estructura, ICFES, optimizaciones"
EOF
chmod +x analizar_ejercicio.sh
```

---

## 🚨 **9. CHECKLIST DE CALIDAD**

### **Antes de Usar un Ejercicio Generado**

- [ ] **Compilación**: ¿El ejercicio compila sin errores?
- [ ] **Aleatorización**: ¿Genera 300+ versiones únicas?
- [ ] **Competencia ICFES**: ¿Está claramente alineado?
- [ ] **Nivel apropiado**: ¿Corresponde al nivel declarado?
- [ ] **Contexto colombiano**: ¿Es culturalmente apropiado?
- [ ] **Distractores**: ¿Son plausibles pero incorrectos?
- [ ] **Gráficos TikZ**: ¿Tienen fidelidad visual 98%+?
- [ ] **Metadatos**: ¿Están completos y correctos?

### **Comandos de Verificación Rápida**

```bash
# Verificar sintaxis R-exams
R -e "library(exams); exams2html('ejercicio.Rmd')"

# Verificar aleatorización
R -e "library(exams); set.seed(123); exams2html('ejercicio.Rmd', n=5)"

# Verificar metadatos
grep -E "^ex(name|type|solution|competencia|nivel):" ejercicio.Rmd
```

---

## 📞 **10. SOPORTE Y RECURSOS**

### **Si Necesitas Ayuda**

1. **Verificación automática**: `.gemini/scripts/verify_setup.sh`
2. **Tutorial completo**: `Auxiliares/Instalaciones/Ais/Gemini_CLI/gemini-cli-r-exams.md`
3. **Ejemplos funcionales**: `Auxiliares/Ejemplos-Funcionales-Rmd/`
4. **Configuración**: `~/.config/gemini/icfes-config.json`

### **Recursos de Aprendizaje**

- **Documentación R-exams**: https://www.r-exams.org/
- **Marco ICFES Matemáticas**: Consulta `GEMINI.md`
- **TikZ/PGF Manual**: `Auxiliares/TikZ-Documentation/`
- **Metodologías del proyecto**: `Auxiliares/METODOLOGIA_*.md`

### **Comunidad y Actualizaciones**

- **Repositorio GitHub**: `alvaretto/proyecto-r-exams-icfes-matematicas-optimizado`
- **Rama de desarrollo**: `experimentos-seguros`
- **Issues y mejoras**: Crear issues en GitHub para reportar problemas

---

**🎯 OBJETIVO ALCANZADO: Con este manual puedes usar Gemini CLI productivamente para crear ejercicios R-exams ICFES de alta calidad en 15-20 minutos.**

**📈 PRÓXIMO NIVEL: Una vez domines estos comandos básicos, explora el tutorial completo para funcionalidades avanzadas y automatización de workflows.**

---

*Manual creado por: Especialista en Integración IA Educativa*\
*Versión: 1.0 | Fecha: Agosto 2025*\
*Proyecto: RepositorioMatematicasICFES_R_Exams*\
*Última actualización: Agosto 24, 2025*
