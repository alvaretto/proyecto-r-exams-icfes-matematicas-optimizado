# 🤖 PLAN MAESTRO GEMINI CLI: Ejercicios ICFES R-exams

## 📋 ESTRUCTURA CONVERSACIONAL DE TAREAS GEMINI

### 🎯 **FASE 0: Inicialización Inteligente Gemini**
*Configuración contextual y análisis preliminar con IA conversacional*

- [ ] **🤖 0.1 Activación del Contexto Gemini**
  - Cargar reglas específicas: `@Auxiliares/Instalaciones/Ais/Gemini_CLI/rules-gemini.md`
  - Establecer contexto del proyecto: `@Auxiliares/rules_full/rules_full_v1.md`
  - Configurar herramientas disponibles: web search, file editing, code execution
  - Validar acceso a ejemplos funcionales: `@Auxiliares/Ejemplos-Funcionales-Rmd/`

- [ ] **🧠 0.2 Análisis Conversacional de Objetivos**
  - **Comando Gemini**: `"Analiza el objetivo de este ejercicio y sugiere el mejor enfoque"`
  - Identificar tipo de problema matemático mediante conversación
  - Determinar competencia ICFES más apropiada con justificación
  - Establecer nivel de dificultad basado en análisis contextual
  - Planificar estrategia de desarrollo personalizada

- [ ] **🔍 0.3 Investigación Automática ICFES**
  - **Comando Gemini**: `"Busca información actualizada sobre [competencia específica] ICFES 2025"`
  - Validar definiciones oficiales de competencias
  - Contrastar con documentación MEN actualizada
  - Verificar contextos y niveles de desempeño
  - Integrar hallazgos en planificación del ejercicio

### 🎨 **FASE 1: Análisis Inteligente de Imagen con Gemini**
*Sistema conversacional de detección y procesamiento de contenido visual*

- [ ] **📷 1.1 Carga y Análisis Contextual**
  - Subir imagen PNG al contexto de Gemini CLI
  - **Comando Gemini**: `"Analiza esta imagen y describe todos los elementos matemáticos presentes"`
  - Identificar tipo de contenido: gráficas, tablas, diagramas, texto
  - Extraer información matemática relevante mediante conversación
  - Clasificar complejidad visual para decisión de flujo

- [ ] **🤖 1.2 Sistema Condicional Automático Gemini**
  - **Comando Gemini**: `"Aplica el sistema condicional automático a esta imagen"`
  - **Decisión Inteligente**:
    * **FLUJO A** (contenido simple): Proceso conversacional estándar
    * **FLUJO B** (contenido complejo): Activar Agente-Graficador Gemini
  - Justificación automática de la decisión tomada
  - Configuración de parámetros específicos según flujo

- [ ] **🎯 1.3 Agente-Graficador Gemini (Solo FLUJO B)**
  - **Comando Gemini**: `"Activa el Agente-Graficador para replicar esta imagen con TikZ"`
  - **Proceso Iterativo Conversacional**:
    * Análisis detallado de elementos gráficos
    * Generación de código TikZ inicial
    * Validación visual automática
    * Refinamiento basado en feedback
    * Alcanzar 98%+ fidelidad mediante iteración
  - **Validación Interactiva**: Comparación lado a lado con aprobación

### ⚙️ **FASE 2: Configuración Técnica Inteligente**
*Implementación automatizada con validación conversacional*

- [ ] **📄 2.1 Generación Automática de Estructura**
  - **Comando Gemini**: `"Genera la estructura base del ejercicio con configuración óptima"`
  - Crear encabezado YAML con metadatos ICFES apropiados
  - Configurar chunks de setup con mejores prácticas
  - Establecer configuración TikZ/LaTeX según necesidades detectadas
  - Validar compatibilidad multi-formato automáticamente

- [ ] **🔧 2.2 Configuración Numérica Inteligente**
  - **Comando Gemini**: `"Configura las opciones numéricas para máxima compatibilidad"`
  - Aplicar configuración estándar: `options(OutDec = ".", scipen = 999)`
  - Establecer locale apropiado: `Sys.setlocale("LC_NUMERIC", "C")`
  - Configurar precisión y formato según tipo de ejercicio
  - Documentar decisiones de configuración automáticamente

- [ ] **🎨 2.3 Preparación TikZ/Python Contextual**
  - **Comando Gemini**: `"Prepara la configuración gráfica óptima para este ejercicio"`
  - Seleccionar entre TikZ o Python según análisis de imagen
  - Configurar paquetes LaTeX necesarios automáticamente
  - Establecer templates apropiados basados en ejemplos funcionales
  - Validar configuración con compilación de prueba

### 🎲 **FASE 3: Generación Inteligente de Datos**
*Aleatorización optimizada con validación automática*

- [ ] **🔢 3.1 Función generar_datos() Conversacional**
  - **Comando Gemini**: `"Crea una función de generación de datos optimizada para este ejercicio"`
  - Análisis de parámetros matemáticamente relevantes
  - Implementación de diversidad pedagógica (no cosmética)
  - Configuración de rangos realistas y coherentes
  - Integración de validaciones matemáticas automáticas

- [ ] **✅ 3.2 Validación Automática de Diversidad**
  - **Comando Gemini**: `"Valida que la función genera 300+ versiones únicas"`
  - Ejecución automática de test de diversidad
  - Análisis de distribución de parámetros
  - Identificación de casos extremos o problemáticos
  - Optimización automática si es necesario

- [ ] **🛡️ 3.3 Testing Matemático Inteligente**
  - **Comando Gemini**: `"Implementa validaciones matemáticas robustas"`
  - Verificación de coherencia entre parámetros
  - Manejo de casos extremos y divisiones por cero
  - Validación de rangos y límites realistas
  - Documentación automática de restricciones

### 📈 **FASE 4: Visualizaciones Inteligentes**
*Generación automática de gráficos con máxima calidad*

- [ ] **🎨 4.1 Implementación TikZ Conversacional (PRIORIDAD)**
  - **Comando Gemini**: `"Genera el código TikZ para replicar la imagen con 98% fidelidad"`
  - Consulta automática de ejemplos funcionales relevantes
  - Aplicación de templates especializados según tipo
  - Configuración de `include_tikz()` con parámetros óptimos
  - Validación visual automática con métricas de fidelidad

- [ ] **🐍 4.2 Alternativa Python Inteligente (Si TikZ no viable)**
  - **Comando Gemini**: `"Implementa gráficos Python como alternativa optimizada"`
  - Configuración automática de matplotlib con mejores prácticas
  - Transferencia inteligente de datos R→Python
  - Aplicación de templates validados de ejemplos funcionales
  - Verificación de compatibilidad multi-formato

- [ ] **📊 4.3 Validación Visual Automática**
  - **Comando Gemini**: `"Valida la calidad visual y fidelidad de los gráficos"`
  - Comparación automática con imagen original
  - Verificación de proporciones, colores y posicionamiento
  - Cálculo de métricas de fidelidad cuantificables
  - Iteración automática hasta alcanzar 98%+ fidelidad

### 📝 **FASE 5: Contenido Pedagógico Inteligente**
*Desarrollo conversacional de ejercicio con IA educativa*

- [ ] **❓ 5.1 Generación Inteligente de Question**
  - **Comando Gemini**: `"Desarrolla la sección Question con contexto realista y pregunta clara"`
  - Creación de contexto matemáticamente relevante
  - Formulación de pregunta según competencia ICFES específica
  - Integración natural de gráficos/tablas generados
  - Validación de claridad y nivel de dificultad apropiado

- [ ] **🎯 5.2 Sistema Avanzado de Distractores Gemini**
  - **Comando Gemini**: `"Genera distractores pedagógicos avanzados con errores conceptuales reales"`
  - Implementación de sistema de valores duplicados (30% probabilidad)
  - Creación de 8+ tipos de errores conceptuales diversos
  - Generación de justificaciones alternativas plausibles pero incorrectas
  - Verificación automática de unicidad textual de las 4 opciones

- [ ] **💡 5.3 Solution Detallada Conversacional**
  - **Comando Gemini**: `"Desarrolla una explicación completa y pedagógica de la solución"`
  - Explicación paso a paso del proceso matemático
  - Justificación de por qué cada distractor es incorrecto
  - Integración de elementos visuales en la explicación
  - Validación de coherencia pedagógica y matemática

### ⚙️ **FASE 6: Configuración Avanzada de Evaluación**
*Optimización automática de tolerancias y meta-información*

- [ ] **🎯 6.1 Configuración Inteligente de Tolerancias**
  - **Comando Gemini**: `"Configura tolerancias apropiadas para evaluación automática"`
  - Identificación automática de tipos de respuesta (schoice vs numéricas)
  - Aplicación de tolerancias estándar: 0 para schoice, ≥1 para numéricas
  - Documentación automática de configuración con comentarios explicativos
  - Validación mediante tests automáticos de evaluación

- [ ] **📋 6.2 Meta-información ICFES Optimizada**
  - **Comando Gemini**: `"Genera meta-información completa y validada"`
  - Configuración automática de extype, exsolution, extol
  - Validación de coherencia con estructura del ejercicio
  - Documentación de decisiones de configuración
  - Verificación de compatibilidad con sistema exams

- [ ] **✅ 6.3 Testing de Evaluación Automática**
  - **Comando Gemini**: `"Valida que la evaluación automática funciona correctamente"`
  - Simulación de respuestas correctas e incorrectas
  - Verificación de aceptación dentro de tolerancias
  - Prueba de rechazo fuera de tolerancias
  - Documentación de casos de prueba para referencia

### 🔧 **FASE 7: Validación y Corrección Inteligente**
*Sistema automático de detección y corrección de errores*

- [ ] **🔍 7.1 Detección Automática de Errores**
  - **Comando Gemini**: `"Analiza el ejercicio completo e identifica errores potenciales"`
  - Aplicación de metodología de corrección de errores recurrentes
  - Verificación de 5 categorías: gramática, posicionamiento, datos, compilación, estructura
  - Consulta automática de biblioteca de soluciones probadas
  - Generación de reporte de errores encontrados

- [ ] **🛠️ 7.2 Corrección Automática Inteligente**
  - **Comando Gemini**: `"Aplica correcciones basadas en mejores prácticas del proyecto"`
  - Implementación de soluciones validadas de ejemplos funcionales
  - Corrección de concordancia de género automática
  - Ajuste de posicionamiento TikZ según patrones probados
  - Validación de opciones únicas y configuraciones LaTeX

- [ ] **✅ 7.3 Validación Final Conversacional**
  - **Comando Gemini**: `"Ejecuta validación completa del ejercicio corregido"`
  - Compilación automática en múltiples formatos (HTML, PDF, Word)
  - Verificación de que todas las correcciones se mantienen
  - Testing de diversidad de versiones post-corrección
  - Confirmación de calidad visual y pedagógica final

### 🚀 **FASE 8: Optimización y Entrega Final**
*Pulimiento final y documentación automática*

- [ ] **🎯 8.1 Optimización Final Inteligente**
  - **Comando Gemini**: `"Optimiza el ejercicio para máximo rendimiento y calidad"`
  - Simplificación de código sin pérdida de funcionalidad
  - Optimización de velocidad de generación de versiones
  - Mejora de legibilidad y mantenibilidad del código
  - Validación de mejores prácticas del proyecto

- [ ] **📚 8.2 Documentación Automática**
  - **Comando Gemini**: `"Genera documentación completa del ejercicio desarrollado"`
  - Creación de comentarios explicativos en código
  - Documentación de decisiones de diseño pedagógico
  - Registro de configuraciones técnicas aplicadas
  - Guía de uso y modificación futura

- [ ] **✅ 8.3 Entrega y Validación Final**
  - **Comando Gemini**: `"Ejecuta validación final completa y prepara entrega"`
  - Testing exhaustivo en todos los formatos de salida
  - Verificación de cumplimiento de estándares ICFES
  - Confirmación de 300+ versiones únicas funcionales
  - Generación de reporte de calidad final

---

## 🤖 **COMANDOS GEMINI RÁPIDOS POR FASE**

### **Inicio Rápido**
```
"Analiza esta imagen y crea un ejercicio ICFES completo siguiendo las mejores prácticas"
"Aplica el sistema condicional automático y genera el ejercicio optimizado"
```

### **Análisis Específico**
```
"Identifica qué competencia ICFES evalúa mejor esta imagen"
"Determina si necesito FLUJO A o FLUJO B para esta imagen"
"Analiza la calidad pedagógica de este ejercicio existente"
```

### **Generación Dirigida**
```
"Crea la función generar_datos() optimizada para este problema"
"Genera distractores pedagógicos avanzados con errores reales"
"Implementa TikZ con 98% fidelidad para esta imagen"
```

### **Validación y Corrección**
```
"Identifica y corrige todos los errores en este ejercicio"
"Valida que cumple estándares ICFES y genera 300+ versiones"
"Optimiza el código manteniendo la calidad pedagógica"
```

---

**🎯 OBJETIVO FINAL**: Crear ejercicios ICFES matemáticos de máxima calidad mediante conversación inteligente con Gemini CLI, aprovechando todas sus capacidades avanzadas para optimizar tiempo, calidad y eficiencia pedagógica.
