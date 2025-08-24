# Reglas Específicas para Gemini CLI - Proyecto R-exams ICFES

## 🎯 **REGLAS GENERALES DE COMPORTAMIENTO**

### **Comunicación**
- SIEMPRE responder en español
- Usar terminología técnica precisa
- Explicar conceptos complejos paso a paso
- Proporcionar ejemplos concretos cuando sea posible

### **Enfoque de Trabajo**
- Priorizar calidad sobre velocidad
- Implementar soluciones completas, no simplificadas
- Documentar decisiones y razonamientos
- Validar resultados antes de presentarlos

## 📚 **REGLAS ESPECÍFICAS PARA R-EXAMS**

### **Estructura de Ejercicios**
- SIEMPRE incluir metadatos ICFES completos
- Generar mínimo 300 versiones únicas
- Usar sintaxis R-exams estándar
- Incluir validación de respuestas

### **Aleatorización**
- Variar parámetros numéricos significativamente
- Cambiar contextos manteniendo competencia
- Rotar opciones de respuesta (A, B, C, D)
- Evitar patrones predecibles

### **Formato de Código**
```r
# Estructura requerida para ejercicios R-exams
<<echo=FALSE, results=hide>>=
# Parámetros aleatorios
# Cálculos
# Generación de opciones
@

Question
========
[Contexto del problema]

[Pregunta específica]

Answerlist
----------
* Opción A
* Opción B  
* Opción C
* Opción D

Solution
========
[Explicación detallada]

Meta-information
================
exname: [Nombre del ejercicio]
extype: schoice
exsolution: [Patrón de respuesta]
exshuffle: TRUE
expoints: 1
extol: 0.01
excompetencia: [interpretacion_representacion|formulacion_ejecucion|argumentacion]
exnivel: [1|2|3|4]
extema: [Tema específico]
```

## 🎨 **REGLAS PARA GRÁFICOS TIKZ**

### **Estilo Visual**
- Elementos de texto en negrita cursiva: `\bfseries\itshape`
- Usar escala apropiada: `scale=1.0` por defecto
- Colores consistentes con paleta del proyecto
- Líneas de grosor apropiado: `thick` para elementos principales

### **Estructura de Código**
```latex
\begin{tikzpicture}[scale=1.0, font=\small]
  % Configuración inicial
  \tikzset{
    estilo1/.style={definición},
    estilo2/.style={definición}
  }
  
  % Elementos principales
  % Ejes, grillas, funciones
  
  % Etiquetas y anotaciones
  \node[font=\bfseries\itshape] at (x,y) {Texto};
\end{tikzpicture}
```

### **Fidelidad Visual**
- Mantener proporciones exactas de imagen original
- Replicar colores con precisión
- Conservar posicionamiento relativo
- Objetivo: 98% de fidelidad visual

## 🔍 **REGLAS PARA ANÁLISIS DE EJERCICIOS**

### **Aspectos a Evaluar**
1. **Técnicos**: Sintaxis, compilación, aleatorización
2. **Pedagógicos**: Competencia, nivel, contexto
3. **Visuales**: Gráficos, formato, accesibilidad
4. **ICFES**: Alineación con estándares oficiales

### **Formato de Análisis**
```markdown
## Análisis de Ejercicio: [Nombre]

### ✅ Fortalezas
- [Lista de aspectos positivos]

### ⚠️ Áreas de Mejora
- [Lista de aspectos a mejorar]

### 🔧 Recomendaciones Específicas
- [Sugerencias concretas de implementación]

### 📊 Evaluación ICFES
- Competencia: [Evaluación]
- Nivel: [Evaluación]
- Contexto: [Evaluación]
```

## 🚀 **REGLAS PARA GENERACIÓN DE CONTENIDO**

### **Contextos Apropiados**
- Usar situaciones colombianas relevantes
- Evitar referencias culturales específicas
- Mantener neutralidad de género
- Incluir diversidad en ejemplos

### **Progresión de Dificultad**
- Nivel 1: Conceptos básicos, aplicación directa
- Nivel 2: Relaciones simples, un paso de razonamiento
- Nivel 3: Múltiples pasos, conexiones conceptuales
- Nivel 4: Razonamiento complejo, síntesis

### **Distractores Efectivos**
- Basados en errores conceptuales comunes
- Plausibles pero incorrectos
- No trivialmente descartables
- Educativamente informativos

## 📋 **REGLAS PARA VALIDACIÓN**

### **Checklist Técnico**
- [ ] Compilación exitosa en PDF
- [ ] Compilación exitosa en HTML
- [ ] Aleatorización funcional
- [ ] Metadatos completos
- [ ] Sintaxis R-exams válida

### **Checklist Pedagógico**
- [ ] Competencia ICFES clara
- [ ] Nivel apropiado
- [ ] Contexto relevante
- [ ] Distractores bien diseñados
- [ ] Solución completa

### **Checklist Visual**
- [ ] Gráficos TikZ optimizados
- [ ] Fidelidad visual 98%+
- [ ] Elementos accesibles
- [ ] Formato consistente

## ⚠️ **RESTRICCIONES Y LIMITACIONES**

### **NO Hacer**
- No usar APIs externas sin autorización
- No generar contenido inapropiado o sesgado
- No ignorar estándares ICFES establecidos
- No crear ejercicios sin validación

### **Limitaciones Técnicas**
- R-exams tiene sintaxis específica obligatoria
- TikZ requiere compilación LaTeX
- Algunos formatos tienen restricciones
- Aleatorización debe ser matemáticamente válida

### **Consideraciones Éticas**
- Respetar derechos de autor
- Mantener neutralidad política
- Evitar estereotipos
- Promover inclusión educativa

## 🔄 **REGLAS PARA ITERACIÓN Y MEJORA**

### **Proceso de Refinamiento**
1. Generar versión inicial
2. Validar técnicamente
3. Revisar pedagógicamente
4. Optimizar visualmente
5. Documentar cambios

### **Criterios de Aceptación**
- Compilación exitosa en todos los formatos
- Validación pedagógica positiva
- Fidelidad visual objetivo alcanzada
- Documentación completa

### **Feedback y Mejora Continua**
- Incorporar sugerencias del usuario
- Aprender de errores anteriores
- Actualizar según nuevos estándares
- Mantener registro de mejores prácticas
