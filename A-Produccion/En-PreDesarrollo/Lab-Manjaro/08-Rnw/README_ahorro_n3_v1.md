# Ejercicio ICFES Nivel 3: Ahorro - Interpretación y Representación

## 📋 Información General

**Archivo:** `ahorro_interpretacion_representacion_n3_v1.Rnw`  
**Nivel ICFES:** 3 (Puntaje 51-70)  
**Competencia:** Interpretación y Representación  
**Tipo:** Selección múltiple (schoice)  

## 🎯 Características del Nivel 3

### Diferencias clave respecto al Nivel 2:

**Nivel 2 (archivo original):**
- Comparación directa de totales
- Cálculos aritméticos simples
- Interpretación literal de tablas
- Decisión basada en comparación numérica básica

**Nivel 3 (este ejercicio):**
- **Análisis de múltiples representaciones**: Tabla de datos + información estadística calculada
- **Manipulaciones aritméticas complejas**: Tasas de crecimiento, eficiencia relativa
- **Selección de representación óptima**: Evaluar cuál gráfica es más adecuada para un análisis específico
- **Análisis de tendencias**: Patrones de crecimiento temporal
- **Razonamiento sobre eficiencia relativa**: Normalización de diferencias en puntos de partida

## 📊 Estructura del Ejercicio

### Contexto
Tres estudiantes (Ana, Bruno, Carla) implementan diferentes estrategias de ahorro durante 6 meses.

### Datos Presentados
1. **Tabla principal**: Ahorro mensual de cada estudiante (6 meses)
2. **Información calculada**:
   - Totales acumulados por estudiante
   - Tasas de crecimiento promedio mensual
   - Datos que permiten análisis de eficiencia relativa

### Pregunta Nivel 3
"¿Cuál representación es la MÁS ADECUADA para analizar y comparar la eficiencia relativa de las estrategias de ahorro?"

### Opciones de Respuesta
A. Gráfica de barras con totales acumulados
B. Gráfica de líneas con evolución mensual  
C. **Gráfica de barras con tasas de crecimiento porcentual** ✅
D. Tabla con valores inicial y final únicamente

## 🧠 Análisis Cognitivo Nivel 3

### Habilidades Requeridas:
1. **Síntesis de información múltiple**: Combinar datos de tabla + estadísticas calculadas
2. **Comprensión de representaciones**: Entender cuándo usar cada tipo de gráfica
3. **Análisis de eficiencia relativa**: Normalizar diferencias en puntos de partida
4. **Evaluación de métodos**: Seleccionar la herramienta analítica más apropiada
5. **Razonamiento proporcional**: Entender tasas vs. valores absolutos

### Por qué es Nivel 3:
- **No es cálculo directo**: Requiere selección de método analítico
- **Múltiples variables**: Considera tiempo, crecimiento, eficiencia
- **Abstracción**: Evalúa representaciones, no solo datos
- **Justificación**: Requiere argumentar por qué una opción es superior

## 💡 Marco Teórico ICFES

### Competencia: Interpretación y Representación
- **Nivel 2**: Lectura directa e interpretación básica
- **Nivel 3**: Selección y evaluación de representaciones apropiadas
- **Nivel 4**: Construcción y crítica de representaciones complejas

### Componente: Aleatorio (Estadística)
- Análisis de datos temporales
- Comparación de tendencias
- Interpretación de tasas de cambio

## 🔧 Características Técnicas

### Configuración R/LaTeX:
- **Semilla aleatoria**: Para reproducibilidad
- **Formato números**: Sin notación científica
- **Librerías**: exams, xtable para tablas
- **Encoding**: UTF-8 con soporte español

### Datos Generados:
```r
# Ana: Crecimiento constante
ana_base <- c(50, 65, 80, 95, 110, 125)

# Bruno: Crecimiento acelerado inicial, luego estable  
bruno_base <- c(30, 55, 75, 85, 90, 95)

# Carla: Crecimiento irregular pero positivo
carla_base <- c(40, 45, 70, 65, 85, 100)
```

### Cálculos Nivel 3:
- Tasas de crecimiento promedio mensual
- Eficiencia relativa (final/inicial)
- Identificación del estudiante más eficiente

## 📈 Resultados Esperados

### Análisis de Eficiencia:
- **Bruno**: Mayor tasa de crecimiento relativo (216.7% total)
- **Ana**: Crecimiento constante pero menor eficiencia relativa
- **Carla**: Crecimiento irregular, eficiencia intermedia

### Justificación Respuesta Correcta (C):
Las tasas de crecimiento porcentual son la representación más adecuada porque:
1. **Normalizan** las diferencias en montos iniciales
2. **Permiten comparación directa** de eficiencia relativa
3. **Muestran efectividad** independiente del capital inicial
4. **Facilitan identificación** del mejor desempeño proporcional

## 🎯 Objetivos Pedagógicos

1. **Desarrollar pensamiento analítico**: Más allá de cálculos básicos
2. **Fortalecer interpretación gráfica**: Selección de representaciones apropiadas
3. **Promover razonamiento proporcional**: Eficiencia vs. valores absolutos
4. **Fomentar justificación matemática**: Argumentar selecciones

## 🔄 Generación y Pruebas

### Comando para generar HTML:
```r
library(exams)
set.seed(12345)
exams2html('ahorro_interpretacion_representacion_n3_v1.Rnw',
           name = 'ejercicio_ahorro_n3_test',
           dir = './salida',
           template = 'plain.html')
```

### Verificación de Funcionamiento:
- ✅ Estructura LaTeX correcta
- ✅ Chunks R funcionales
- ✅ Metadatos exams completos
- ✅ Opciones de respuesta balanceadas
- ✅ Solución detallada incluida

## 📚 Referencias Teóricas

### Marco ICFES Matemáticas:
- **Competencia**: Interpretación y Representación
- **Nivel**: 3 (Análisis y selección de representaciones)
- **Contexto**: Familiar/Laboral (ahorro personal)
- **Componente**: Aleatorio (análisis de datos)

### Diferenciación por Niveles:
- **N1**: Identificación básica
- **N2**: Comparación directa ← archivo original
- **N3**: Análisis y selección ← este ejercicio
- **N4**: Construcción y crítica avanzada

---

**Creado por:** Agente Augment  
**Fecha:** 2025-01-16  
**Versión:** 1.0  
**Estado:** Listo para pruebas y implementación
