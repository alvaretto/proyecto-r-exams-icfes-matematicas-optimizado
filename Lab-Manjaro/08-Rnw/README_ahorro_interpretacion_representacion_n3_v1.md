# README: Ejercicio de Ahorro - Interpretación y Representación (Nivel 3)

## 📋 Información General

**Archivo:** `ahorro_interpretacion_representacion_n3_v1.Rnw`  
**Tipo:** Ejercicio R-exams en formato .Rnw (LaTeX + R)  
**Nivel:** 3 (Análisis avanzado)  
**Tema:** Interpretación de representaciones gráficas de datos de ahorro  
**Formato:** Opción múltiple con 4 alternativas (A, B, C, D)

## 🎯 Objetivo del Ejercicio

Este ejercicio evalúa la capacidad del estudiante para:
- Analizar diferentes tipos de representaciones gráficas
- Seleccionar la representación más adecuada según el contexto
- Interpretar datos de ahorro y eficiencia relativa
- Aplicar pensamiento crítico en la selección de gráficas

## 📊 Estructura del Ejercicio

### Datos Base
El ejercicio presenta datos de ahorro de **3 estudiantes** durante **6 meses**:

- **Ana**: Crecimiento constante (50→125 mil pesos)
- **Bruno**: Crecimiento acelerado inicial, luego estable (30→95 mil pesos)  
- **Carla**: Crecimiento irregular con tendencia positiva (40→100 mil pesos)

### Tipos de Gráficas Generadas
1. **Gráfica A**: Totales acumulados por estudiante
2. **Gráfica B**: Evolución mensual del ahorro
3. **Gráfica C**: Tasas de crecimiento promedio mensual
4. **Gráfica D**: Comparación inicial vs final

## 🔄 Sistema de Aleatorización

### Escenarios Dinámicos
El ejercicio incluye **4 escenarios diferentes** que se seleccionan aleatoriamente:

1. **Escenario 1**: Análisis de eficiencia relativa (Respuesta correcta: C)
2. **Escenario 2**: Comparación de montos totales (Respuesta correcta: A)
3. **Escenario 3**: Análisis de tendencias temporales (Respuesta correcta: B)
4. **Escenario 4**: Comparación simple inicial vs final (Respuesta correcta: D)

### Aleatorización de Opciones
- Las opciones A, B, C, D se presentan en **orden aleatorio**
- La respuesta correcta **varía** según el escenario y la aleatorización
- Esto evita patrones predecibles en las respuestas

## 🛠️ Características Técnicas

### Configuración Anti-Notación Científica
```r
options(scipen = 999)  # Evitar notación científica
options(digits = 10)   # Suficientes dígitos
Sys.setlocale(category = "LC_NUMERIC", locale = "C")
```

### Función de Formateo
```r
formatear_entero <- function(x) {
  formatC(as.integer(round(x)), format='d', big.mark='')
}
```

### Generación de Gráficas
- Utiliza la función `generar_grafica()` que crea 4 tipos diferentes
- Cada gráfica se genera con parámetros específicos (colores, etiquetas, etc.)
- Las gráficas se insertan dinámicamente en el documento

## 📝 Estructura del Documento

### Sección de Pregunta
1. **Tabla de datos**: Muestra los valores mensuales de cada estudiante
2. **Información calculada**: Totales y tasas de crecimiento
3. **Contexto del escenario**: Descripción de la situación específica
4. **Pregunta**: Solicita seleccionar la mejor representación gráfica
5. **Opciones**: 4 gráficas con sus respectivas explicaciones

### Sección de Solución
1. **Introducción**: Explica el criterio de análisis según el escenario
2. **Análisis detallado**: Evalúa cada gráfica presentada
3. **Justificación**: Explica por qué una opción es correcta y las otras no
4. **Datos de referencia**: Incluye las tasas de crecimiento calculadas
5. **Conclusión**: Identifica al estudiante más eficiente

## 🚀 Cómo Usar Este Ejercicio

### Requisitos Previos
- R instalado con las librerías: `exams`
- LaTeX instalado para compilación
- Conocimientos básicos de R-exams

### Generar HTML
```r
library(exams)
exams2html('ahorro_interpretacion_representacion_n3_v1.Rnw', 
           name='ahorro_test', 
           dir='output')
```

### Generar PDF
```r
library(exams)
exams2pdf('ahorro_interpretacion_representacion_n3_v1.Rnw', 
          name='ahorro_test', 
          dir='output')
```

### Generar para Moodle
```r
library(exams)
exams2moodle('ahorro_interpretacion_representacion_n3_v1.Rnw', 
             name='ahorro_test', 
             dir='output')
```

## 🎓 Valor Pedagógico

### Competencias Evaluadas
- **Interpretación de datos**: Lectura y comprensión de tablas numéricas
- **Análisis gráfico**: Comparación entre diferentes tipos de representaciones
- **Pensamiento crítico**: Selección de la herramienta más adecuada
- **Contextualización**: Aplicación según la situación específica

### Nivel de Dificultad
- **Nivel 3**: Requiere análisis, síntesis y evaluación
- **Pensamiento de orden superior**: No solo memorización
- **Aplicación práctica**: Situaciones realistas de análisis financiero

## 🔧 Personalización

### Modificar Datos
Para cambiar los datos de ahorro, editar las variables:
```r
ana_base <- c(50, 65, 80, 95, 110, 125)
bruno_base <- c(30, 55, 75, 85, 90, 95)
carla_base <- c(40, 45, 70, 65, 85, 100)
```

### Agregar Escenarios
Para añadir nuevos escenarios, agregar elementos a la lista `escenarios`:
```r
list(
  contexto = "Nuevo contexto...",
  pregunta = "Nueva pregunta...",
  correcta = X,  # Número de opción correcta
  argumentos = c("Argumento A", "Argumento B", ...)
)
```

### Modificar Gráficas
La función `generar_grafica()` puede personalizarse para:
- Cambiar colores
- Modificar títulos
- Ajustar escalas
- Añadir elementos visuales

## 📋 Metadatos del Ejercicio

```
%% META-INFORMATION
%% \extype{schoice}
%% \exsolution{Variable según aleatorización}
%% \exname{Ahorro Interpretacion Representacion N3 V1}
```

## 🐛 Solución de Problemas

### Error de Compilación
- Verificar que todas las librerías estén instaladas
- Comprobar la sintaxis LaTeX
- Revisar que los chunks de R estén correctamente cerrados

### Gráficas No Aparecen
- Verificar que `fig=TRUE` esté en los chunks correspondientes
- Comprobar los parámetros de tamaño: `height=3, width=4`
- Asegurar que `eps=FALSE` esté configurado

### Aleatorización No Funciona
- Verificar que `set.seed()` esté configurado correctamente
- Comprobar que las variables de aleatorización estén bien definidas

## 🧪 Testing y Validación

### Pruebas Recomendadas
1. **Generar múltiples versiones** para verificar la aleatorización:
```r
# Generar 10 versiones para probar
for(i in 1:10) {
  exams2html('ahorro_interpretacion_representacion_n3_v1.Rnw',
             name=paste0('test_', i),
             dir='testing')
}
```

2. **Verificar que todas las opciones aparezcan como correctas**
3. **Comprobar que las gráficas se muestren correctamente**
4. **Validar que los textos tengan ortografía correcta**

### Lista de Verificación
- [ ] Las 4 gráficas se generan correctamente
- [ ] Cada gráfica aparece con su opción correspondiente (A, B, C, D)
- [ ] Los 4 escenarios funcionan
- [ ] La aleatorización cambia el orden de las opciones
- [ ] La respuesta correcta varía según el escenario
- [ ] Los cálculos matemáticos son precisos
- [ ] No hay notación científica en los números
- [ ] La ortografía es correcta (tildes incluidas)

## 💡 Mejores Prácticas

### Para Docentes
1. **Revisar antes de usar**: Generar varias versiones para verificar funcionamiento
2. **Explicar el contexto**: Asegurar que los estudiantes entiendan los diferentes tipos de análisis
3. **Tiempo recomendado**: 15-20 minutos por ejercicio
4. **Prerrequisitos**: Conocimientos básicos de interpretación de gráficas

### Para Desarrolladores
1. **Mantener consistencia**: Usar el mismo estilo en todos los ejercicios
2. **Documentar cambios**: Actualizar este README al modificar el ejercicio
3. **Probar exhaustivamente**: Verificar todas las combinaciones posibles
4. **Backup regular**: Mantener versiones anteriores como respaldo

## 🔍 Análisis de Resultados

### Interpretación de Respuestas
- **Opción A elegida**: El estudiante prioriza montos totales
- **Opción B elegida**: El estudiante valora el análisis temporal
- **Opción C elegida**: El estudiante comprende la eficiencia relativa
- **Opción D elegida**: El estudiante prefiere comparaciones simples

### Indicadores de Calidad
- **Distribución equilibrada**: Las 4 opciones deberían elegirse con frecuencia similar
- **Correlación con nivel**: Estudiantes avanzados deberían identificar mejor el contexto
- **Tiempo de respuesta**: Indicador de dificultad del ejercicio

## 📈 Estadísticas del Ejercicio

### Datos Calculados Automáticamente
- **Totales de ahorro**: Ana (525), Bruno (430), Carla (405)
- **Tasas de crecimiento**: Ana (30%), Bruno (43.3%), Carla (30%)
- **Eficiencia relativa**: Bruno es el más eficiente proporcionalmente
- **Mayor ahorrador**: Ana en términos absolutos

### Variabilidad
- **4 escenarios** × **24 permutaciones de orden** = **96 versiones únicas**
- Cada estudiante verá una versión diferente
- Imposible memorizar patrones de respuesta

## 📚 Referencias

- [Documentación R-exams](http://www.r-exams.org/)
- [Manual de LaTeX](https://www.latex-project.org/help/documentation/)
- [Guía de gráficas en R](https://www.r-graph-gallery.com/)
- [Ejemplos R-exams](http://www.r-exams.org/templates/)

## 📞 Soporte

### Problemas Comunes
1. **Error de compilación**: Verificar instalación de LaTeX y librerías R
2. **Gráficas no aparecen**: Comprobar configuración de chunks
3. **Aleatorización falla**: Revisar función `sample()` y semillas

### Contacto
Para reportar problemas o sugerir mejoras, documentar en el sistema de seguimiento del proyecto.

---

**Autor**: Sistema R-exams ICFES Matemáticas
**Versión**: 1.0
**Fecha**: 2024
**Licencia**: Uso educativo
**Última actualización**: README creado con correcciones ortográficas aplicadas
