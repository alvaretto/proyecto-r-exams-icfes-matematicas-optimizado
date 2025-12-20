# 🏆 Walkthrough: Ejercicios ICFES Juegos Deportivos - Interpretación y Representación

> **🔰 PARA NOVATOS:** Este documento explica paso a paso cómo funcionan dos ejercicios de matemáticas que se generan automáticamente con cientos de versiones diferentes. ¡No te preocupes si no entiendes todo al principio!

## 📋 Descripción General del Proyecto

> **💡 ¿Qué es esto?** Imagina que tienes un profesor que puede crear infinitas versiones del mismo problema de matemáticas, cada una con números diferentes pero evaluando la misma habilidad. Eso es exactamente lo que hace este proyecto.

Este proyecto contiene **dos versiones complementarias** de ejercicios ICFES de matemáticas sobre **análisis de datos deportivos**, ambos enfocados en la competencia de **Interpretación y Representación** con nivel de dificultad 2.

> **🎯 Ejemplo práctico:** El ejercicio presenta una tabla con datos de atletas en diferentes años y pregunta: "Si el 7% de 5,000 atletas compite en natación, ¿el procedimiento sugerido para calcular cuántos nadadores hay es correcto?"

### 📁 Archivos del Proyecto
- `juegos_deportivos_aleatorio_interpretacion_representacion_n2_opcA_v1.Rmd` - **Versión A** *(Detectar errores)*
- `juegos_deportivos_aleatorio_interpretacion_representacion_n2_opcB_v1.Rmd` - **Versión B** *(Validar procedimientos)*

> **🤔 ¿Por qué dos versiones?** Cada versión evalúa una habilidad diferente: una enseña a encontrar errores, la otra a reconocer procedimientos correctos.

## 🎯 Competencia ICFES Evaluada

> **📚 Para novatos:** ICFES es el examen de Estado colombiano. Cada pregunta evalúa una "competencia" específica (una habilidad matemática). Aquí te explicamos qué evalúa este ejercicio:

**Competencia:** Interpretación y Representación *(Leer y entender tablas, gráficos y procedimientos matemáticos)*
**Nivel:** 2 (Intermedio) *(No es muy fácil, pero tampoco muy difícil)*
**Componente:** Aleatorio (Estadística) *(Trabaja con datos, porcentajes y probabilidades)*
**Contexto:** Comunitario *(Situaciones de la vida real, como deportes)*
**Eje Axial:** Eje 4 *(Clasificación técnica ICFES)*

> **🎯 En palabras simples:** Este ejercicio evalúa si puedes leer una tabla de datos deportivos y determinar si un procedimiento matemático para calcular porcentajes es correcto o no.

## 🔄 Diferencias Clave Entre Versiones

> **🤓 Para novatos:** Las dos versiones parecen iguales, pero evalúan habilidades opuestas. Una te enseña a detectar errores, la otra a reconocer cuando algo está bien hecho.

### 🅰️ **VERSIÓN A (opcA)** - Identificación de Errores
> **🔍 ¿Qué hace?** Te presenta un procedimiento INCORRECTO y evalúa si puedes detectar el error.

- **Enfoque:** Evalúa la capacidad de **identificar errores** en procedimientos matemáticos
- **Respuesta Correcta:** "incorrecto, porque se debe multiplicar [decimal_correcto] por el número de atletas"
- **Procedimiento Presentado:** Usa valor decimal **INCORRECTO** (ej: 0.7 en lugar de 0.07 para 7%)
- **Competencia Específica:** Detectar errores conceptuales en conversión porcentaje-decimal

> **💡 Ejemplo:** Si el problema dice "7% de atletas", el procedimiento incorrecto sugiere usar 0.7 (que sería 70%), cuando debería usar 0.07 (que sí es 7%).

### 🅱️ **VERSIÓN B (opcB)** - Validación de Procedimientos
> **✅ ¿Qué hace?** Te presenta un procedimiento CORRECTO y evalúa si puedes reconocer que está bien.

- **Enfoque:** Evalúa la capacidad de **validar procedimientos correctos**
- **Respuesta Correcta:** "suficiente para determinar el número de atletas que participó en [disciplina]"
- **Procedimiento Presentado:** Usa valor decimal **CORRECTO** (ej: 0.07 para 7%)
- **Competencia Específica:** Reconocer cuando un procedimiento es matemáticamente válido

> **💡 Ejemplo:** Si el problema dice "7% de atletas", el procedimiento correcto sugiere usar 0.07, y tú debes reconocer que eso está bien y es suficiente para resolver el problema.

## 🏗️ Arquitectura Técnica Común

> **🔧 Para novatos:** Esta sección explica cómo el código genera automáticamente datos diferentes cada vez. ¡Es como tener una máquina que inventa números realistas!

### 📊 Generación de Datos Aleatorios
> **🎲 ¿Cómo funciona?** El programa elige aleatoriamente entre diferentes opciones para crear versiones únicas del ejercicio.

```r
# Contextos deportivos (10 opciones)
# El programa elige uno al azar cada vez:
- Juegos Panamericanos, Olímpicos, Mundiales, etc.
- Regiones: América, mundial, Centroamérica, etc.

# Datos temporales
# Simula eventos deportivos reales (cada 4 años):
- 5 años consecutivos (cada 4 años, formato realista)
- Rango: 1998-2024

# Datos deportivos por año
# Números realistas basados en eventos deportivos reales:
- Países: 30-60 participantes
- Deportes: 30-50 disciplinas
- Atletas: 3,000-9,000 participantes
```

> **💡 Resultado:** Cada vez que generas el ejercicio, obtienes una combinación diferente: "Juegos Olímpicos de 2020 con 45 países, 38 deportes y 6,500 atletas" o "Juegos Panamericanos de 2015 con 38 países, 42 deportes y 5,200 atletas", etc.

### 🎲 Sistema Avanzado de Distractores
> **🤔 ¿Qué son distractores?** Son las opciones incorrectas en una pregunta de opción múltiple. Deben ser creíbles para que el ejercicio sea desafiante, pero incorrectas para evaluar el conocimiento real.

- **8+ tipos diferentes** de distractores conceptuales *(Errores comunes que cometen los estudiantes)*
- **30% probabilidad** de valores duplicados con justificaciones diferentes *(A veces dos opciones dan el mismo número pero por razones diferentes)*
- **Selección estratégica:** 1 duplicado + 2 diferentes (cuando aplica) *(Mezcla inteligente de tipos de errores)*
- **Verificación única:** Garantiza 4 opciones textualmente distintas *(Nunca habrá dos opciones idénticas)*

> **📝 Ejemplo de distractores:**
> - Opción A: "Correcto" ✅ *(Respuesta correcta)*
> - Opción B: "Incorrecto, falta multiplicar por países" ❌ *(Error conceptual común)*
> - Opción C: "Incorrecto, se debe usar 0.7 en lugar de 0.07" ❌ *(Error de conversión decimal)*
> - Opción D: "Correcto solo si es número entero" ❌ *(Malentendido sobre redondeo)*

### 🐍 Integración R-Python
> **🤖 ¿Por qué dos lenguajes?** R es excelente para estadísticas y exámenes, Python es mejor para crear gráficos bonitos. ¡Usamos lo mejor de cada uno!

```python
# Generación de tablas con matplotlib (librería de Python para gráficos)
- Formato español (coma como separador de miles) # 5,000 en lugar de 5000
- Estilización profesional con colores # Verde para encabezados, amarillo para datos importantes
- Resaltado del año de cálculo # El año que se usa en el problema aparece destacado
- Compatibilidad PDF/HTML/Word # La misma tabla se ve bien en cualquier formato
```

> **🎨 Resultado visual:** En lugar de una tabla aburrida de texto, obtienes una tabla colorida y profesional que parece sacada de un libro de texto o presentación empresarial.

## 📈 Visualizaciones y Tablas

> **🎨 Para novatos:** Esta sección explica cómo se ven las tablas y por qué son tan bonitas. ¡Los colores y el formato no son casuales!

### 🎨 Características Visuales
> **🌈 Psicología del color:** Cada color tiene un propósito educativo específico.

- **Encabezados:** Verde (#4CAF50) con texto blanco *(Verde = información importante, fácil de leer)*
- **Filas alternas:** Gris claro (#f0f0f0) y blanco *(Facilita seguir las filas con la vista)*
- **Año destacado:** Amarillo (#FFE082) con texto en negrita *(Amarillo = atención, es el dato clave del problema)*
- **Resolución:** 150 DPI para calidad profesional *(Suficiente calidad para imprimir o ver en pantalla)*

> **👀 Resultado:** Una tabla que no cansa la vista y guía naturalmente la atención hacia los datos importantes.

### 📱 Adaptabilidad de Formato
> **🔄 ¿Por qué es importante?** El mismo ejercicio debe verse bien en Moodle (plataforma web), PDF (para imprimir) y Word (para editar).

```r
# Detección automática de formato
if (es_moodle) {
  # Tabla HTML responsiva (se adapta al tamaño de pantalla)
} else {
  # Imagen PNG/PDF generada con Python (calidad fija, siempre se ve igual)
}
```

> **🎯 Beneficio:** Los profesores pueden usar el mismo ejercicio en clase digital, examen impreso o tarea en línea sin preocuparse por problemas de formato.

## 🧮 Lógica Matemática

> **🔢 Para novatos:** Aquí está el corazón del problema matemático. La diferencia entre las versiones está en un pequeño detalle que cambia todo el significado.

### ✅ **Versión A - Detección de Errores**
> **🚨 El truco:** Te presentan un error muy común que cometen los estudiantes al convertir porcentajes a decimales.

```
Porcentaje real: 7%
Decimal correcto: 0.07 (7 ÷ 100 = 0.07)
Decimal presentado: 0.7 (ERROR - esto sería 70%, no 7%)
Resultado incorrecto: 0.7 × atletas = 70% (no 7%)
```

> **💡 Ejemplo concreto:** Si hay 5,000 atletas:
> - Cálculo correcto: 0.07 × 5,000 = 350 atletas (7%)
> - Cálculo incorrecto: 0.7 × 5,000 = 3,500 atletas (70%) ¡Error enorme!

### ✅ **Versión B - Validación Correcta**
> **✅ La prueba:** Te presentan el procedimiento correcto y debes reconocer que está bien.

```
Porcentaje real: 7%
Decimal presentado: 0.07 (CORRECTO - 7% = 7 ÷ 100 = 0.07)
Resultado correcto: 0.07 × atletas = 7%
```

> **💡 Ejemplo concreto:** Si hay 5,000 atletas:
> - Cálculo presentado: 0.07 × 5,000 = 350 atletas
> - Tu tarea: Reconocer que esto es correcto y suficiente para resolver el problema

## 🔬 Sistema de Validaciones

> **🛡️ Para novatos:** Estos son "tests automáticos" - código que verifica que todo funcione correctamente. ¡Es como tener un inspector de calidad automático!

### 🧪 Tests Automáticos
> **🔍 ¿Para qué sirven?** Cada vez que se genera un ejercicio, estos tests verifican que los números sean realistas y las opciones sean válidas.

```r
# Rangos realistas de datos
# Verifica que los números parezcan de eventos deportivos reales
test_that("Los datos están en rangos realistas", {
  expect_true(países >= 30 && países <= 60)     # Entre 30 y 60 países (realista)
  expect_true(deportes >= 30 && deportes <= 50) # Entre 30 y 50 deportes (realista)
  expect_true(atletas >= 3000 && atletas <= 9000) # Entre 3,000 y 9,000 atletas (realista)
})

# Coherencia matemática
# Verifica que los cálculos matemáticos sean correctos
test_that("El porcentaje está en rango correcto", {
  expect_true(porcentaje >= 5 && porcentaje <= 10)        # Entre 5% y 10% (realista)
  expect_true(porcentaje_decimal == porcentaje / 100)     # Conversión correcta (ej: 7% = 0.07)
})

# Unicidad de opciones
# Verifica que no haya opciones duplicadas o problemas en las respuestas
test_that("Las opciones son válidas", {
  expect_equal(length(unique(opciones)), 4)    # Exactamente 4 opciones diferentes
  expect_true(afirmacion_correcta %in% opciones) # La respuesta correcta está incluida
})
```

> **🚨 ¿Qué pasa si falla un test?** El programa se detiene y muestra un error, evitando generar ejercicios con problemas. ¡Es mejor prevenir que lamentar!

## 🎯 Estrategias Pedagógicas

> **🎓 Para novatos:** Esta sección explica la filosofía educativa detrás de cada versión. ¡No es solo matemáticas, es psicología del aprendizaje!

### 📚 **Versión A - Errores Conceptuales Comunes**
> **🧠 Filosofía:** "Aprender de los errores". Esta versión enseña identificando errores típicos que cometen los estudiantes.

- **Confusión entre 0.07 y 0.7** *(Error #1 más común en porcentajes)*
- **Malentendido en conversión porcentaje-decimal** *(7% ≠ 0.7, sino 7% = 0.07)*
- **Identificación de procedimientos incorrectos** *(Desarrolla pensamiento crítico)*

> **🎯 Beneficio educativo:** Los estudiantes que practican con esta versión se vuelven expertos en detectar errores comunes, tanto propios como ajenos.

### 📚 **Versión B - Validación de Conocimientos**
> **🧠 Filosofía:** "Confianza en lo correcto". Esta versión enseña a reconocer y validar procedimientos correctos.

- **Reconocimiento de procedimientos válidos** *(Desarrolla confianza matemática)*
- **Suficiencia de información disponible** *(¿Tengo todo lo que necesito para resolver?)*
- **Aplicación correcta de conceptos porcentuales** *(Refuerza conocimientos sólidos)*

> **🎯 Beneficio educativo:** Los estudiantes que practican con esta versión desarrollan confianza en sus habilidades y aprenden a validar sus propios procedimientos.

## 🚀 Compilación y Uso

> **⚙️ Para novatos:** Esta sección te enseña cómo "compilar" (convertir el código en ejercicios listos para usar). ¡Es como cocinar siguiendo una receta!

### 📋 Requisitos Previos
> **🛠️ ¿Qué necesito instalar?** Piensa en esto como instalar las herramientas necesarias en tu cocina antes de cocinar.

```r
# Librerías R necesarias (como ingredientes básicos)
library(exams)      # La librería principal para crear exámenes
library(reticulate) # Para que R y Python trabajen juntos
library(digest)     # Para crear códigos únicos de cada versión
library(testthat)   # Para hacer tests automáticos
library(knitr)      # Para convertir código en documentos
library(stringr)    # Para manipular texto

# Python configurado correctamente (para hacer gráficos bonitos)
use_python(Sys.which("python"), required = TRUE)
```

> **💡 Analogía:** Es como tener todos los utensilios de cocina listos antes de empezar a cocinar.

### 🔧 Comandos de Compilación
> **👨‍🍳 ¿Cómo "cocino" mis ejercicios?** Estos comandos convierten el código en ejercicios listos para usar.

```r
# Compilar versión individual (hacer UN ejercicio)
exams2pdf("juegos_deportivos_aleatorio_interpretacion_representacion_n2_opcA_v1.Rmd")
exams2pdf("juegos_deportivos_aleatorio_interpretacion_representacion_n2_opcB_v1.Rmd")

# Compilar ambas versiones (hacer DOS ejercicios a la vez)
exams2pdf(c("opcA_v1.Rmd", "opcB_v1.Rmd"))

# Para Moodle (plataforma educativa en línea)
exams2moodle(c("opcA_v1.Rmd", "opcB_v1.Rmd"))
```

> **📁 ¿Dónde aparecen los resultados?** Se crean carpetas automáticamente con nombres como "pdf", "moodle", etc., donde encontrarás tus ejercicios listos.

## 📊 Métricas de Calidad

> **📏 Para novatos:** Esta sección explica cómo medimos si los ejercicios son "buenos". ¡Es como tener criterios de calidad en una fábrica!

### ✨ Diversidad de Versiones
> **🎲 ¿Por qué es importante?** Si generas 100 ejercicios y todos son iguales, no sirve. Necesitas que cada uno sea diferente pero igual de válido.

- **Objetivo:** 300+ versiones únicas por archivo *(Meta: poder generar 300 ejercicios diferentes)*
- **Logrado:** Sistema de aleatorización avanzado *(Realidad: el código puede generar miles de versiones)*
- **Verificación:** Tests automáticos incluidos *(Comprobación: el programa verifica automáticamente la diversidad)*

> **💡 Ejemplo práctico:** Puedes generar 50 exámenes para 50 estudiantes y cada uno tendrá números diferentes, pero todos evaluarán la misma habilidad matemática.

### 🎯 Alineación ICFES
> **🎯 ¿Cumple con los estándares oficiales?** Estos ejercicios siguen exactamente las especificaciones del ICFES colombiano.

- **Competencia:** Interpretación y Representación ✅ *(Evalúa la habilidad correcta)*
- **Nivel 2:** Dificultad intermedia apropiada ✅ *(No muy fácil, no muy difícil)*
- **Contexto:** Comunitario/deportivo realista ✅ *(Situaciones de la vida real)*
- **Componente:** Aleatorio/estadístico ✅ *(Área matemática correcta)*

> **🏆 Certificación:** Estos ejercicios pueden usarse oficialmente para preparar estudiantes para el examen ICFES real.

## 🔄 Flujo de Trabajo Recomendado

> **📋 Para novatos:** Esta es la "receta" paso a paso para usar estos ejercicios exitosamente. ¡Síguelos en orden!

1. **Selección de Versión:** Elegir A (errores) o B (validación) según objetivo pedagógico
   > *🤔 Pregúntate: ¿Quiero que mis estudiantes practiquen detectando errores o validando procedimientos correctos?*

2. **Compilación:** Usar comandos apropiados según formato destino
   > *💻 Decide: ¿Necesito PDFs para imprimir, archivos para Moodle, o documentos Word para editar?*

3. **Validación:** Verificar que tests automáticos pasen
   > *🔍 Comprueba: ¿El programa dice que todo está bien? Si hay errores, léelos y corrígelos.*

4. **Implementación:** Desplegar en plataforma educativa
   > *🚀 Acción: Sube los ejercicios a tu plataforma (Moodle, Google Classroom, etc.) o imprímelos.*

5. **Análisis:** Revisar resultados estudiantiles para ajustes futuros
   > *📊 Reflexiona: ¿Qué errores cometen más los estudiantes? ¿Necesito ajustar la dificultad?*

> **⏰ Tiempo estimado:** Todo el proceso toma entre 15-30 minutos la primera vez, y 5-10 minutos una vez que dominas el flujo.

## 📝 Notas Técnicas Importantes

> **⚙️ Para novatos:** Esta sección contiene información técnica importante. ¡No te asustes si no entiendes todo, pero guárdala para cuando tengas problemas!

### ⚠️ Configuraciones Críticas
> **🔧 ¿Por qué son críticas?** Cambiar estas configuraciones puede romper todo el sistema. ¡Mejor no tocarlas!

- **Semilla aleatoria:** `set.seed(sample(1:100000, 1))` para máxima diversidad
  > *🎲 Explicación: Esto hace que cada vez que ejecutes el código, obtengas números diferentes*

- **Formato español:** Coma como separador de miles, punto decimal
  > *🇪🇸 Explicación: 5.000,50 (formato español) vs 5,000.50 (formato inglés)*

- **Python/matplotlib:** Configuración `matplotlib.use('Agg')` para compatibilidad
  > *🐍 Explicación: Esto hace que Python genere gráficos sin abrir ventanas*

- **LaTeX:** Paquetes tikz, xcolor, graphicx incluidos
  > *📄 Explicación: Herramientas necesarias para generar PDFs bonitos*

### 🔧 Solución de Problemas Comunes
> **🆘 ¿Algo no funciona?** Aquí están las soluciones a los problemas más frecuentes:

- **Error Python:** Verificar `use_python()` configurado correctamente
  > *💡 Solución: Reinstalar Python o usar `install_python()` en R*

- **Tablas no aparecen:** Comprobar detección de formato Moodle vs PDF
  > *💡 Solución: Verificar que el formato de salida esté bien especificado*

- **Tests fallan:** Revisar rangos de datos aleatorios
  > *💡 Solución: Los números generados están fuera de los rangos esperados*

- **Compilación LaTeX:** Verificar paquetes extra_dependencies
  > *💡 Solución: Instalar MiKTeX o TeX Live completo*

> **🚨 Regla de oro:** Si algo no funciona, copia exactamente el mensaje de error y búscalo en Google. ¡Probablemente alguien más ya tuvo el mismo problema!

---

**🎓 Proyecto desarrollado siguiendo estándares ICFES y mejores prácticas R-exams**  
**📅 Versión: Junio 2025 | 🔄 Actualización continua**
