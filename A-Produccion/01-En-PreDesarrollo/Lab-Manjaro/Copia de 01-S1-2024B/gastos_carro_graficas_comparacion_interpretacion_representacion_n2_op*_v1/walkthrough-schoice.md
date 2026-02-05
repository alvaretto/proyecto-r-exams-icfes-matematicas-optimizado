# 🚀 Walkthrough Detallado - Ejercicio de Gráficas con Python y R

## 📚 **Guía Paso a Paso para Principiantes**

Este documento explica **línea por línea** cómo funciona el ejercicio de gráficas de gastos de carro, perfecto para quienes están aprendiendo R-exams con Python.

---

## 🎯 **¿Qué vamos a crear?**

Un ejercicio de selección múltiple que:

- Genera datos aleatorios de gastos de carro
- Crea 4 gráficas diferentes con Python
- Pregunta cuál gráfica permite comparar totales por semana
- Funciona en HTML, PDF y DOCX

---

## 📋 **Estructura del Archivo .Rmd**

### **1. Encabezado YAML**
```yaml
Question
========
```
**¿Qué hace?** Define que esto es una pregunta de R-exams.

### **2. Configuración Inicial**
```r
```{r configuracion_inicial, echo=FALSE, message=FALSE, warning=FALSE}
# Cargar librerías necesarias
library(exams)      # Para crear exámenes
library(reticulate) # Para usar Python desde R
library(knitr)      # Para procesar documentos

# Configurar Python
use_python('/usr/bin/python3')

# Configurar opciones para evitar notación científica
options(scipen = 999)        # Evita 1e+05, muestra 100000
options(digits = 10)         # Más decimales
Sys.setlocale(category='LC_NUMERIC', locale='C')  # Formato numérico estándar
```

**¿Qué hace cada línea?**

- `library(exams)`: Carga el paquete para crear exámenes
- `library(reticulate)`: Permite usar Python dentro de R
- `use_python()`: Le dice a R qué versión de Python usar
- `options(scipen = 999)`: Evita números como 1.23e+05, los muestra como 123000
- `Sys.setlocale()`: Configura el formato de números (punto decimal, no coma)

### **3. Detección de Formato de Salida**
```r
# Detectar formato de salida para gráficas
typ <- match_exams_device()
es_pdf <- (typ == "pdf")

# Detectar formatos que requieren archivos PNG
formatos_png <- c("pdf", "pandoc", "docx", "odt")
usar_png <- (typ %in% formatos_png)

# Detectar formatos HTML/Moodle que usan plt.show()
formatos_moodle <- c("exams2moodle", "exams2qti12", "exams2qti21", "exams2openolat")
es_html_moodle <- (match_exams_call() %in% formatos_moodle) || (typ == "html")
```

**¿Qué hace?**

- `match_exams_device()`: Detecta si estamos generando HTML, PDF, etc.
- `usar_png`: Variable que dice si necesitamos generar archivos PNG
- `es_html_moodle`: Variable que dice si es para web/Moodle

**¿Por qué es importante?**

- HTML puede mostrar gráficas directamente
- PDF necesita archivos PNG guardados
- Cada formato tiene sus propias reglas

### **4. Generación de Datos Aleatorios**
```r
```{r generar_datos_aleatorios, echo=FALSE}
# Generar datos aleatorios para 4 semanas
set.seed(sample(1:1000, 1))  # Semilla aleatoria para datos únicos

# Crear datos de gastos por semana (en pesos colombianos)
datos <- list()
datos$gastos_semanas <- list()

for(i in 1:4) {
  datos$gastos_semanas[[i]] <- list(
    gasolina = sample(20000:50000, 1),      # Entre $20,000 y $50,000
    parqueadero = sample(15000:30000, 1),   # Entre $15,000 y $30,000
    peajes = sample(10000:25000, 1)         # Entre $10,000 y $25,000
  )
}
```

**¿Qué hace cada línea?**

- `set.seed(sample(1:1000, 1))`: Genera una semilla aleatoria diferente cada vez
- `datos <- list()`: Crea una lista vacía para guardar todos los datos
- `for(i in 1:4)`: Repite 4 veces (una por cada semana)
- `sample(20000:50000, 1)`: Escoge un número aleatorio entre 20,000 y 50,000

**¿Por qué estos rangos?**

- Son valores realistas para gastos de carro en Colombia
- Permiten variación suficiente para hacer el ejercicio interesante
- Los totales quedan en rangos manejables ($50,000 - $100,000 por semana)

### **5. Cálculos Automáticos**
```r
# Calcular totales por categoría
datos$totales_categoria <- list(
  gasolina = sum(sapply(datos$gastos_semanas, function(x) x$gasolina)),
  parqueadero = sum(sapply(datos$gastos_semanas, function(x) x$parqueadero)),
  peajes = sum(sapply(datos$gastos_semanas, function(x) x$peajes))
)

# Calcular totales por semana
datos$totales_semana <- sapply(datos$gastos_semanas, function(semana) {
  semana$gasolina + semana$parqueadero + semana$peajes
})
```

**¿Qué hace?**

- `sapply()`: Aplica una función a cada elemento de la lista
- `sum()`: Suma todos los valores
- `function(x) x$gasolina`: Función que extrae solo la gasolina de cada semana

**Ejemplo de resultado:**

- Total gasolina 4 semanas: $140,000
- Total semana 1: $89,000 (gasolina + parqueadero + peajes)

### **6. Generación de Gráficas con Python**
```python
```{python generar_graficas_archivos, echo=FALSE, results='hide'}
import matplotlib.pyplot as plt
import numpy as np
import os

# Configurar matplotlib para generar archivos
plt.ioff()  # Modo no interactivo (no muestra ventanas)
plt.rcParams['font.size'] = 10  # Tamaño de fuente base
```

**¿Qué hace cada línea?**

- `import matplotlib.pyplot as plt`: Importa la librería para hacer gráficas
- `import numpy as np`: Importa librería para cálculos matemáticos
- `plt.ioff()`: Desactiva el modo interactivo (no abre ventanas)
- `plt.rcParams['font.size'] = 10`: Establece el tamaño de letra por defecto

### **7. Obtener Datos de R en Python**
```python
# Obtener datos de R
gastos_semanas = r.datos['gastos_semanas']
totales_semana = r.datos['totales_semana']
porc_gasolina = r.porc_gasolina
porc_parqueadero = r.porc_parqueadero
porc_peajes = r.porc_peajes
porc_semanas = r.porc_semanas
```

**¿Qué hace?**

- `r.datos`: Accede a la variable `datos` que creamos en R
- `r.porc_gasolina`: Accede al porcentaje de gasolina calculado en R

**¿Cómo funciona?**

- `reticulate` permite que Python lea variables de R usando `r.nombre_variable`
- Los datos se transfieren automáticamente entre R y Python

### **8. Preparar Datos para Gráficas**
```python
# Preparar datos
categorias = ['Gasolina', 'Parqueadero', 'Peajes']
semanas = ['Semana 1', 'Semana 2', 'Semana 3', 'Semana 4']
gasolina_por_semana = [gastos_semanas[i]['gasolina'] for i in range(4)]
parqueadero_por_semana = [gastos_semanas[i]['parqueadero'] for i in range(4)]
peajes_por_semana = [gastos_semanas[i]['peajes'] for i in range(4)]
```

**¿Qué hace?**

- Crea listas con los nombres para las etiquetas
- `[gastos_semanas[i]['gasolina'] for i in range(4)]`: List comprehension que extrae la gasolina de cada semana

**Resultado ejemplo:**
- `gasolina_por_semana = [44000, 26000, 41000, 28000]`

### **9. Paleta de Colores Profesional**
```python
# Paletas de colores profesionales
colores_categorias = ['#2E5984', '#5B9BD5', '#A5A5A5']  # Azul oscuro, azul claro, gris
colores_semanas = ['#1F4E79', '#2E75B6', '#5B9BD5', '#8DB4E2']  # Gradiente azul
```

**¿Por qué estos colores?**

- Son colores corporativos profesionales
- Tienen buen contraste para lectura
- Son distinguibles para personas con daltonismo
- Se ven bien tanto en pantalla como impresos

### **10. Creación de Gráfica A - Circular por Categoría**
```python
# GRÁFICA A: Circular por categoría
fig_a, ax_a = plt.subplots(figsize=(7, 6))
porcentajes_cat = [porc_gasolina, porc_parqueadero, porc_peajes]
wedges, texts, autotexts = ax_a.pie(porcentajes_cat, labels=categorias, autopct='%1.1f%%',
                                   colors=colores_categorias, startangle=90,
                                   explode=(0.05, 0.05, 0.05), shadow=True,
                                   textprops={'fontsize': 10, 'fontweight': 'bold'})
```

**¿Qué hace cada parámetro?**

- `figsize=(7, 6)`: Tamaño de la figura en pulgadas (ancho x alto)
- `autopct='%1.1f%%'`: Formato de porcentajes (1 decimal)
- `startangle=90`: Empieza el gráfico desde arriba (90 grados)
- `explode=(0.05, 0.05, 0.05)`: Separa ligeramente cada sector
- `shadow=True`: Agrega sombra para efecto 3D
- `textprops`: Propiedades del texto (tamaño y peso)

**¿Por qué esta gráfica NO es la respuesta correcta?**

- Muestra **proporciones** (porcentajes) del total
- NO muestra los **valores absolutos** por semana
- NO permite comparar totales entre semanas

### **11. Personalización de Texto**
```python
for autotext in autotexts:
    autotext.set_color('white')      # Texto blanco
    autotext.set_fontweight('bold')  # Texto en negrita
    autotext.set_fontsize(11)        # Tamaño de fuente
```

**¿Qué hace?**

- `autotexts`: Lista con los textos de porcentajes
- `set_color('white')`: Cambia el color a blanco para mejor contraste
- `set_fontweight('bold')`: Hace el texto más grueso y visible

### **12. Guardar la Gráfica**
```python
ax_a.set_title('Gráfica circular por categoría', fontsize=12, fontweight='bold', pad=20)
plt.tight_layout()
plt.savefig('grafica_a.png', dpi=150, bbox_inches='tight', facecolor='white', edgecolor='none')
plt.savefig('grafica_a.pdf', dpi=150, bbox_inches='tight', facecolor='white', edgecolor='none')
plt.close()
```

**¿Qué hace cada parámetro?**

- `set_title()`: Agrega título a la gráfica
- `pad=20`: Espacio entre título y gráfica
- `tight_layout()`: Ajusta automáticamente los márgenes
- `dpi=150`: Resolución (puntos por pulgada) - calidad de impresión
- `bbox_inches='tight'`: Recorta espacios en blanco
- `facecolor='white'`: Fondo blanco
- `edgecolor='none'`: Sin borde
- `plt.close()`: Cierra la figura para liberar memoria

**¿Por qué guardar PNG y PDF?**

- PNG: Para HTML y documentos web
- PDF: Para documentos LaTeX y impresión vectorial

### **13. Gráfica B - Barras Apiladas (RESPUESTA CORRECTA)**
```python
# GRÁFICA B: Barras apiladas por semana
fig_b, ax_b = plt.subplots(figsize=(9, 6))
x = np.arange(len(semanas))  # Posiciones [0, 1, 2, 3]
width = 0.4                  # Ancho de las barras

p1 = ax_b.bar(x, gasolina_por_semana, width, label='Gasolina', color=colores_categorias[0])
p2 = ax_b.bar(x, parqueadero_por_semana, width, bottom=gasolina_por_semana,
              label='Parqueadero', color=colores_categorias[1])
p3 = ax_b.bar(x, peajes_por_semana, width,
              bottom=np.array(gasolina_por_semana) + np.array(parqueadero_por_semana),
              label='Peajes', color=colores_categorias[2])
```

**¿Qué hace cada línea?**

- `np.arange(len(semanas))`: Crea [0, 1, 2, 3] para posiciones en X
- `width = 0.4`: Barras ocupan 40% del espacio disponible
- `bottom=gasolina_por_semana`: La segunda barra empieza donde termina la primera
- `np.array()`: Convierte listas a arrays para poder sumarlos

**¿Por qué ES la respuesta correcta?**

- La **altura total** de cada barra = total gastado esa semana
- Permite **comparar visualmente** qué semana gastó más
- Muestra **valores absolutos**, no porcentajes

### **14. Agregar Totales Encima de las Barras**
```python
max_total = max(totales_semana)
for i, total in enumerate(totales_semana):
    ax_b.text(i, total + max_total * 0.02, f'${total:,}', ha='center', va='bottom',
              fontweight='bold', fontsize=10, color='#333333')
```

**¿Qué hace?**

- `max(totales_semana)`: Encuentra el total más alto
- `enumerate()`: Da tanto el índice (i) como el valor (total)
- `total + max_total * 0.02`: Posición Y = total + 2% del máximo (para separación)
- `f'${total:,}'`: Formato con símbolo $ y separadores de miles
- `ha='center'`: Alineación horizontal centrada
- `va='bottom'`: Alineación vertical desde abajo

### **15. Formato de Ejes y Leyenda Inteligente**
```python
ax_b.set_xlabel('Semanas', fontsize=11, fontweight='bold')
ax_b.set_ylabel('Gastos (pesos)', fontsize=11, fontweight='bold')
ax_b.set_xticks(x)
ax_b.set_xticklabels(semanas, fontsize=10)

# Posicionar leyenda de forma inteligente para evitar solapamientos
min_total = min(totales_semana)
if min_total > 70000:  # Si todas las barras son altas, leyenda abajo
    ax_b.legend(loc='lower right', frameon=True, fancybox=True, shadow=True, fontsize=10)
else:  # Si hay barras bajas, leyenda arriba a la derecha
    ax_b.legend(loc='upper right', frameon=True, fancybox=True, shadow=True, fontsize=10)
```

**¿Qué hace cada línea?**

- `set_xlabel()`: Etiqueta del eje X
- `set_xticks(x)`: Posiciones donde van las etiquetas [0, 1, 2, 3]
- `set_xticklabels(semanas)`: Textos de las etiquetas ['Semana 1', 'Semana 2', ...]
- `min(totales_semana)`: Encuentra el total más bajo para decidir posición
- **Posicionamiento inteligente**: Evita que la leyenda tape las barras
- `loc='lower right'`: Leyenda abajo si las barras son altas
- `loc='upper right'`: Leyenda arriba si hay barras bajas
- `fancybox=True`: Esquinas redondeadas
- `shadow=True`: Sombra en la leyenda

**¿Por qué es importante?**
- Las leyendas fijas pueden tapar datos importantes
- El posicionamiento dinámico mejora la legibilidad
- Se adapta automáticamente a diferentes rangos de datos

### **16. Formato de Números en Eje Y**
```python
ax_b.yaxis.set_major_formatter(plt.FuncFormatter(lambda x, p: f'${x:,.0f}'))
ax_b.tick_params(axis='y', labelsize=9)
ax_b.grid(True, alpha=0.3, linestyle='--', linewidth=0.5)
ax_b.set_axisbelow(True)
```

**¿Qué hace?**

- `FuncFormatter()`: Función personalizada para formatear números
- `lambda x, p: f'${x:,.0f}'`: Función que agrega $ y separadores de miles
- `tick_params()`: Cambia el tamaño de las etiquetas del eje
- `grid()`: Agrega líneas de cuadrícula
- `alpha=0.3`: Transparencia de la cuadrícula (30% visible)
- `linestyle='--'`: Líneas punteadas
- `set_axisbelow(True)`: Cuadrícula detrás de las barras

### **17. Inclusión de Gráficas en el Documento**
```markdown
- ![](grafica_a.png){width=70%}
- ![](grafica_b.png){width=80%}
- ![](grafica_c.png){width=70%}
- ![](grafica_d.png){width=90%}
```

**¿Por qué esta sintaxis?**

- `![](archivo.png)`: Sintaxis Markdown estándar para imágenes
- `{width=70%}`: Atributo Pandoc para controlar el tamaño
- Funciona en HTML, PDF y DOCX automáticamente

### **18. Sección de Respuestas**
```r
Answerlist
----------
* Gráfica circular por categoría
* Gráfica de barras apiladas por semana
* Gráfica circular por semana
* Gráfica de barras agrupadas por categoría
```

**¿Qué hace?**

- `Answerlist`: Palabra clave de R-exams para las opciones
- `----------`: Separador requerido
- `*`: Cada opción empieza con asterisco

### **19. Metainformación**
```r
Meta-information
================
extype: schoice
exsolution: 0100
exname: gastos_carro_graficas_comparacion
```

**¿Qué significa?**

- `extype: schoice`: Single choice (selección única)
- `exsolution: 0100`: La respuesta correcta es la segunda (B)
- `exname`: Nombre interno del ejercicio

---

## 🎯 **Conceptos Clave para Principiantes**

### **1. ¿Por qué usar Python con R-exams?**

- **R**: Excelente para estadística y generación de exámenes
- **Python**: Mejor para gráficas complejas y visualización
- **Combinación**: Lo mejor de ambos mundos

### **2. ¿Cómo funciona la aleatorización?**

- Cada vez que generas el examen, los números cambian
- Pero la lógica y la respuesta correcta se mantienen
- Permite crear múltiples versiones del mismo ejercicio

### **3. ¿Por qué generar PNG y PDF?**

- **HTML**: Muestra PNG directamente
- **PDF**: Necesita archivos de imagen guardados
- **DOCX**: Compatible con PNG

### **4. ¿Cómo elegir la respuesta correcta?**

- Lee cuidadosamente qué pregunta el ejercicio
- "Comparar totales por semana" → necesitas ver alturas/valores absolutos
- Las gráficas circulares muestran proporciones, no totales

### **5. ¿Cómo evitar problemas de solapamiento de leyendas?**
- **Problema común**: Las leyendas fijas pueden tapar barras o datos
- **Solución inteligente**: Posicionamiento dinámico basado en los datos
- **Para barras apiladas**: Leyenda abajo si barras altas, arriba si barras bajas
- **Para barras agrupadas**: Leyenda fuera del área de gráfica con `bbox_to_anchor`
- **Beneficio**: Gráficas siempre legibles sin importar los datos aleatorios

---

## 🚀 **Próximos Pasos**

1. **Experimenta** cambiando los rangos de datos aleatorios
2. **Modifica** los colores y estilos de las gráficas
3. **Agrega** más categorías de gastos
4. **Crea** tu propio ejercicio siguiendo este patrón

---

## 📚 **Recursos Adicionales**

- [Documentación de R-exams](http://www.r-exams.org/)
- [Matplotlib Gallery](https://matplotlib.org/stable/gallery/)
- [Reticulate Documentation](https://rstudio.github.io/reticulate/)
- [Código GitHub](https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado/blob/experimentos-seguros/Lab-Manjaro/01-S1-2024B/gastos_carro_graficas_comparacion_interpretacion_representacion_n2_v1.Rmd/)

**¡Felicidades!** Ahora entiendes cómo crear ejercicios profesionales con R-exams y Python. 🎉
