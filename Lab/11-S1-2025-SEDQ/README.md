# Ejercicio: Consumo de Gas Natural - Porcentaje del Máximo

## Descripción

Este ejercicio evalúa la competencia de **interpretación y representación** en el componente **aleatorio y sistemas de datos**. Los estudiantes deben interpretar un gráfico de barras que muestra el consumo mensual de gas natural y calcular qué porcentaje representa el consumo de junio respecto al consumo máximo posible.

## Información del Ejercicio

- **Archivo**: `consumo_gas_natural_porcentaje_maximo_aleatorio_interpretacion_representacion_n2_v1.Rmd`
- **Competencia ICFES**: Interpretación y representación
- **Componente**: Aleatorio y sistemas de datos
- **Nivel de dificultad**: 2 (Medio)
- **Tiempo estimado**: 3-4 minutos
- **Tipo de pregunta**: Selección múltiple con única respuesta

## Estructura del Problema

### Contexto
- Dos personas (un nombre masculino y uno femenino, orden aleatorio) comparten gastos de gas natural en su vivienda
- Se presenta información coherente sobre la factura de octubre (monto calculado basado en el consumo real mostrado en la gráfica)
- Un gráfico de barras muestra el consumo histórico mensual en metros cúbicos

### Pregunta Central
¿A qué porcentaje del consumo máximo posible corresponde el consumo de junio?

### Elementos Aleatorizados
1. **Nombres de los personajes**:
   - 10 nombres masculinos: Pedro, Carlos, Miguel, Antonio, Diego, Sebastián, Andrés, Luis, José, Francisco
   - 10 nombres femeninos: María, Ana, Carmen, Laura, Sofía, Valentina, Isabella, Camila, Daniela, Alejandra
   - Orden aleatorio de mención (100 combinaciones posibles)
2. **Tipo de vivienda**: apartamento, casa, hogar, residencia
3. **Consumo máximo**: 18, 20, 22, o 25 metros cúbicos
4. **Porcentaje objetivo**: 60%, 65%, 70%, 75%, 80%, 85%, o 90%
5. **Datos económicos coherentes**:
   - Cargo fijo: $2,500 a $3,200
   - Precio por m³: $800 a $1,200
   - Total factura = cargo fijo + (consumo octubre × precio por m³)
6. **Consumos mensuales**: valores realistas con variación aleatoria

## Características Técnicas

### Generación de Gráficos
- **Tecnología**: Python con matplotlib (siguiendo ejemplos funcionales)
- **Formato de salida**: PNG y PDF para compatibilidad total
- **Características visuales**:
  - Barras verdes con junio destacado en verde más oscuro
  - Valores mostrados sobre cada barra
  - Grilla horizontal para facilitar lectura
  - Rotación de etiquetas de meses para mejor legibilidad

### Validaciones Integradas
- Coherencia matemática de todos los cálculos
- Unicidad de las 4 opciones de respuesta
- Rangos apropiados para todos los valores generados
- Verificación de existencia de archivos gráficos

### Distractores Inteligentes
1. **Error conceptual**: Usar el valor absoluto como porcentaje
2. **Error de lectura**: Usar el consumo de otro mes
3. **Error matemático**: Aplicar incorrectamente la fórmula de porcentaje

## Uso

### Generación Individual
```r
library(exams)
library(reticulate)

# Generar una versión HTML
exams2html("consumo_gas_natural_porcentaje_maximo_aleatorio_interpretacion_representacion_n2_v1.Rmd", n=1)

# Generar una versión PDF
exams2pdf("consumo_gas_natural_porcentaje_maximo_aleatorio_interpretacion_representacion_n2_v1.Rmd", n=1)
```

### Generación Múltiple
```r
# Generar 10 versiones para Moodle
exams2moodle("consumo_gas_natural_porcentaje_maximo_aleatorio_interpretacion_representacion_n2_v1.Rmd", n=10)

# Generar 20 versiones para examen impreso
exams2nops("consumo_gas_natural_porcentaje_maximo_aleatorio_interpretacion_representacion_n2_v1.Rmd", n=20)
```

### Pruebas de Validación
```r
# Ejecutar script de prueba
source("test_consumo_gas_natural.R")
```

## Competencias Evaluadas

### Interpretación y Representación
- **Lectura de gráficos**: Extraer información específica de gráficos de barras
- **Comprensión de escalas**: Interpretar valores numéricos en contexto
- **Identificación de datos clave**: Localizar información relevante para el cálculo

### Razonamiento Cuantitativo
- **Cálculo de porcentajes**: Aplicar la fórmula (parte/total) × 100%
- **Proporcionalidad**: Entender la relación entre consumo real y máximo posible
- **Verificación de resultados**: Evaluar la coherencia de la respuesta

## Garantías de Calidad

### Diversidad de Versiones
- Más de 1,000 versiones únicas posibles
- 100 combinaciones de nombres (10 masculinos × 10 femeninos × 2 órdenes)
- Combinaciones aleatorias de todos los parámetros
- Mantenimiento de coherencia matemática en todas las versiones

### Compatibilidad Técnica
- ✅ HTML (navegadores web)
- ✅ PDF (documentos impresos)
- ✅ Word (documentos editables)
- ✅ Moodle (plataformas LMS)
- ✅ NOPS (exámenes escaneables)

### Validación Automática
- Pruebas unitarias integradas
- Verificación de coherencia matemática
- Validación de coherencia económica (factura = cargo fijo + consumo × precio)
- Validación de generación de gráficos
- Comprobación de unicidad de opciones

## Archivos Relacionados

- `consumo_gas_natural_porcentaje_maximo_aleatorio_interpretacion_representacion_n2_v1.Rmd`: Archivo principal del ejercicio
- `test_consumo_gas_natural.R`: Script de pruebas y validación
- `demo_nombres_mixtos.R`: Demostración de diversidad de nombres
- `demo_coherencia_factura.R`: Demostración de coherencia económica
- `SemilleroUnico_v2.R`: Script de configuración para generación masiva
- `README.md`: Este archivo de documentación

## Notas Técnicas

### Dependencias
- R packages: `exams`, `reticulate`, `knitr`, `testthat`
- Python: `matplotlib`, `numpy`
- LaTeX: Para generación de PDF (opcional)

### Resolución de Problemas
- Si los gráficos no se visualizan, verificar instalación de Python y matplotlib
- Para problemas de codificación, asegurar configuración UTF-8
- En caso de errores de LaTeX, verificar instalación de TinyTeX o MiKTeX

---

**Autor**: Sistema R-Exams ICFES  
**Versión**: 1.0  
**Fecha**: Enero 2025  
**Licencia**: Uso académico e institucional
