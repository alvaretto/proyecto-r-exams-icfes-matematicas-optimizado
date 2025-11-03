# 📋 EXAMEN DE FIN DE PERÍODO 4

## 📖 Descripción

Este directorio contiene el sistema automatizado para generar el **Examen de Fin de Período 4**, que selecciona aleatoriamente 15 ejercicios de un banco de 178 ejercicios matemáticos en formato R-exams (.Rmd).

## 🎯 Características Principales

- ✅ **Selección automática**: Detecta todos los archivos .Rmd disponibles en el directorio
- ✅ **Selección aleatoria**: Elige exactamente 15 ejercicios sin repetición
- ✅ **Manejo robusto de errores**: Continúa la generación aunque algunos archivos fallen
- ✅ **Múltiples formatos de salida**: DOCX y PDF, con y sin soluciones
- ✅ **Ejercicios ordenados por recencia**: Los archivos están numerados del 001 al 178, donde 001 es el más reciente

## 📁 Estructura del Directorio

```
04-Examen-Fin-de-Periodo-4/
├── SemilleroFinDePeriodo_v4.R    # Script principal de generación
├── SemilleroMoodle_v2.R          # Script para exportación a Moodle (heredado)
├── SemilleroUnico_v2.R           # Script unificado (heredado)
├── pcielo.tex                    # Template LaTeX con soluciones
├── pcielo_nosol.tex              # Template LaTeX sin soluciones
├── solpcielo.tex                 # Template LaTeX solo soluciones
├── exam.tex                      # Template LaTeX para examen
├── README.md                     # Este archivo
├── 001-[nombre].Rmd              # Ejercicio más reciente
├── 002-[nombre].Rmd              # Segundo más reciente
├── ...
├── 178-[nombre].Rmd              # Ejercicio más antiguo
├── ejercicios/                   # Directorio para ejercicios generados
└── salida/                       # Directorio para archivos de salida
```

## 🚀 Uso del Sistema

### Requisitos Previos

1. **R** instalado (versión 4.0 o superior)
2. **RStudio** (recomendado)
3. Paquete **exams** instalado:
   ```r
   install.packages("exams")
   ```
4. **LaTeX** instalado (para generación de PDF):
   - En Linux: `sudo apt-get install texlive-full`
   - En Windows: MiKTeX o TeX Live
   - En macOS: MacTeX

### Ejecución del Script

#### Opción 1: Desde RStudio

1. Abrir RStudio
2. Establecer el directorio de trabajo:
   ```r
   setwd("Auxiliares/Examenes_Fin_de_Periodo/04-Examen-Fin-de-Periodo-4")
   ```
3. Ejecutar el script:
   ```r
   source("SemilleroFinDePeriodo_v4.R")
   ```

#### Opción 2: Desde la Terminal

```bash
cd Auxiliares/Examenes_Fin_de_Periodo/04-Examen-Fin-de-Periodo-4
Rscript SemilleroFinDePeriodo_v4.R
```

## 📊 Salida del Sistema

El script genera automáticamente 4 archivos en el directorio `salida/`:

1. **Evaluacion_Fin_de_Periodo_4-docx1.docx** - Examen en formato Word CON soluciones
2. **Evaluacion_Fin_de_Periodo_4_sin_sol1.docx** - Examen en formato Word SIN soluciones
3. **Evaluacion_Fin_de_Periodo_4_sol1.pdf** - Examen en formato PDF CON soluciones
4. **Evaluacion_Fin_de_Periodo_41.pdf** - Examen en formato PDF SIN soluciones

## 🔧 Configuración Avanzada

### Modificar el Número de Ejercicios

Para cambiar el número de ejercicios seleccionados (por defecto 15), editar la línea 17 del script:

```r
NUM_EJERCICIOS <- 15  # Cambiar este valor
```

### Modificar el Nombre del Examen

Para cambiar el nombre de los archivos generados, editar la línea 30:

```r
nombre_sin_extension <- "Evaluacion_Fin_de_Periodo_4"  # Cambiar este valor
```

### Usar una Semilla Específica

Para reproducir exactamente el mismo examen, comentar la línea 26 y establecer una semilla fija:

```r
# semilla <- sample(100:1e8, 1)  # Comentar esta línea
semilla <- 12345  # Usar una semilla específica
```

## 📋 Banco de Ejercicios

El sistema cuenta con **178 ejercicios** ordenados por fecha de modificación:

- **001-muestreo_sesgo_municipio_aleatorio_argumentacion_n2_v1.Rmd** (más reciente)
- **002-cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_2.Rmd**
- **003-cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_1.Rmd**
- ... (175 ejercicios más)
- **178-vuelo_acrobatico_A.Rmd** (más antiguo)

### Criterios de Selección de Ejercicios

Los ejercicios incluidos en este banco cumplen con los siguientes criterios:

✅ **Tipo de ejercicio**: Solo ejercicios tipo `schoice` (selección múltiple)  
✅ **Exclusión de Cloze**: No se incluyen ejercicios tipo `cloze`  
✅ **Funcionalidad**: Solo ejercicios funcionales y compilables  
✅ **Ordenamiento**: Por fecha de modificación según historial de Git

## 🛠️ Manejo de Errores

El script implementa un sistema robusto de manejo de errores:

- **Validación inicial**: Verifica que haya al menos 15 ejercicios disponibles
- **Try-Catch por formato**: Si falla la generación de un formato, continúa con los demás
- **Mensajes informativos**: Muestra claramente qué archivos se generaron exitosamente y cuáles fallaron
- **Continuidad**: El sistema no se detiene ante errores individuales

### Ejemplo de Salida con Errores

```
================================================================================
  GENERANDO EXAMEN EN FORMATO DOCX (CON SOLUCIONES)
================================================================================

✓ Examen DOCX (con soluciones) generado exitosamente

================================================================================
  GENERANDO EXAMEN EN FORMATO DOCX (SIN SOLUCIONES)
================================================================================

✗ ERROR al generar examen DOCX (sin soluciones):
  Error en la compilación de LaTeX

  Continuando con los siguientes formatos...
```

## 📝 Registro de Ejecución

Cada vez que se ejecuta el script, se muestra:

1. **Total de ejercicios disponibles**
2. **Lista de los 15 ejercicios seleccionados**
3. **Semilla aleatoria utilizada** (para reproducibilidad)
4. **Estado de generación de cada formato**
5. **Resumen final con archivos generados**

### Ejemplo de Salida

```
================================================================================
  GENERACIÓN DE EXAMEN DE FIN DE PERÍODO 4
================================================================================

Total de ejercicios .Rmd disponibles: 178

Seleccionando aleatoriamente 15 ejercicios...

Ejercicios seleccionados para el examen:
----------------------------------------
 1. 045-ejercicio_ejemplo_1.Rmd
 2. 123-ejercicio_ejemplo_2.Rmd
 3. 078-ejercicio_ejemplo_3.Rmd
 ...
15. 156-ejercicio_ejemplo_15.Rmd

Semilla aleatoria utilizada: 87654321
```

## 🔄 Actualización del Banco de Ejercicios

Para agregar nuevos ejercicios al banco:

1. Copiar el archivo .Rmd al directorio `04-Examen-Fin-de-Periodo-4/`
2. Renombrar con el prefijo numérico apropiado (ej: `179-nuevo_ejercicio.Rmd`)
3. Asegurarse de que el ejercicio sea tipo `schoice` (no `cloze`)
4. El script detectará automáticamente el nuevo ejercicio en la próxima ejecución

## 📞 Soporte y Contacto

Para problemas o preguntas sobre el sistema:

- **Repositorio**: proyecto-r-exams-icfes-matematicas-optimizado
- **Usuario GitHub**: alvaretto
- **Sistema**: Manjaro Plasma KDE con VSCode + Augment IA

## 📜 Licencia

Este sistema es parte del proyecto ICFES R-Exams para la generación de ejercicios matemáticos.

---

**Última actualización**: 2025-01-30  
**Versión del script**: 4.0  
**Total de ejercicios**: 178

