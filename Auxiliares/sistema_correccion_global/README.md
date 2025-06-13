# 🎯 SISTEMA GLOBAL DE CORRECCIÓN SEMÁNTICA Y GRAMATICAL

## 📋 Descripción General

Sistema centralizado para la corrección automática de errores semánticos, gramaticales y de estilo en archivos .Rmd del proyecto R-exams ICFES Matemáticas.

## 🚀 Características Principales

- ✅ **Corrección automática** de errores de concordancia de género
- ✅ **Validación de coherencia** entre entidades y elementos
- ✅ **Aplicación masiva** a múltiples archivos
- ✅ **Validación automática** con reportes detallados
- ✅ **Fácil integración** en nuevos ejercicios
- ✅ **Escalabilidad** para nuevos tipos de errores

## 📁 Estructura del Sistema

```
Auxiliares/sistema_correccion_global/
├── funciones_correccion.R      # Funciones centralizadas
├── aplicador_automatico.R      # Aplicación a archivos existentes
├── template_inclusion.R        # Template para nuevos ejercicios
├── validador_masivo.R         # Validación y reportes
└── README.md                  # Esta documentación
```

## 🔧 Instalación y Configuración

### 1. Verificar Estructura
Asegúrate de que el directorio `Auxiliares/sistema_correccion_global/` existe con todos los archivos.

### 2. Cargar el Sistema
```r
# Cargar funciones principales
source("Auxiliares/sistema_correccion_global/funciones_correccion.R")

# Cargar aplicador automático (opcional)
source("Auxiliares/sistema_correccion_global/aplicador_automatico.R")

# Cargar validador masivo (opcional)
source("Auxiliares/sistema_correccion_global/validador_masivo.R")
```

## 🎯 Uso del Sistema

### Para Archivos Nuevos

1. **Incluir en el chunk de inicio:**
```r
```{r inicio, include=FALSE}
# Librerías esenciales
library(exams)
library(stringr)
# ... otras librerías ...

# ⭐ CARGAR SISTEMA DE CORRECCIÓN ⭐
source("Auxiliares/sistema_correccion_global/funciones_correccion.R")
```

2. **Modificar función generar_datos():**
```r
generar_datos <- function() {
  # ... tu lógica aquí ...
  
  # ⭐ APLICAR CORRECCIONES AUTOMÁTICAS ⭐
  datos_corregidos <- procesar_datos_con_correcciones(datos)
  return(datos_corregidos)
}
```

3. **Usar variables corregidas:**
```r
# Variables automáticamente corregidas
elemento <- datos$elemento_texto    # Sin errores de concordancia
condicion <- datos$condicion_texto  # Sin errores de concordancia
```

### Para Archivos Existentes

#### Aplicar a un archivo individual:
```r
source("Auxiliares/sistema_correccion_global/aplicador_automatico.R")
aplicar_correcciones_a_archivo("Lab/mi_ejercicio.Rmd")
```

#### Aplicar a todos los archivos:
```r
source("Auxiliares/sistema_correccion_global/aplicador_automatico.R")
aplicar_correcciones_masivas("Lab/")
```

#### Validar archivos:
```r
source("Auxiliares/sistema_correccion_global/validador_masivo.R")
generar_reporte_validacion("Lab/")
```

## 🔍 Funciones Principales

### `corregir_todos_errores_concordancia(texto)`
Corrige automáticamente errores de concordancia de género.

**Ejemplo:**
```r
corregir_todos_errores_concordancia("empresas registrados")
# Retorna: "empresas registradas"
```

### `procesar_datos_con_correcciones(datos)`
Procesa datos generados aplicando todas las correcciones automáticas.

**Ejemplo:**
```r
datos_corregidos <- procesar_datos_con_correcciones(datos_originales)
# datos_corregidos$elemento_texto  # Versión corregida
# datos_corregidos$condicion_texto # Versión corregida
```

### `validar_coherencia(entidad, elemento, condicion, total)`
Valida coherencia lógica entre componentes.

**Ejemplo:**
```r
errores <- validar_coherencia("ICBF", "familias", "beneficiarias", 1000000)
# Retorna: vector vacío si no hay errores
```

## 📊 Errores Corregidos Automáticamente

| Error Original | Corrección Automática |
|---|---|
| familias matriculados | familias matriculadas |
| familias registrados | familias registradas |
| familias certificados | familias certificadas |
| empresas matriculados | empresas matriculadas |
| empresas registrados | empresas registradas |
| empresas certificados | empresas certificadas |
| ... | ... |

## 🧪 Validación y Pruebas

### Validar un archivo individual:
```r
errores <- validar_archivo_individual("mi_archivo.Rmd")
```

### Generar reporte completo:
```r
resultados <- generar_reporte_validacion("Lab/")
```

### Estadísticas de calidad:
```r
stats <- generar_estadisticas_calidad(".")
```

## 🔄 Flujo de Trabajo Recomendado

### Para Nuevos Ejercicios:
1. Copiar template de `template_inclusion.R`
2. Adaptar función `generar_datos()`
3. Usar variables `elemento_texto` y `condicion_texto`
4. Probar compilación

### Para Archivos Existentes:
1. Ejecutar validación: `generar_reporte_validacion("Lab/")`
2. Aplicar correcciones: `aplicar_correcciones_masivas("Lab/")`
3. Verificar resultados: `generar_estadisticas_calidad("Lab/")`

## 🚀 Comandos Rápidos

```r
# Cargar todo el sistema
source("Auxiliares/sistema_correccion_global/aplicador_automatico.R")

# Validar todo el proyecto
generar_reporte_validacion(".")

# Aplicar correcciones a todo el proyecto
aplicar_correcciones_masivas(".")

# Ver estadísticas de calidad
generar_estadisticas_calidad(".")
```

## 🔧 Extensibilidad

### Agregar nuevos errores:
1. Editar `GENEROS_ELEMENTOS` en `funciones_correccion.R`
2. Agregar a `ADJETIVOS_FORMAS` si es necesario
3. Actualizar patrones en `validador_masivo.R`

### Agregar nuevas coherencias:
1. Editar `COHERENCIAS_ENTIDAD_ELEMENTO` en `funciones_correccion.R`

## 📈 Beneficios

- **Calidad Profesional:** Textos gramaticalmente correctos
- **Eficiencia:** Corrección automática sin intervención manual
- **Escalabilidad:** Fácil agregar nuevos tipos de correcciones
- **Mantenimiento:** Validación continua de calidad
- **Consistencia:** Estándares uniformes en todo el proyecto

## 🎯 Resultado Final

Con este sistema, errores como:
- **"empresas registrados"** → **"empresas registradas"** ✅
- **"familias matriculados"** → **"familias matriculadas"** ✅

Se corrigen automáticamente en todo el proyecto, garantizando calidad profesional y consistencia en todos los ejercicios.
