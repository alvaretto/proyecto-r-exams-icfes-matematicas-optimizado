# 📊 EJERCICIO AHORRO CON PORCENTAJES - R-EXAMS ICFES

## 📋 INFORMACIÓN GENERAL

**Archivo:** `Ahorro_opciones_porcentaje_interpretacion_representacion_n2_v1.Rnw`  
**Tipo:** Ejercicio de selección múltiple (schoice)  
**Tema:** Matemáticas Financieras - Porcentajes y Ahorro  
**Nivel:** Secundaria (ICFES)  
**Estado:** ✅ FUNCIONAL - Compilación exitosa  

## 🎯 DESCRIPCIÓN DEL EJERCICIO

Este ejercicio presenta un problema de ahorro donde un estudiante debe elegir entre dos opciones de ayuda familiar basadas en porcentajes sobre montos acumulados. Los estudiantes deben:

1. **Calcular** los regalos mensuales para cada opción
2. **Comparar** los totales acumulados
3. **Interpretar** cuál opción es más beneficiosa
4. **Representar** matemáticamente los cálculos

## 🔧 CARACTERÍSTICAS TÉCNICAS

### ✅ Aleatorización Implementada
- **Nombres:** 6 opciones aleatorias para el protagonista
- **Familiares:** 4 combinaciones de tío/tía
- **Montos:** Ahorro mensual entre $125,000 - $200,000
- **Porcentajes Opción 1:** Constante entre 8% - 12%
- **Porcentajes Opción 2:** Variables (1-5%, 3-7%, 15-25%)
- **Duración:** 3-5 meses aleatorio

### 📊 Datos Generados
```r
# Ejemplo de datos aleatorios:
datos$nombre <- "Ana"
datos$familiar1 <- "tío"
datos$familiar2 <- "tía"
datos$ahorro_mensual <- 150000
datos$meses <- 3
datos$porcentaje_constante <- 10
datos$porcentajes_variables <- c(3, 4, 20)
```

## 🧮 ESTRUCTURA MATEMÁTICA

### Cálculos Principales
1. **Ahorros Acumulados:** `ahorro_mensual * (1:meses)`
2. **Regalos Opción 1:** `ahorros_acumulados * porcentaje_constante / 100`
3. **Regalos Opción 2:** `ahorros_acumulados * porcentajes_variables / 100`
4. **Totales:** Suma de todos los regalos por opción

### Validaciones Implementadas
- ✅ Diferencia mínima entre opciones (>$10,000)
- ✅ Coherencia en porcentajes variables
- ✅ Rangos realistas de montos
- ✅ Verificación de respuesta correcta

## 📁 ARCHIVOS RELACIONADOS

### Archivos Principales
- `Ahorro_opciones_porcentaje_interpretacion_representacion_n2_v1.Rnw` - Ejercicio principal
- `test_ahorro_final.R` - Script de testing completo
- `README_Ahorro_Ejercicio.md` - Esta documentación

### Archivos de Testing Generados
- `test_compilacion_basica1.html` - Test básico HTML
- `test_multiples_versiones1-5.html` - Múltiples versiones
- `test_compilacion_pdf1.pdf` - Test PDF
- `test_seed11.html`, `test_seed21.html` - Tests de aleatorización

## 🚀 INSTRUCCIONES DE USO

### Compilación HTML
```r
library(exams)
exams2html('Ahorro_opciones_porcentaje_interpretacion_representacion_n2_v1.Rnw', 
           n = 3, 
           name = 'ahorro_html', 
           dir = '.')
```

### Compilación PDF
```r
library(exams)
exams2pdf('Ahorro_opciones_porcentaje_interpretacion_representacion_n2_v1.Rnw', 
          n = 3, 
          name = 'ahorro_pdf', 
          dir = '.')
```

### Exportar a Moodle
```r
library(exams)
exams2moodle('Ahorro_opciones_porcentaje_interpretacion_representacion_n2_v1.Rnw', 
             n = 10, 
             name = 'ahorro_moodle', 
             dir = '.')
```

## 🧪 TESTING

### Ejecutar Tests Completos
```bash
cd Lab-Manjaro/08-Rnw
Rscript test_ahorro_final.R
```

### Tests Incluidos
1. ✅ **Compilación básica HTML**
2. ✅ **Múltiples versiones (5)**
3. ✅ **Compilación PDF**
4. ✅ **Verificación de aleatorización**
5. ✅ **Verificación de archivos generados**

## 📈 RESULTADOS DE TESTING

```
✅ TEST 1 EXITOSO: Compilación HTML básica
✅ TEST 2 EXITOSO: Múltiples versiones HTML
✅ TEST 3 EXITOSO: Compilación PDF
✅ TEST 4 EXITOSO: Aleatorización verificada
✅ TEST 5 EXITOSO: Todos los archivos generados correctamente
```

## 🎓 COMPETENCIAS EVALUADAS

### Matemáticas
- **Cálculo de porcentajes** sobre montos variables
- **Operaciones con decimales** y formateo de moneda
- **Comparación numérica** y toma de decisiones
- **Interpretación de resultados** matemáticos

### Pensamiento Crítico
- **Análisis de opciones** financieras
- **Evaluación de beneficios** a largo plazo
- **Interpretación de datos** tabulares
- **Razonamiento lógico** matemático

## 🔍 ERRORES COMUNES IDENTIFICADOS

1. **Error 1:** Confundir porcentaje promedio con cálculo real
2. **Error 2:** Considerar solo el último mes
3. **Error 3:** Sumar porcentajes sin considerar bases
4. **Error 4:** Invertir la conclusión por lectura incorrecta

## 📝 NOTAS TÉCNICAS

### Correcciones Aplicadas
- ✅ Eliminados entornos `\itemize` problemáticos
- ✅ Reemplazados `\item` por texto directo
- ✅ Corregida sintaxis LaTeX para expresiones matemáticas
- ✅ Mejorado formato de moneda con separadores

### Avisos Conocidos
- Comandos duplicados en metadatos (no crítico)
- Avisos de TTM en compilación HTML (no afecta resultado)

## 🎯 PRÓXIMOS PASOS

1. **Integrar** en banco de ejercicios ICFES
2. **Crear** variantes con diferentes contextos
3. **Desarrollar** ejercicios complementarios
4. **Validar** con estudiantes reales

---

**Fecha de creación:** 2025-01-16  
**Última actualización:** 2025-01-16  
**Autor:** Augment Agent  
**Estado:** Producción ✅
