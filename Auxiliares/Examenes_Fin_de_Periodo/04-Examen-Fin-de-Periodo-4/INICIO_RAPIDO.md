# 🚀 INICIO RÁPIDO - EXAMEN FIN DE PERÍODO 4

## ⚡ Generación Rápida del Examen

### Opción 1: Desde RStudio (Recomendado)

```r
# 1. Abrir RStudio
# 2. Establecer directorio de trabajo
setwd("Auxiliares/Examenes_Fin_de_Periodo/04-Examen-Fin-de-Periodo-4")

# 3. Ejecutar el script
source("SemilleroFinDePeriodo_v4.R")
```

### Opción 2: Desde Terminal

```bash
cd Auxiliares/Examenes_Fin_de_Periodo/04-Examen-Fin-de-Periodo-4
Rscript SemilleroFinDePeriodo_v4.R
```

---

## 📦 Requisitos Previos

### Software Necesario

- ✅ **R** (versión 4.0+)
- ✅ **RStudio** (recomendado)
- ✅ **LaTeX** (para PDF)
- ✅ Paquete **exams**

### Instalación del Paquete exams

```r
install.packages("exams")
```

### Instalación de LaTeX

**Linux (Manjaro/Arch):**
```bash
sudo pacman -S texlive-most
```

**Ubuntu/Debian:**
```bash
sudo apt-get install texlive-full
```

---

## 📊 ¿Qué Hace el Script?

1. **Detecta** automáticamente los 178 ejercicios disponibles
2. **Selecciona** aleatoriamente 15 ejercicios
3. **Genera** 4 archivos de salida:
   - DOCX con soluciones
   - DOCX sin soluciones
   - PDF con soluciones
   - PDF sin soluciones

---

## 📁 Archivos Generados

Los archivos se guardan en el directorio `salida/`:

```
salida/
├── Evaluacion_Fin_de_Periodo_4-docx1.docx       # DOCX con soluciones
├── Evaluacion_Fin_de_Periodo_4_sin_sol1.docx    # DOCX sin soluciones
├── Evaluacion_Fin_de_Periodo_4_sol1.pdf         # PDF con soluciones
└── Evaluacion_Fin_de_Periodo_41.pdf             # PDF sin soluciones
```

---

## 🔧 Configuración Rápida

### Cambiar el Número de Ejercicios

Editar línea 17 de `SemilleroFinDePeriodo_v4.R`:

```r
NUM_EJERCICIOS <- 20  # Cambiar de 15 a 20 (o el número deseado)
```

### Usar una Semilla Fija (Reproducir Examen)

Editar líneas 26-27 de `SemilleroFinDePeriodo_v4.R`:

```r
# semilla <- sample(100:1e8, 1)  # Comentar esta línea
semilla <- 12345678  # Usar semilla fija
```

---

## 📋 Ejemplo de Salida del Script

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
 4. 156-ejercicio_ejemplo_4.Rmd
 5. 012-ejercicio_ejemplo_5.Rmd
 6. 089-ejercicio_ejemplo_6.Rmd
 7. 167-ejercicio_ejemplo_7.Rmd
 8. 034-ejercicio_ejemplo_8.Rmd
 9. 101-ejercicio_ejemplo_9.Rmd
10. 145-ejercicio_ejemplo_10.Rmd
11. 067-ejercicio_ejemplo_11.Rmd
12. 178-ejercicio_ejemplo_12.Rmd
13. 023-ejercicio_ejemplo_13.Rmd
14. 098-ejercicio_ejemplo_14.Rmd
15. 134-ejercicio_ejemplo_15.Rmd

Semilla aleatoria utilizada: 87654321

================================================================================
  GENERANDO EXAMEN EN FORMATO DOCX (CON SOLUCIONES)
================================================================================

✓ Examen DOCX (con soluciones) generado exitosamente

================================================================================
  GENERANDO EXAMEN EN FORMATO DOCX (SIN SOLUCIONES)
================================================================================

✓ Examen DOCX (sin soluciones) generado exitosamente

================================================================================
  GENERANDO EXAMEN EN FORMATO PDF (CON SOLUCIONES)
================================================================================

✓ Examen PDF (con soluciones) generado exitosamente

================================================================================
  GENERANDO EXAMEN EN FORMATO PDF (SIN SOLUCIONES)
================================================================================

✓ Examen PDF (sin soluciones) generado exitosamente

================================================================================
  GENERACIÓN DE EXAMEN COMPLETADA
================================================================================

Semilla utilizada: 87654321
Número de ejercicios: 15
Directorio de salida: salida

Archivos generados:
  - Evaluacion_Fin_de_Periodo_4-docx1.docx (con soluciones)
  - Evaluacion_Fin_de_Periodo_4_sin_sol1.docx (sin soluciones)
  - Evaluacion_Fin_de_Periodo_4_sol1.pdf (con soluciones)
  - Evaluacion_Fin_de_Periodo_41.pdf (sin soluciones)

================================================================================
```

---

## ❓ Solución de Problemas Comunes

### Error: "No se encuentra el paquete 'exams'"

**Solución:**
```r
install.packages("exams")
```

### Error: "LaTeX no encontrado"

**Solución:**
- Instalar LaTeX según tu sistema operativo (ver sección de requisitos)
- Verificar instalación: `pdflatex --version`

### Error: "No hay suficientes ejercicios disponibles"

**Solución:**
- Verificar que hay al menos 15 archivos .Rmd con prefijo numérico (001-...)
- Reducir `NUM_EJERCICIOS` si hay menos de 15 ejercicios

### Error al compilar un ejercicio específico

**Comportamiento:**
- El script continuará con los demás formatos
- Se mostrará un mensaje de error pero no se detendrá

---

## 📚 Documentación Adicional

- **README.md** - Documentación completa del sistema
- **LISTA_EJERCICIOS.md** - Lista de los 178 ejercicios disponibles
- **RESUMEN_PROYECTO.md** - Resumen técnico del proyecto

---

## 🎯 Próximos Pasos

1. ✅ Ejecutar el script
2. ✅ Verificar archivos en `salida/`
3. ✅ Revisar los exámenes generados
4. ✅ Ajustar configuración si es necesario
5. ✅ Generar nuevos exámenes con diferentes semillas

---

## 💡 Consejos Útiles

- **Reproducibilidad**: Guarda la semilla aleatoria si necesitas regenerar el mismo examen
- **Variedad**: Ejecuta el script múltiples veces para obtener diferentes combinaciones
- **Personalización**: Modifica `NUM_EJERCICIOS` según tus necesidades
- **Backup**: Los archivos originales .Rmd no se modifican, solo se copian

---

**¿Listo para generar tu examen?** 🚀

```r
source("SemilleroFinDePeriodo_v4.R")
```

---

**Última actualización**: 2025-01-30  
**Versión**: 4.0

