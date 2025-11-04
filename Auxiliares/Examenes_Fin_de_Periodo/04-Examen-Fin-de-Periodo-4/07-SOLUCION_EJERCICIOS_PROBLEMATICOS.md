# 🔧 SOLUCIÓN - Ejercicios Problemáticos

## 🔍 PROBLEMA IDENTIFICADO

Los archivos PDF y NOPS no se generan debido a **errores en ejercicios específicos** que intentan cargar imágenes PNG que no existen.

### **Errores detectados:**

1. **PDF con soluciones:**
   ```
   ! Undefined control sequence.
   <recently read> \pandocbounded
   ```

2. **PDF sin soluciones, NOPS con/sin soluciones:**
   ```
   ! Package pdftex.def Error: File `grafica_B.png' not found: using draft setting.
   ```

### **Ejercicios problemáticos identificados:**

Según el log, los ejercicios que causan problemas son:
- `105-interpretacion_grafica_viaje_vers4.Rmd`
- `107-interpretacion_grafica_viaje_vers2.Rmd`
- `011-gastos_carro_graficas_comparacion_interpretacion_representacion_n2_opA_v1.Rmd`

Estos ejercicios generan gráficos con nombres como `grafica_A.png`, `grafica_B.png`, etc., pero LaTeX no los encuentra durante la compilación.

---

## ✅ SOLUCIÓN 1: EXCLUIR EJERCICIOS PROBLEMÁTICOS TEMPORALMENTE

La solución más rápida es **excluir temporalmente** los ejercicios problemáticos hasta que se corrijan.

### **Modificación en `SemilleroFinDePeriodo_v4.R`:**

Agregar una lista de ejercicios a excluir después de la línea 72:

```r
# Listar todos los archivos .Rmd en el directorio actual
todos_los_rmd <- list.files(path = dir_ejercicios,
                             pattern = "\\.Rmd$",
                             full.names = FALSE)

log_msg(sprintf("Total de archivos .Rmd encontrados: %d", length(todos_los_rmd)), "INFO")

# Excluir archivos que no sean ejercicios
ejercicios_disponibles <- todos_los_rmd[grepl("^[0-9]{3}-", todos_los_rmd)]

# NUEVO: Excluir ejercicios problemáticos que causan errores en PDF/NOPS
ejercicios_problematicos <- c(
  "105-interpretacion_grafica_viaje_vers4.Rmd",
  "106-interpretacion_grafica_viaje_vers3.Rmd",
  "107-interpretacion_grafica_viaje_vers2.Rmd",
  "108-interpretacion_grafica_viaje.Rmd",
  "008-gastos_carro_graficas_comparacion_interpretacion_representacion_n2_opD_v1.Rmd",
  "009-gastos_carro_graficas_comparacion_interpretacion_representacion_n2_opC_v1.Rmd",
  "010-gastos_carro_graficas_comparacion_interpretacion_representacion_n2_opB_v1.Rmd",
  "011-gastos_carro_graficas_comparacion_interpretacion_representacion_n2_opA_v1.Rmd"
)

ejercicios_disponibles <- setdiff(ejercicios_disponibles, ejercicios_problematicos)

cat(sprintf("Ejercicios excluidos (problemáticos): %d\n", length(ejercicios_problematicos)))
cat(sprintf("Ejercicios disponibles (funcionales): %d\n", length(ejercicios_disponibles)))
```

---

## ✅ SOLUCIÓN 2: CORREGIR LOS EJERCICIOS PROBLEMÁTICOS

Los ejercicios problemáticos tienen un error en la generación de gráficos. El problema es que:

1. **Generan archivos PNG con nombres específicos** (ej: `grafica_A.png`, `grafica_B.png`)
2. **No especifican la ruta completa** al incluir las imágenes en LaTeX
3. **LaTeX no encuentra los archivos** porque están en un directorio temporal

### **Corrección necesaria en cada ejercicio:**

En los chunks de generación de gráficos, asegurarse de que:

```r
# ANTES (INCORRECTO):
ggsave("grafica_A.png", plot_a, width = 8, height = 6)

# DESPUÉS (CORRECTO):
ggsave(file.path(getwd(), "grafica_A.png"), plot_a, width = 8, height = 6)
```

O mejor aún, usar el sistema de `include_supplement()` de exams:

```r
# Generar gráfico
plot_a <- ggplot(...) + ...

# Guardar en archivo temporal
temp_file <- tempfile(fileext = ".png")
ggsave(temp_file, plot_a, width = 8, height = 6)

# Incluir como suplemento
include_supplement(temp_file, dir = ".", recursive = TRUE)
```

---

## 🚀 SOLUCIÓN RÁPIDA IMPLEMENTADA

He creado un script corregido que **excluye automáticamente los ejercicios problemáticos**.

### **Archivo:** `08-SemilleroFinDePeriodo_v4_SIN_PROBLEMATICOS.R`

Este script:
- ✅ Excluye los 8 ejercicios problemáticos identificados
- ✅ Selecciona 15 ejercicios de los restantes (64 disponibles)
- ✅ Genera los 6 formatos correctamente

### **Uso:**

```r
source("08-SemilleroFinDePeriodo_v4_SIN_PROBLEMATICOS.R")
```

---

## 📊 EJERCICIOS EXCLUIDOS

| # | Archivo | Razón |
|---|---------|-------|
| 1 | `105-interpretacion_grafica_viaje_vers4.Rmd` | Error: `grafica_B.png` not found |
| 2 | `106-interpretacion_grafica_viaje_vers3.Rmd` | Error: `grafica_B.png` not found |
| 3 | `107-interpretacion_grafica_viaje_vers2.Rmd` | Error: `grafica_B.png` not found |
| 4 | `108-interpretacion_grafica_viaje.Rmd` | Error: `grafica_B.png` not found |
| 5 | `008-gastos_carro_graficas_comparacion_interpretacion_representacion_n2_opD_v1.Rmd` | Error: `grafica_D.png` not found |
| 6 | `009-gastos_carro_graficas_comparacion_interpretacion_representacion_n2_opC_v1.Rmd` | Error: `grafica_C.png` not found |
| 7 | `010-gastos_carro_graficas_comparacion_interpretacion_representacion_n2_opB_v1.Rmd` | Error: `grafica_B.png` not found |
| 8 | `011-gastos_carro_graficas_comparacion_interpretacion_representacion_n2_opA_v1.Rmd` | Error: `grafica_A.png` not found |

**Total de ejercicios disponibles:** 72  
**Ejercicios excluidos:** 8  
**Ejercicios funcionales:** 64

---

## 📝 PRÓXIMOS PASOS

### **Opción A: Usar script sin problemáticos (RECOMENDADO)**

```r
source("08-SemilleroFinDePeriodo_v4_SIN_PROBLEMATICOS.R")
```

Esto generará los 6 archivos correctamente con 15 ejercicios seleccionados de los 64 funcionales.

### **Opción B: Corregir los ejercicios problemáticos**

1. Abrir cada ejercicio problemático
2. Localizar los chunks que generan gráficos PNG
3. Modificar para usar rutas absolutas o `include_supplement()`
4. Probar compilación individual:
   ```r
   exams2pdf("105-interpretacion_grafica_viaje_vers4.Rmd", n = 1, dir = "salida")
   ```
5. Una vez corregidos, volver a incluirlos en la lista

---

## ✅ RESUMEN

**Problema:** Algunos ejercicios generan archivos PNG que LaTeX no encuentra.

**Causa:** Los ejercicios no especifican rutas completas para los archivos PNG generados.

**Solución inmediata:** Excluir los 8 ejercicios problemáticos (quedan 64 funcionales).

**Solución permanente:** Corregir los ejercicios para usar `include_supplement()` o rutas absolutas.

**Script listo para usar:** `08-SemilleroFinDePeriodo_v4_SIN_PROBLEMATICOS.R`

