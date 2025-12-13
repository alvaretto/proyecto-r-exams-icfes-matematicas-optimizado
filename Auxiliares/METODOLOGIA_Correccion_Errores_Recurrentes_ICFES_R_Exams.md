# METODOLOGÍA AVANZADA PARA CORRECCIÓN DE ERRORES RECURRENTES EN R-EXAMS ICFES

## Versión 1.0 - Estrategia Sistemática de Detección y Corrección

### INTRODUCCIÓN

Esta metodología complementa la **Estrategia TikZ Avanzada** y proporciona un sistema sistemático para identificar, diagnosticar y corregir errores recurrentes en archivos .Rmd del proyecto ICFES R-exams. Basada en patrones identificados tras múltiples correcciones, esta estrategia reduce significativamente el tiempo de depuración y mejora la calidad del código.

---

## FASE 1: SISTEMA DE DETECCIÓN AUTOMÁTICA

### 1.1 Categorías de Errores Recurrentes

#### **CATEGORÍA A: Errores Gramaticales y de Concordancia**
- **A1**: Concordancia de género (artículos con sustantivos)
- **A2**: Concordancia de número (singular/plural)
- **A3**: Uso incorrecto de preposiciones
- **A4**: Errores de acentuación en variables dinámicas

#### **CATEGORÍA B: Errores de Posicionamiento TikZ**
- **B1**: Elementos superpuestos o mal posicionados
- **B2**: Tablas que aparecen antes del texto explicativo
- **B3**: Coordenadas incorrectas en nodos
- **B4**: Escalas inconsistentes entre elementos

#### **CATEGORÍA C: Errores de Generación de Datos**
- **C1**: Opciones de respuesta duplicadas
- **C2**: Valores idénticos en datasets (problemas de moda/mediana)
- **C3**: Rangos de aleatorización incorrectos
- **C4**: Falta de validación de unicidad

#### **CATEGORÍA D: Errores de Compilación LaTeX/TikZ**
- **D1**: Paquetes faltantes o mal configurados
- **D2**: Caracteres especiales sin escapar
- **D3**: Comandos TikZ incompatibles
- **D4**: Problemas de encoding

#### **CATEGORÍA E: Errores de Estructura R-exams**
- **E1**: Headers YAML incompletos o incorrectos
- **E2**: Funciones include_tikz mal configuradas
- **E3**: Variables no definidas en chunks
- **E4**: Problemas con exsolution y answerlist

### 1.2 Patrones de Detección Automática

```r
# Función de detección automática de errores
detectar_errores_recurrentes <- function(archivo_rmd) {
  contenido <- readLines(archivo_rmd)
  errores_detectados <- list()
  
  # A1: Detectar concordancia de género
  patron_genero <- "\\b(La|El)\\s+(conteo|número|total|cantidad|suma)\\b"
  
  # B2: Detectar orden incorrecto tabla-texto
  patron_orden_tikz <- "% Tabla de datos.*% Texto explicativo"
  
  # C1: Detectar posibles duplicados en opciones
  patron_opciones <- "opciones_finales.*sample"
  
  # D2: Detectar caracteres sin escapar
  patron_caracteres <- "[%$#&](?!\\\\)"
  
  return(errores_detectados)
}
```

---

## FASE 2: BIBLIOTECA DE SOLUCIONES PROBADAS

### 2.1 Soluciones para Categoría A (Gramaticales)

#### **Solución A1: Sistema de Concordancia de Género**
```r
# Implementación probada para concordancia automática
terminos_cantidad_data <- data.frame(
  termino = c("cantidad", "número", "total", "suma", "conteo"),
  articulo = c("La", "El", "El", "La", "El"),
  stringsAsFactors = FALSE
)
termino_seleccionado <- sample(1:nrow(terminos_cantidad_data), 1)
termino_cantidad <- terminos_cantidad_data$termino[termino_seleccionado]
articulo_cantidad <- terminos_cantidad_data$articulo[termino_seleccionado]

# Uso en texto: `r articulo_cantidad` `r termino_cantidad`
```

#### **Solución A2: Concordancia de Número**
```r
# Para elementos que varían en número
elementos_data <- data.frame(
  singular = c("elemento", "dato", "valor"),
  plural = c("elementos", "datos", "valores"),
  stringsAsFactors = FALSE
)
```

### 2.2 Soluciones para Categoría B (Posicionamiento TikZ)

#### **Solución B2: Orden Correcto Texto-Tabla**
```r
# Estructura correcta en TikZ
"% Texto explicativo PRIMERO\n",
"\\node[anchor=north west, text width=12cm] at (0, 1.5) {\n",
"  \\textbf{Texto explicativo aquí}\n",
"};\n\n",
"% Tabla DESPUÉS del texto\n",
"\\node[anchor=north west] at (0, 0.5) {\n",
"  \\begin{tabular}{...}\n",
"  \\end{tabular}\n",
"};\n\n"
```

### 2.3 Soluciones para Categoría C (Generación de Datos)

#### **Solución C1: Prevención de Duplicados**
```r
# Sistema robusto anti-duplicados
generar_opciones_unicas <- function(respuesta_correcta, num_opciones = 4) {
  opciones <- c(respuesta_correcta)
  intentos <- 0
  max_intentos <- 100
  
  while(length(unique(opciones)) < num_opciones && intentos < max_intentos) {
    nuevo_distractor <- generar_distractor(respuesta_correcta)
    if(!nuevo_distractor %in% opciones) {
      opciones <- c(opciones, nuevo_distractor)
    }
    intentos <- intentos + 1
  }
  
  return(sample(unique(opciones)[1:num_opciones]))
}
```

---

## FASE 3: CHECKLIST DE VALIDACIÓN SISTEMÁTICA

### 3.1 Checklist Pre-Compilación
- [ ] **A1**: Verificar concordancia de género en todos los artículos
- [ ] **A2**: Verificar concordancia de número (singular/plural)
- [ ] **B1**: Verificar posicionamiento correcto de elementos TikZ
- [ ] **B2**: Confirmar orden texto → tabla → pregunta
- [ ] **C1**: Validar que todas las opciones de respuesta son únicas
- [ ] **C2**: Verificar que no hay valores idénticos en datasets
- [ ] **D1**: Confirmar que todos los paquetes LaTeX están incluidos
- [ ] **E1**: Validar estructura completa del header YAML

### 3.2 Checklist Post-Compilación
- [ ] **Visual**: Verificar que la tabla aparece después del texto
- [ ] **Funcional**: Confirmar que todas las opciones son diferentes
- [ ] **Gramatical**: Revisar concordancia en el output final
- [ ] **Matemático**: Validar que los cálculos son correctos

---

## FASE 4: METODOLOGÍA DE CORRECCIÓN PASO A PASO

### 4.1 Protocolo de Corrección Rápida (< 5 minutos)

1. **IDENTIFICACIÓN** (30 segundos)
   - Ejecutar función de detección automática
   - Clasificar error según categorías A-E

2. **APLICACIÓN DE SOLUCIÓN** (2-3 minutos)
   - Consultar biblioteca de soluciones
   - Aplicar solución probada correspondiente
   - Verificar que no se introducen nuevos errores

3. **VALIDACIÓN** (1-2 minutos)
   - Ejecutar checklist de validación
   - Compilar y verificar output
   - Confirmar corrección exitosa

### 4.2 Protocolo de Corrección Compleja (> 5 minutos)

1. **ANÁLISIS PROFUNDO** (5 minutos)
   - Identificar múltiples errores relacionados
   - Determinar causa raíz del problema
   - Planificar corrección integral

2. **CORRECCIÓN SISTEMÁTICA** (10-15 minutos)
   - Aplicar soluciones en orden de dependencia
   - Validar cada corrección antes de continuar
   - Documentar nuevos patrones encontrados

3. **TESTING EXHAUSTIVO** (5 minutos)
   - Ejecutar checklist completo
   - Probar múltiples compilaciones
   - Verificar estabilidad de la solución

---

## FASE 5: INTEGRACIÓN CON METODOLOGÍA TIKZ

### 5.1 Workflow Integrado
1. **Consultar ejemplos funcionales** (Metodología TikZ - Fase 1)
2. **Aplicar detección de errores** (Esta metodología - Fase 1)
3. **Corregir errores sistemáticamente** (Esta metodología - Fase 4)
4. **Validar con checklist TikZ** (Metodología TikZ - Fase 6)
5. **Documentar nuevos patrones** (Ambas metodologías)

### 5.2 Casos de Uso Combinados
- **Replicación TikZ + Corrección de errores**: Aplicar ambas metodologías secuencialmente
- **Optimización de archivos existentes**: Priorizar corrección de errores antes de mejoras TikZ
- **Desarrollo de nuevos ejercicios**: Usar ambas metodologías desde el inicio

---

## DOCUMENTACIÓN Y MEJORA CONTINUA

### Registro de Nuevos Patrones
Cada vez que se identifique un nuevo error recurrente:
1. Documentar el patrón en la categoría correspondiente
2. Desarrollar solución probada
3. Actualizar función de detección automática
4. Añadir al checklist de validación

### Métricas de Efectividad
- Tiempo promedio de corrección por categoría
- Tasa de reincidencia de errores
- Efectividad de la detección automática
- Satisfacción del usuario con las correcciones

---

**Nota**: Esta metodología debe usarse en conjunto con la consulta obligatoria a ejemplos funcionales en `/A-Produccion/Ejemplos-Funcionales-Rmd/` antes de cualquier corrección.
