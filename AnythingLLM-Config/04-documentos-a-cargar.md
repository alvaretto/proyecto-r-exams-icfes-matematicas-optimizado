# 📚 Documentos a Cargar en AnythingLLM

## 🎯 PRIORIDAD ALTA (Cargar primero)

### Reglas y Configuración Principal
```
✅ .augment/rules/reglas-generales.md
✅ .augment/rules/siempre.md
✅ .agent.md
✅ AnythingLLM-Config/01-README_AnythingLLM_ICFES.md
✅ Auxiliares/Agentes-IA/01-GUIA_COMPLETA_Agentes_Workflows_ICFES_R_Exams.md
```

**Razón**: Estos archivos definen la filosofía completa del proyecto, reglas obligatorias y configuración de agentes.

---

## 📊 PRIORIDAD MEDIA (Cargar segundo)

### Metodologías y Guías
```
✅ Auxiliares/METODOLOGIA_Correccion_Errores_Recurrentes_ICFES_R_Exams.md
✅ Auxiliares/BIBLIOTECA_Soluciones_Errores_Comunes.md
✅ Auxiliares/CHECKLIST_Validacion_Archivos_Rmd.md
✅ Auxiliares/README_AGENTE_TIKZ.md
```

### Metadatos ICFES
```
✅ Auxiliares/plantilla_metadatos_icfes.md
✅ Auxiliares/matriz_alineacion_icfes.md
✅ Auxiliares/guia_implementacion_icfes.md
```

**Razón**: Información crítica para validación, corrección y clasificación ICFES.

---

## 💡 EJEMPLOS FUNCIONALES (Cargar tercero)

### Todos los archivos .Rmd en:
```
✅ Auxiliares/Ejemplos-Funcionales-Rmd/
   ├── Ejemplo_00_numeros_triangulares_sucesion_argumentacion_n2_v1.Rmd
   ├── estadistica_diagramas_caja_interpretacion_representacion_Nivel2_v2.Rmd
   ├── mediana_aleatorio_argumentacion_n2_v1.Rmd
   ├── fracciones_reparto_premio_v1.Rmd
   └── [todos los demás archivos .Rmd]
```

**Razón**: Patrones validados y funcionales que los agentes deben seguir.

---

## 🔧 DOCUMENTACIÓN TÉCNICA (Cargar cuarto)

### Python y Reticulate
```
✅ Auxiliares/Python-Documentation/Python-ICFES-Guide.md
✅ Auxiliares/Python-Documentation/[otros archivos .md]
```

### TikZ (si existe)
```
✅ Auxiliares/TikZ-Documentation/[archivos .md]
```

### Estrategias Avanzadas
```
✅ Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/DOCUMENTACION_SISTEMA_ANTERIOR.md
```

**Razón**: Conocimiento técnico específico para generación de gráficos y visualizaciones.

---

## 📁 ESTRUCTURA DEL PROYECTO (Cargar quinto)

### Documentación de Estructura
```
✅ Auxiliares/Estructura-Repositorio/Estructura_Repositorio.md
✅ Estructura-Repositorio/Estructura_Repositorio.md
✅ README.md
```

**Razón**: Comprensión de la organización del proyecto.

---

## 🎓 DOCUMENTACIÓN EDUCATIVA (Opcional)

### Guías de Instalación y Uso
```
⏳ Auxiliares/Rexams-Lubuntu/GUIA_INSTALACION_R_EXAMS_ICFES.md
⏳ Auxiliares/Documentacion/Errores-Y-Soluciones/INDICE_ERRORES_COMUNES_ICFES_R_EXAMS.md
```

**Razón**: Información complementaria para troubleshooting.

---

## 📋 CHECKLIST DE CARGA

### Paso 1: Preparar Archivos
- [ ] Verificar que todos los archivos existen
- [ ] Confirmar rutas correctas
- [ ] Revisar que archivos no estén corruptos

### Paso 2: Cargar en AnythingLLM
- [ ] Abrir http://localhost:3001
- [ ] Ir a workspace "ICFES R-Exams"
- [ ] Seleccionar "Upload Documents"
- [ ] Cargar archivos por prioridad

### Paso 3: Configurar Embeddings
- [ ] Seleccionar modelo de embeddings
- [ ] Configurar chunk size: 1000
- [ ] Configurar chunk overlap: 200
- [ ] Iniciar procesamiento

### Paso 4: Verificar Carga
- [ ] Confirmar que todos los documentos están indexados
- [ ] Probar búsqueda de conceptos clave
- [ ] Verificar que agentes pueden acceder a la información

---

## 🔍 VERIFICACIÓN DE CARGA EXITOSA

### Pruebas de Búsqueda

**Prueba 1: Reglas Generales**
```
Pregunta: ¿Cuál es la estructura obligatoria de un archivo .Rmd?
Respuesta esperada: Debe incluir YAML header, chunk setup, generar_datos(), etc.
```

**Prueba 2: Ejemplos Funcionales**
```
Pregunta: Muéstrame un ejemplo de ejercicio de mediana
Respuesta esperada: Debe encontrar y mostrar ejemplo funcional
```

**Prueba 3: Metadatos ICFES**
```
Pregunta: ¿Cuáles son las competencias ICFES válidas?
Respuesta esperada: interpretacion_representacion, formulacion_ejecucion, argumentacion
```

**Prueba 4: Corrección de Errores**
```
Pregunta: ¿Cómo corregir error de sintaxis TikZ?
Respuesta esperada: Debe encontrar soluciones en biblioteca de errores
```

---

## 📊 ESTADÍSTICAS DE CARGA

### Documentos Totales Recomendados
- **Prioridad Alta**: ~5 archivos
- **Prioridad Media**: ~7 archivos
- **Ejemplos Funcionales**: ~10-15 archivos .Rmd
- **Documentación Técnica**: ~5-10 archivos
- **Estructura**: ~3 archivos

**Total estimado**: 30-40 documentos

### Tamaño Estimado
- **Total**: ~50-100 MB
- **Tiempo de procesamiento**: 10-20 minutos
- **Chunks generados**: ~5,000-10,000

---

## 🚀 SCRIPT DE CARGA AUTOMÁTICA

Crear script `05-cargar-documentos.sh`:

```bash
#!/bin/bash

# Script para preparar documentos para carga en AnythingLLM
# Crea un directorio con todos los archivos organizados

PROYECTO_ROOT="/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams"
DESTINO="$HOME/anythingllm-docs-icfes"

echo "Preparando documentos para AnythingLLM..."

# Crear estructura
mkdir -p "$DESTINO"/{prioridad-alta,prioridad-media,ejemplos,tecnica,estructura}

# Copiar archivos de prioridad alta
cp "$PROYECTO_ROOT/.augment/rules/reglas-generales.md" "$DESTINO/prioridad-alta/"
cp "$PROYECTO_ROOT/.augment/rules/siempre.md" "$DESTINO/prioridad-alta/"
cp "$PROYECTO_ROOT/.agent.md" "$DESTINO/prioridad-alta/"
cp "$PROYECTO_ROOT/AnythingLLM-Config/01-README_AnythingLLM_ICFES.md" "$DESTINO/prioridad-alta/"
cp "$PROYECTO_ROOT/Auxiliares/Agentes-IA/01-GUIA_COMPLETA_Agentes_Workflows_ICFES_R_Exams.md" "$DESTINO/prioridad-alta/"

# Copiar archivos de prioridad media
cp "$PROYECTO_ROOT/Auxiliares/METODOLOGIA_Correccion_Errores_Recurrentes_ICFES_R_Exams.md" "$DESTINO/prioridad-media/"
cp "$PROYECTO_ROOT/Auxiliares/BIBLIOTECA_Soluciones_Errores_Comunes.md" "$DESTINO/prioridad-media/"
cp "$PROYECTO_ROOT/Auxiliares/CHECKLIST_Validacion_Archivos_Rmd.md" "$DESTINO/prioridad-media/"
cp "$PROYECTO_ROOT/Auxiliares/plantilla_metadatos_icfes.md" "$DESTINO/prioridad-media/"

# Copiar ejemplos funcionales
cp "$PROYECTO_ROOT/Auxiliares/Ejemplos-Funcionales-Rmd"/*.Rmd "$DESTINO/ejemplos/" 2>/dev/null || true

echo "✅ Documentos preparados en: $DESTINO"
echo ""
echo "Ahora puedes cargarlos en AnythingLLM desde la interfaz web"
```

---

## 💡 RECOMENDACIONES

### Orden de Carga
1. **Primero**: Reglas y configuración (base del conocimiento)
2. **Segundo**: Metodologías y guías (procesos)
3. **Tercero**: Ejemplos funcionales (patrones validados)
4. **Cuarto**: Documentación técnica (detalles específicos)
5. **Quinto**: Estructura del proyecto (contexto)

### Actualización de Documentos
- **Frecuencia**: Semanal o cuando haya cambios importantes
- **Método**: Re-indexar workspace completo
- **Verificación**: Ejecutar pruebas de búsqueda después de actualizar

### Optimización
- **Eliminar duplicados**: Evitar cargar el mismo contenido múltiples veces
- **Priorizar calidad**: Mejor pocos documentos bien seleccionados que muchos irrelevantes
- **Mantener actualizado**: Documentos desactualizados pueden generar respuestas incorrectas

---

**¡Sigue esta guía para una carga óptima de documentos en AnythingLLM!** 📚✨

