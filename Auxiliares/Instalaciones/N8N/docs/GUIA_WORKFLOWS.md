# 📚 Guía Completa de Workflows N8N - ICFES Matemáticas

## 🎯 Workflows Implementados

### **1. 🔧 Validador Automático de Archivos .Rmd ICFES**

**Archivo:** `workflows/validador_rmd_icfes.json`  
**Estado:** ✅ Activo y funcionando  
**Frecuencia:** Cada 30 minutos  

#### **Función:**
- Escanea archivos .Rmd modificados en las últimas 24 horas
- Ejecuta auto-corrector v2.0 mejorado
- Corrige automáticamente errores de encoding y metadatos
- Genera logs detallados con estadísticas

#### **Cómo usar:**
1. **Automático:** Se ejecuta cada 30 minutos sin intervención
2. **Manual:** Hacer clic en "Execute Workflow" en N8N
3. **Monitoreo:** Ver logs en tiempo real en N8N dashboard

#### **Tipos de errores que corrige:**
- `encoding_mal_ubicado`: Encoding dentro de chunks R
- `encoding_duplicado`: Encoding repetido después de YAML
- `answerlist_corrupto`: Encoding en secciones Answerlist
- `metadatos_faltantes`: extype, exsolution faltantes

#### **Salida esperada:**
```json
{
  "archivos_procesados": 10,
  "errores_corregidos": 27,
  "tipos_errores": {
    "encoding_mal_ubicado": 11,
    "encoding_duplicado": 4,
    "answerlist_corrupto": 2
  },
  "version": "2.0_mejorado"
}
```

---

### **2. 📊 Compilador Masivo Multi-formato ICFES**

**Archivo:** `workflows/compilador_masivo_icfes.json`  
**Estado:** ✅ Listo para usar  
**Trigger:** Manual o webhook  

#### **Función:**
- Compila archivos .Rmd a múltiples formatos simultáneamente
- Genera PDF, HTML, y archivos Moodle
- Timeout inteligente de 3 minutos por proceso
- Cuenta resultados automáticamente

#### **Cómo usar:**
1. **Importar workflow** desde archivo JSON
2. **Ejecutar manualmente** haciendo clic en "Execute Workflow"
3. **Seleccionar carpeta** (por defecto: 01-S2-2025-SEDQ)
4. **Monitorear progreso** en tiempo real

#### **Carpetas soportadas:**
- `01-S2-2025-SEDQ`
- `02-S1-2025-SEDQ`
- `05-S1-2025-SEDQ`
- `05-S2-2025-SEDQ`
- `07-S2-2025-SEDQ`
- `08-Rnw`

#### **Proceso de compilación:**
1. **Verificar archivos** en la carpeta seleccionada
2. **Compilar PDF** usando SemilleroUnico_v2.R
3. **Compilar Moodle** usando SemilleroMoodle_v2.R
4. **Contar resultados** y generar reporte

#### **Salida esperada:**
```json
{
  "pdfs_generados": 5,
  "archivos_moodle": 3,
  "archivos_salida": 8,
  "compilacion_exitosa": true,
  "resumen": "PDFs: 5, Moodle: 3, Salida: 8"
}
```

---

### **3. 🎨 Replicador TikZ Automático PNG→TikZ**

**Archivo:** `workflows/replicador_tikz_automatico.json`  
**Estado:** ✅ Listo para usar  
**Trigger:** Webhook POST  

#### **Función:**
- Convierte imágenes PNG a código TikZ automáticamente
- Analiza descripción para generar código apropiado
- Guarda archivos .tex en `Auxiliares/TikZ_Generated/`
- Respuesta JSON con código TikZ generado

#### **Cómo usar:**

**Método 1: Webhook (Recomendado)**
```bash
curl -X POST http://localhost:5678/webhook/tikz-replicator \
  -H "Content-Type: application/json" \
  -d '{
    "image_url": "https://ejemplo.com/imagen.png",
    "description": "círculo con radio 2cm",
    "complexity": "medium"
  }'
```

**Método 2: Manual en N8N**
1. Hacer clic en "Execute Workflow"
2. Proporcionar datos de entrada en formato JSON
3. Ver resultado en tiempo real

#### **Parámetros de entrada:**
- `image_url` o `image_base64`: Imagen a convertir
- `description`: Descripción de la figura (requerido)
- `complexity`: "simple", "medium", "complex" (opcional)
- `output_format`: "tikz" (por defecto)

#### **Tipos de figuras soportadas:**
- **Círculos:** "círculo", "circle"
- **Triángulos:** "triángulo", "triangle"
- **Cuadrados:** "cuadrado", "square"
- **Gráficos:** "gráfico", "graph"

#### **Salida esperada:**
```json
{
  "success": true,
  "message": "🎨 Conversión TikZ completada exitosamente",
  "tikz_code": "\\begin{tikzpicture}\n\\draw[thick] (0,0) circle (2cm);\n\\end{tikzpicture}",
  "filename": "tikz_generated_2025-07-27T15-30-00.tex",
  "location": "/Auxiliares/TikZ_Generated/tikz_generated_2025-07-27T15-30-00.tex"
}
```

---

### **4. 🚨 Monitor de Errores de Compilación**

**Archivo:** `workflows/monitor_errores_compilacion.json`  
**Estado:** ✅ Listo para activar  
**Frecuencia:** Cada 15 minutos  

#### **Función:**
- Testea compilación de archivos .Rmd recientes
- Detecta errores proactivamente antes de que afecten el trabajo
- Ejecuta auto-corrección automática en casos críticos
- Genera alertas según severidad

#### **Cómo usar:**
1. **Importar workflow** desde archivo JSON
2. **Activar** usando el toggle "Active"
3. **Monitorear** logs automáticamente cada 15 minutos
4. **Revisar alertas** cuando se detecten errores

#### **Niveles de severidad:**
- **🟢 Low (Healthy):** 0 errores detectados
- **🟡 Medium (Warning):** < 50% archivos con errores
- **🔴 High (Critical):** ≥ 50% archivos con errores

#### **Acciones automáticas:**
- **Healthy:** Log de sistema OK
- **Warning:** Alerta con detalles de errores
- **Critical:** Auto-corrección + escalamiento

#### **Métricas monitoreadas:**
- Archivos testeados por ciclo
- Tasa de error (%)
- Puntuación de salud (%)
- Tiempo de respuesta por archivo

#### **Salida esperada:**
```json
{
  "files_tested": 5,
  "files_with_errors": 1,
  "files_successful": 4,
  "error_rate": 20.0,
  "health_score": 80.0,
  "status": "warning",
  "severity": "medium"
}
```

---

## 🚀 Importación de Workflows

### **Pasos para importar:**
1. **Abrir N8N:** http://localhost:5678
2. **Nuevo workflow:** Clic en "+ New workflow"
3. **Importar:** "..." → "Import from file"
4. **Seleccionar archivo:** Elegir el .json correspondiente
5. **Guardar:** Clic en "Save" después de importar
6. **Activar:** Toggle "Active" si es automático

### **Archivos de workflows:**
```
Auxiliares/Instalaciones/N8N/workflows/
├── validador_rmd_icfes.json          # ✅ YA IMPORTADO
├── compilador_masivo_icfes.json      # 📊 LISTO PARA IMPORTAR
├── replicador_tikz_automatico.json   # 🎨 LISTO PARA IMPORTAR
└── monitor_errores_compilacion.json  # 🚨 LISTO PARA IMPORTAR
```

---

## 🔧 Troubleshooting

### **Problemas comunes:**

#### **1. Workflow no se ejecuta automáticamente**
- ✅ Verificar que esté "Active"
- ✅ Revisar configuración de Schedule Trigger
- ✅ Comprobar logs en N8N dashboard

#### **2. Auto-corrector no funciona**
- ✅ Verificar permisos: `chmod +x core/auto_corrector_icfes.py`
- ✅ Probar manualmente: `python3 core/auto_corrector_icfes.py --scan-recent`
- ✅ Revisar ruta en el workflow

#### **3. Compilación falla**
- ✅ Verificar que R y exams estén instalados
- ✅ Comprobar rutas de archivos en el workflow
- ✅ Revisar timeout (3 minutos por defecto)

#### **4. Webhook no responde**
- ✅ Verificar que N8N esté ejecutándose
- ✅ Comprobar URL: http://localhost:5678/webhook/[path]
- ✅ Revisar formato JSON de entrada

### **Logs y debugging:**
- **N8N logs:** ~/.n8n/logs/n8n.log
- **Workflow execution:** Ver en N8N dashboard
- **Auto-corrector:** Salida directa en terminal

---

## 📈 Próximas Mejoras

### **Funcionalidades planificadas:**
1. **🤖 Integración con IA** para análisis de imágenes real
2. **📧 Notificaciones por email** en errores críticos
3. **📊 Dashboard personalizado** con métricas
4. **🔄 Sincronización automática** con Git
5. **🎯 Workflows específicos** por tipo de ejercicio

### **Optimizaciones:**
1. **⚡ Paralelización** de compilaciones
2. **🧠 Machine learning** para detección de errores
3. **📱 Interfaz móvil** para monitoreo
4. **☁️ Backup en la nube** automático

---

**Documentación actualizada:** 27 de Julio, 2025  
**Versión:** 1.0  
**Autor:** Augment Agent
