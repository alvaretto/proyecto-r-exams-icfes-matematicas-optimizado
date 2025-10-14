# Uso de Chrome DevTools MCP con R/exams

Esta guía te muestra cómo usar el servidor MCP de Chrome DevTools específicamente para tu proyecto de exámenes ICFES con R/exams.

## 🎯 Casos de Uso para R/exams

### 1. Verificar Renderizado de Exámenes HTML

Cuando generas exámenes HTML con R/exams, puedes usar Copilot para verificar que se vean correctamente.

**Prompt en Copilot:**
```
Abre el archivo file:///home/proyectos/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams/out_html_cloze/render11.html y toma un screenshot
```

**Variante - Verificar múltiples archivos:**
```
Abre estos archivos HTML y toma un screenshot de cada uno:
1. file:///.../out_html_cloze/render11.html
2. file:///.../out_html_cloze/render121.html
3. file:///.../out_html_cloze/render131.html
```

---

### 2. Detectar Errores de JavaScript en Exámenes

Verifica si hay errores de JavaScript en tus exámenes generados.

**Prompt en Copilot:**
```
Abre file:///.../out_html_cloze/render11.html y muéstrame todos los errores de JavaScript en la consola
```

---

### 3. Verificar Imágenes y Recursos

Asegúrate de que todas las imágenes y recursos se carguen correctamente.

**Prompt en Copilot:**
```
Abre file:///.../out_html_cloze/render11.html y:
1. Lista todas las peticiones de red
2. Muéstrame cuáles fallaron (404, 500, etc.)
3. Identifica imágenes que no cargaron
```

---

### 4. Analizar Rendimiento de Exámenes

Verifica qué tan rápido cargan tus exámenes HTML.

**Prompt en Copilot:**
```
Analiza el rendimiento de file:///.../out_html_cloze/render11.html y dime:
1. Tiempo de carga total
2. Tamaño de todos los recursos
3. Recursos más pesados
```

---

### 5. Verificar Responsive Design

Comprueba cómo se ven los exámenes en diferentes dispositivos.

**Prompt en Copilot:**
```
Abre file:///.../out_html_cloze/render11.html y muéstrame cómo se ve en:
1. Móvil (375x667)
2. Tablet (768x1024)
3. Desktop (1920x1080)
Toma un screenshot de cada uno
```

---

### 6. Extraer Contenido de Preguntas

Extrae el texto de las preguntas para revisión.

**Prompt en Copilot:**
```
Abre file:///.../out_html_cloze/render11.html y ejecuta:
Array.from(document.querySelectorAll('.question')).map(q => q.textContent.trim())
```

---

### 7. Verificar Fórmulas Matemáticas (MathJax/KaTeX)

Asegúrate de que las fórmulas matemáticas se rendericen correctamente.

**Prompt en Copilot:**
```
Abre file:///.../out_html_cloze/render11.html, espera 3 segundos (para que MathJax cargue) y toma un screenshot
```

**Verificar errores de MathJax:**
```
Abre file:///.../out_html_cloze/render11.html y ejecuta:
document.querySelectorAll('.MathJax_Error, .MathJax_Preview').length
```

---

### 8. Comparar Versiones de Exámenes

Compara dos versiones del mismo examen.

**Prompt en Copilot:**
```
Compara visualmente estos dos archivos:
1. file:///.../out_html_cloze/render11.html
2. file:///.../out_html_cloze/render121.html
Toma screenshots y dime las diferencias principales
```

---

### 9. Verificar Estilos CSS

Revisa que los estilos CSS se apliquen correctamente.

**Prompt en Copilot:**
```
Abre file:///.../out_html_cloze/render11.html y ejecuta:
Array.from(document.styleSheets).map(s => ({
  href: s.href,
  rules: s.cssRules.length
}))
```

---

### 10. Generar Documentación Visual

Crea capturas de pantalla para documentación.

**Prompt en Copilot:**
```
Abre file:///.../out_html_cloze/render11.html y:
1. Toma un screenshot de la página completa
2. Toma un screenshot solo de la primera pregunta
3. Guarda las imágenes con nombres descriptivos
```

---

## 🔧 Scripts de Automatización

### Script 1: Verificar Todos los Exámenes HTML

Crea un script que Copilot puede usar para verificar todos tus exámenes:

**Prompt en Copilot:**
```
Crea un script que:
1. Liste todos los archivos HTML en out_html_cloze/
2. Abra cada uno en Chrome
3. Verifique errores de consola
4. Tome un screenshot
5. Genere un reporte con los resultados
```

---

### Script 2: Validación de Imágenes

**Prompt en Copilot:**
```
Para cada archivo HTML en out_html_cloze/:
1. Abre el archivo
2. Lista todas las imágenes (<img> tags)
3. Verifica que cada imagen cargue correctamente
4. Reporta imágenes rotas o faltantes
```

---

### Script 3: Análisis de Rendimiento Batch

**Prompt en Copilot:**
```
Analiza el rendimiento de todos los archivos HTML en out_html_cloze/ y genera un reporte CSV con:
- Nombre del archivo
- Tiempo de carga
- Tamaño total
- Número de recursos
- Errores encontrados
```

---

## 📋 Flujo de Trabajo Recomendado

### Paso 1: Generar Exámenes con R
```r
# En R
library(exams)
exams2html("mi_examen.Rmd", dir = "out_html_cloze")
```

### Paso 2: Verificar con Copilot

Abre Copilot en VS Code Insiders y usa:

```
Verifica el último examen generado en out_html_cloze/:
1. Abre el archivo HTML más reciente
2. Verifica errores de consola
3. Toma un screenshot
4. Verifica que todas las imágenes carguen
5. Dame un resumen de cualquier problema encontrado
```

### Paso 3: Corregir Problemas

Si Copilot encuentra problemas, úsalo para investigar:

```
El examen tiene un error de JavaScript. Muéstrame:
1. El mensaje de error completo
2. La línea de código que causa el error
3. El stack trace
```

### Paso 4: Validar Correcciones

```
Abre nuevamente el archivo y verifica que el error esté corregido
```

---

## 🎨 Ejemplos Específicos para ICFES

### Verificar Metadatos ICFES

**Prompt en Copilot:**
```
Abre file:///.../out_html_cloze/render11.html y extrae los metadatos ICFES:
document.querySelectorAll('meta[name^="icfes"]')
```

### Verificar Competencias

**Prompt en Copilot:**
```
Abre el examen y ejecuta:
Array.from(document.querySelectorAll('[data-competencia]')).map(el => ({
  competencia: el.dataset.competencia,
  componente: el.dataset.componente,
  afirmacion: el.dataset.afirmacion
}))
```

### Verificar Niveles de Dificultad

**Prompt en Copilot:**
```
Analiza el examen y muéstrame la distribución de niveles de dificultad:
Array.from(document.querySelectorAll('[data-nivel]')).reduce((acc, el) => {
  acc[el.dataset.nivel] = (acc[el.dataset.nivel] || 0) + 1;
  return acc;
}, {})
```

---

## 🐛 Debugging Común

### Problema: Imágenes no cargan

**Prompt en Copilot:**
```
Abre file:///.../out_html_cloze/render11.html y:
1. Lista todas las rutas de imágenes
2. Verifica cuáles devuelven 404
3. Muéstrame las rutas completas de las imágenes rotas
```

### Problema: MathJax no renderiza

**Prompt en Copilot:**
```
Abre el examen, espera 5 segundos y ejecuta:
{
  mathjaxLoaded: typeof MathJax !== 'undefined',
  errors: document.querySelectorAll('.MathJax_Error').length,
  formulas: document.querySelectorAll('.MathJax').length
}
```

### Problema: CSS no se aplica

**Prompt en Copilot:**
```
Abre el examen y verifica:
1. Qué archivos CSS se cargaron
2. Si hay errores de CORS
3. Si los estilos se aplicaron correctamente
```

---

## 📊 Reportes Automatizados

### Reporte de Calidad de Exámenes

**Prompt en Copilot:**
```
Genera un reporte de calidad para todos los exámenes en out_html_cloze/:

Para cada examen, verifica:
1. ✅ Sin errores de JavaScript
2. ✅ Todas las imágenes cargan
3. ✅ MathJax renderiza correctamente
4. ✅ Tiempo de carga < 3 segundos
5. ✅ Responsive en móvil

Genera un reporte markdown con los resultados
```

---

## 🔗 Integración con Git

### Verificar Cambios Visuales

Cuando haces cambios en plantillas:

**Prompt en Copilot:**
```
Compara visualmente:
1. Versión anterior: file:///.../backup/render11.html
2. Versión nueva: file:///.../out_html_cloze/render11.html

Toma screenshots y dime qué cambió visualmente
```

---

## 💡 Tips y Trucos

### 1. Usar rutas absolutas

Siempre usa rutas absolutas completas:
```
file:///home/proyectos/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams/out_html_cloze/render11.html
```

### 2. Esperar a que cargue MathJax

Siempre espera unos segundos para MathJax:
```
Abre el archivo, espera 3 segundos, luego toma screenshot
```

### 3. Usar selectores específicos

Usa selectores CSS específicos de tu proyecto:
```
document.querySelectorAll('.icfes-question')
document.querySelectorAll('[data-competencia]')
```

### 4. Guardar screenshots con nombres descriptivos

```
Toma un screenshot y guárdalo como "examen_geometria_nivel2_screenshot.png"
```

---

## 🚀 Comandos Rápidos

### Verificación Rápida
```
Abre file:///.../out_html_cloze/render11.html, verifica errores y toma screenshot
```

### Análisis Completo
```
Analiza completamente file:///.../out_html_cloze/render11.html:
- Errores de consola
- Imágenes rotas
- Rendimiento
- Responsive design
Dame un reporte completo
```

### Comparación Rápida
```
Compara render11.html y render121.html visualmente
```

---

## 📚 Recursos Adicionales

- [Documentación R/exams](http://www.r-exams.org/)
- [Guía de instalación MCP](./README_INSTALACION.md)
- [Ejemplos generales MCP](./EJEMPLOS_USO.md)

---

**Nota**: Recuerda que debes usar estos prompts en **Copilot dentro de VS Code Insiders**, no en Augment Agent.

