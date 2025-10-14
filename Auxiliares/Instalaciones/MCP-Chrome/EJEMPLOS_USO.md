# Ejemplos de Uso - Chrome DevTools MCP

Esta guía contiene ejemplos prácticos de cómo usar el servidor MCP de Chrome DevTools con Copilot en VS Code Insiders.

## 🎯 Ejemplos Básicos

### 1. Análisis de Rendimiento

**Prompt:**
```
Analiza el rendimiento de https://developers.chrome.com y dame un reporte detallado
```

**Qué hace:**

- Abre Chrome automáticamente
- Navega a la URL
- Graba un trace de rendimiento
- Analiza métricas como LCP, FID, CLS
- Proporciona recomendaciones de optimización

---

### 2. Tomar Screenshots

**Prompt:**
```
Toma un screenshot de https://github.com
```

**Qué hace:**

- Navega a GitHub
- Captura una imagen de la página
- Guarda el screenshot

**Variante - Screenshot de elemento específico:**
```
Navega a https://github.com y toma un screenshot solo del header
```

---

### 3. Inspeccionar Peticiones de Red

**Prompt:**
```
Abre https://example.com y muéstrame todas las peticiones de red que se hicieron
```

**Qué hace:**

- Navega a la página
- Captura todas las peticiones HTTP
- Muestra URLs, métodos, códigos de estado
- Muestra tamaños y tiempos de carga

**Variante - Filtrar peticiones:**
```
Abre https://example.com y muéstrame solo las peticiones que fallaron (4xx o 5xx)
```

---

### 4. Ejecutar JavaScript

**Prompt:**
```
Navega a https://example.com y ejecuta: document.title
```

**Qué hace:**

- Navega a la página
- Ejecuta el código JavaScript
- Devuelve el resultado

**Ejemplo más complejo:**
```
Navega a https://github.com y ejecuta:
Array.from(document.querySelectorAll('a')).map(a => a.href).slice(0, 10)
```

---

### 5. Verificar Mensajes de Consola

**Prompt:**
```
Abre https://example.com y muéstrame todos los mensajes de la consola
```

**Qué hace:**

- Navega a la página
- Captura todos los logs, warnings y errores
- Muestra el contenido de cada mensaje

---

## 🔧 Ejemplos Avanzados

### 6. Emular Dispositivo Móvil

**Prompt:**
```
Cambia el viewport a 375x667 (iPhone SE) y navega a https://example.com
```

**Qué hace:**

- Configura el tamaño de viewport
- Navega a la página
- Permite ver cómo se ve en móvil

**Variante - Diferentes dispositivos:**
```
# iPad
Cambia el viewport a 768x1024 y navega a https://example.com

# Desktop HD
Cambia el viewport a 1920x1080 y navega a https://example.com
```

---

### 7. Emular Conexión Lenta

**Prompt:**
```
Emula una conexión 3G lenta y navega a https://example.com, luego analiza el rendimiento
```

**Qué hace:**

- Configura throttling de red (3G)
- Navega a la página
- Mide el rendimiento bajo condiciones de red limitadas

**Variantes de red:**
```
# Offline
Emula estar offline y navega a https://example.com

# 4G
Emula una conexión 4G y navega a https://example.com
```

---

### 8. Emular CPU Lenta

**Prompt:**
```
Emula una CPU 4x más lenta y navega a https://example.com
```

**Qué hace:**

- Reduce la velocidad de la CPU
- Navega a la página
- Permite ver el rendimiento en dispositivos lentos

---

### 9. Automatización de Formularios

**Prompt:**
```
Navega a https://example.com/contact, rellena el formulario con:
- Nombre: Juan Pérez
- Email: juan@example.com
- Mensaje: Hola, esto es una prueba
Y luego toma un screenshot
```

**Qué hace:**

- Navega al formulario
- Rellena los campos automáticamente
- Captura el resultado

---

### 10. Pruebas de Accesibilidad

**Prompt:**
```
Navega a https://example.com y toma un snapshot del DOM para analizar la accesibilidad
```

**Qué hace:**

- Navega a la página
- Captura el árbol de accesibilidad
- Permite analizar problemas de accesibilidad

---

## 🎨 Ejemplos para Desarrollo Web

### 11. Verificar Responsive Design

**Prompt:**
```
Verifica cómo se ve https://mi-sitio.com en estos tamaños:
1. Móvil (375x667)
2. Tablet (768x1024)
3. Desktop (1920x1080)
Toma un screenshot de cada uno
```

---

### 12. Analizar Recursos Cargados

**Prompt:**
```
Navega a https://example.com y muéstrame:
1. Cuántos archivos CSS se cargaron
2. Cuántos archivos JS se cargaron
3. Cuántas imágenes se cargaron
4. El tamaño total de todos los recursos
```

---

### 13. Verificar Tiempos de Carga

**Prompt:**
```
Navega a https://example.com y dime:
1. Cuánto tardó en cargar el DOM
2. Cuánto tardó en cargar completamente
3. Cuál fue el recurso más lento en cargar
```

---

### 14. Probar Interacciones

**Prompt:**
```
Navega a https://example.com, haz clic en el botón "Sign Up", espera 2 segundos y toma un screenshot
```

---

### 15. Verificar Cookies y Storage

**Prompt:**
```
Navega a https://example.com y ejecuta:
{
  cookies: document.cookie,
  localStorage: Object.keys(localStorage),
  sessionStorage: Object.keys(sessionStorage)
}
```

---

## 🐛 Ejemplos para Debugging

### 16. Encontrar Errores JavaScript

**Prompt:**
```
Navega a https://example.com y muéstrame todos los errores de JavaScript que ocurrieron
```

---

### 17. Verificar Links Rotos

**Prompt:**
```
Navega a https://example.com, obtén todos los links y verifica cuáles devuelven 404
```

---

### 18. Analizar Fuentes Web

**Prompt:**
```
Navega a https://example.com y ejecuta:
Array.from(document.fonts).map(f => ({
  family: f.family,
  style: f.style,
  weight: f.weight,
  status: f.status
}))
```

---

### 19. Verificar Meta Tags

**Prompt:**
```
Navega a https://example.com y ejecuta:
Array.from(document.querySelectorAll('meta')).map(m => ({
  name: m.name || m.property,
  content: m.content
}))
```

---

### 20. Analizar Rendimiento de Imágenes

**Prompt:**
```
Navega a https://example.com y muéstrame todas las imágenes que son mayores a 100KB
```

---

## 🔄 Ejemplos de Flujos Completos

### 21. Auditoría Completa de Sitio

**Prompt:**
```
Realiza una auditoría completa de https://example.com:
1. Analiza el rendimiento
2. Lista todos los errores de consola
3. Verifica las peticiones de red
4. Toma un screenshot
5. Dame un resumen con recomendaciones
```

---

### 22. Comparación de Rendimiento

**Prompt:**
```
Compara el rendimiento de:
1. https://sitio-a.com
2. https://sitio-b.com
Dame un reporte comparativo
```

---

### 23. Test de Carga Progresiva

**Prompt:**
```
Para https://example.com:
1. Emula 3G lenta
2. Graba el rendimiento
3. Muéstrame cuándo aparece el primer contenido
4. Muéstrame cuándo la página es interactiva
```

---

### 24. Verificación de SEO Básico

**Prompt:**
```
Navega a https://example.com y verifica:
1. Título de la página
2. Meta description
3. Meta keywords
4. Open Graph tags
5. Canonical URL
```

---

### 25. Test de Formulario Completo

**Prompt:**
```
Navega a https://example.com/contact:
1. Rellena el formulario con datos de prueba
2. Toma un screenshot antes de enviar
3. Haz clic en enviar
4. Espera la respuesta
5. Toma un screenshot del resultado
6. Muéstrame los mensajes de consola
```

---

## 💡 Tips y Trucos

### Esperar por Elementos

```
Navega a https://example.com y espera hasta que aparezca el elemento con id "content"
```

### Múltiples Páginas

```
Abre 3 páginas:
1. https://google.com
2. https://github.com
3. https://stackoverflow.com
Y lista todas las páginas abiertas
```

### Navegación por Historial

```
Navega a https://example.com, luego a /about, luego vuelve atrás
```

### Drag and Drop

```
Navega a https://ejemplo-drag-drop.com y arrastra el elemento A al elemento B
```

### Manejo de Diálogos

```
Navega a https://example.com, haz clic en el botón que abre un alert y acéptalo
```

---

## 🚨 Notas Importantes

1. **Primera ejecución**: La primera vez que uses una herramienta, Chrome se abrirá automáticamente
2. **Headless mode**: Si quieres que Chrome no se muestre, configura `--headless=true`
3. **Perfiles**: Por defecto se usa un perfil persistente. Usa `--isolated=true` para sesiones temporales
4. **Timeouts**: Algunas operaciones pueden tardar. Sé paciente en la primera ejecución
5. **Seguridad**: No compartas información sensible, el MCP tiene acceso completo al navegador

---

## 📚 Recursos Adicionales

- [Documentación oficial](https://github.com/ChromeDevTools/chrome-devtools-mcp)
- [Referencia de herramientas](https://github.com/ChromeDevTools/chrome-devtools-mcp#tools)
- [Puppeteer Docs](https://pptr.dev/) (motor de automatización usado)
- [Chrome DevTools Protocol](https://chromedevtools.github.io/devtools-protocol/)

---

**¿Tienes más ideas de ejemplos?** Contribuye al repositorio o crea tus propios casos de uso.

