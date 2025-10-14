# Comandos Útiles - Chrome DevTools MCP

Referencia rápida de comandos útiles para gestionar el servidor MCP de Chrome DevTools.

## 🔧 Scripts Incluidos

### Menú de Ayuda Interactivo
```bash
./ayuda.sh
```
Abre un menú interactivo con todas las opciones disponibles.

### Verificar Instalación
```bash
./test_mcp.sh
```
Verifica que todos los requisitos estén instalados y la configuración sea correcta.

## 📁 Gestión de Archivos de Configuración

### Ver configuración actual
```bash
cat ~/.config/Code\ -\ Insiders/User/globalStorage/github.copilot-chat/mcp.json
```

### Ver configuración formateada (con jq)
```bash
cat ~/.config/Code\ -\ Insiders/User/globalStorage/github.copilot-chat/mcp.json | jq '.'
```

### Editar configuración
```bash
code-insiders ~/.config/Code\ -\ Insiders/User/globalStorage/github.copilot-chat/mcp.json
```

### Hacer backup de la configuración
```bash
cp ~/.config/Code\ -\ Insiders/User/globalStorage/github.copilot-chat/mcp.json \
   ~/.config/Code\ -\ Insiders/User/globalStorage/github.copilot-chat/mcp.json.backup
```

### Restaurar backup
```bash
cp ~/.config/Code\ -\ Insiders/User/globalStorage/github.copilot-chat/mcp.json.backup \
   ~/.config/Code\ -\ Insiders/User/globalStorage/github.copilot-chat/mcp.json
```

## 🔄 VS Code Insiders

### Reiniciar VS Code Insiders
```bash
killall code-insiders && code-insiders
```

### Abrir VS Code Insiders en el directorio actual
```bash
code-insiders .
```

### Ver versión de VS Code Insiders
```bash
code-insiders --version
```

### Listar extensiones instaladas
```bash
code-insiders --list-extensions
```

## 📦 Gestión del Paquete MCP

### Ver versión instalada
```bash
npm view chrome-devtools-mcp version
```

### Ver información completa del paquete
```bash
npm view chrome-devtools-mcp
```

### Forzar actualización
```bash
npx -y chrome-devtools-mcp@latest --help
```

### Ver ayuda del servidor
```bash
npx chrome-devtools-mcp@latest --help
```

### Probar el servidor manualmente
```bash
npx chrome-devtools-mcp@latest --headless=false
```

## 🗂️ Gestión de Caché

### Ver directorio de caché
```bash
ls -lh ~/.cache/chrome-devtools-mcp/
```

### Ver perfiles de Chrome
```bash
find ~/.cache/chrome-devtools-mcp/ -type d -name "chrome-profile-*"
```

### Limpiar caché del servidor
```bash
rm -rf ~/.cache/chrome-devtools-mcp/
```

### Limpiar caché de npm
```bash
npm cache clean --force
```

### Limpiar caché de npx
```bash
npx clear-npx-cache
```

## 🌐 Chrome/Chromium

### Ver versión de Chromium
```bash
chromium --version
```

### Abrir Chromium con DevTools
```bash
chromium --auto-open-devtools-for-tabs
```

### Abrir Chromium en modo headless
```bash
chromium --headless --disable-gpu --screenshot https://example.com
```

### Ver ubicación del ejecutable
```bash
which chromium
```

## 📊 Monitoreo y Logs

### Ver procesos de Chrome/Chromium
```bash
ps aux | grep chromium
```

### Ver procesos de Node.js
```bash
ps aux | grep node
```

### Monitorear uso de recursos
```bash
top -p $(pgrep chromium | tr '\n' ',' | sed 's/,$//')
```

### Ver logs del sistema
```bash
journalctl -f | grep -i chrome
```

## 🔍 Debugging

### Ejecutar con debug habilitado
Edita `mcp.json` y agrega:
```json
{
  "mcpServers": {
    "chrome-devtools": {
      "command": "npx",
      "args": ["-y", "chrome-devtools-mcp@latest"],
      "env": {
        "DEBUG": "*"
      }
    }
  }
}
```

### Ver variables de entorno
```bash
env | grep -i chrome
```

### Verificar puertos en uso
```bash
netstat -tulpn | grep LISTEN
```

## 🧪 Testing

### Probar conexión a una URL
```bash
curl -I https://developers.chrome.com
```

### Verificar DNS
```bash
nslookup developers.chrome.com
```

### Probar JavaScript en Chromium
```bash
chromium --headless --disable-gpu --repl
```

## 📝 Documentación

### Abrir README principal
```bash
code-insiders README.md
```

### Abrir guía de instalación
```bash
code-insiders README_INSTALACION.md
```

### Abrir ejemplos de uso
```bash
code-insiders EJEMPLOS_USO.md
```

### Ver documentación en terminal
```bash
less README.md
```

## 🔐 Seguridad

### Ver permisos del archivo de configuración
```bash
ls -l ~/.config/Code\ -\ Insiders/User/globalStorage/github.copilot-chat/mcp.json
```

### Cambiar permisos (solo lectura para otros)
```bash
chmod 644 ~/.config/Code\ -\ Insiders/User/globalStorage/github.copilot-chat/mcp.json
```

### Ver procesos de Chrome con detalles de seguridad
```bash
ps aux | grep chromium | grep sandbox
```

## 🧹 Limpieza

### Limpiar todo (caché, perfiles, etc.)
```bash
rm -rf ~/.cache/chrome-devtools-mcp/
npm cache clean --force
npx clear-npx-cache
```

### Limpiar solo perfiles temporales
```bash
find ~/.cache/chrome-devtools-mcp/ -type d -name "chrome-profile-*" -exec rm -rf {} +
```

## 📋 Información del Sistema

### Ver información de Node.js
```bash
node --version
npm --version
npx --version
```

### Ver información del sistema
```bash
uname -a
```

### Ver uso de disco
```bash
df -h
```

### Ver memoria disponible
```bash
free -h
```

## 🔗 URLs Útiles

### Abrir repositorio oficial
```bash
xdg-open https://github.com/ChromeDevTools/chrome-devtools-mcp
```

### Abrir documentación de Puppeteer
```bash
xdg-open https://pptr.dev/
```

### Abrir Chrome DevTools Protocol
```bash
xdg-open https://chromedevtools.github.io/devtools-protocol/
```

## 🎯 Atajos Rápidos

### Alias útiles (agregar a ~/.bashrc o ~/.zshrc)
```bash
# Alias para MCP Chrome DevTools
alias mcp-chrome-test='cd ~/path/to/MCP-Chrome && ./test_mcp.sh'
alias mcp-chrome-help='cd ~/path/to/MCP-Chrome && ./ayuda.sh'
alias mcp-chrome-config='code-insiders ~/.config/Code\ -\ Insiders/User/globalStorage/github.copilot-chat/mcp.json'
alias vscode-restart='killall code-insiders && code-insiders'
```

### Función para reiniciar VS Code con el proyecto
```bash
# Agregar a ~/.bashrc o ~/.zshrc
vscode-reload() {
    local current_dir=$(pwd)
    killall code-insiders
    sleep 2
    code-insiders "$current_dir"
}
```

## 🚀 Comandos Avanzados

### Ejecutar servidor con opciones personalizadas
```bash
npx chrome-devtools-mcp@latest \
  --channel=canary \
  --headless=true \
  --isolated=true \
  --viewport=1920x1080
```

### Conectar a Chrome remoto
```bash
npx chrome-devtools-mcp@latest --browserUrl=http://localhost:9222
```

### Usar proxy
```bash
npx chrome-devtools-mcp@latest --proxyServer=http://proxy.example.com:8080
```

## 📊 Estadísticas

### Ver tamaño del caché
```bash
du -sh ~/.cache/chrome-devtools-mcp/
```

### Contar archivos en caché
```bash
find ~/.cache/chrome-devtools-mcp/ -type f | wc -l
```

### Ver archivos más grandes en caché
```bash
find ~/.cache/chrome-devtools-mcp/ -type f -exec ls -lh {} \; | sort -k5 -hr | head -10
```

## 🔄 Automatización

### Script para limpiar caché semanalmente
```bash
#!/bin/bash
# Agregar a crontab: 0 0 * * 0 /path/to/clean_mcp_cache.sh

echo "Limpiando caché de Chrome DevTools MCP..."
rm -rf ~/.cache/chrome-devtools-mcp/chrome-profile-*
npm cache clean --force
echo "Caché limpiado: $(date)"
```

### Agregar a crontab
```bash
crontab -e
# Agregar: 0 0 * * 0 /path/to/clean_mcp_cache.sh
```

---

**Tip**: Guarda este archivo como referencia rápida. Puedes buscar comandos con:
```bash
grep -i "palabra_clave" COMANDOS_UTILES.md
```

