# 🚀 Tutorial Completo: Instalación de Gemini CLI en Manjaro XFCE

## 📋 Índice
1. [Información del Sistema](#información-del-sistema)
2. [Prerequisitos](#prerequisitos)
3. [Instalación Paso a Paso](#instalación-paso-a-paso)
4. [Configuración](#configuración)
5. [Uso Básico](#uso-básico)
6. [Errores Comunes y Soluciones](#errores-comunes-y-soluciones)
7. [Mantenimiento](#mantenimiento)
8. [Comandos de Referencia Rápida](#comandos-de-referencia-rápida)

---

## 📊 Información del Sistema

**Sistema Operativo:** Manjaro XFCE  
**Usuario:** pequeniomanjaro  
**Carpeta de Instalación:** `/home/pequeniomanjaro/Programas/`  
**Entorno Virtual:** `Gemini-CLI-All`  
**Proyecto:** [Google Gemini CLI](https://github.com/google-gemini/gemini-cli)

---

## 🔧 Prerequisitos

### Verificar Dependencias del Sistema

Antes de comenzar, verifica que tengas instaladas las siguientes herramientas:

```bash
# Verificar Python 3
python3 --version
# Salida esperada: Python 3.13.3 (o superior)

# Verificar Git
git --version
# Salida esperada: git version 2.50.0 (o superior)

# Verificar Node.js
node --version
# Salida esperada: v22.16.0 (o superior, mínimo v18)

# Verificar npm
npm --version
# Salida esperada: 10.9.2 (o superior)
```

### Instalar Dependencias Faltantes

Si alguna dependencia no está instalada:

```bash
# Instalar Python pip (si no está disponible)
sudo pacman -S python-pip --noconfirm

# Instalar Git (si no está disponible)
sudo pacman -S git --noconfirm

# Instalar Node.js y npm (si no están disponibles)
sudo pacman -S nodejs npm --noconfirm
```

---

## 🚀 Instalación Paso a Paso

### Paso 1: Crear la Estructura de Directorios

```bash
# Navegar a la carpeta padre
cd /home/pequeniomanjaro/Programas/

# Verificar que estamos en el lugar correcto
pwd
# Salida esperada: /home/pequeniomanjaro/Programas
```

### Paso 2: Crear el Entorno Virtual Python

```bash
# Crear entorno virtual llamado "Gemini-CLI-All"
python3 -m venv Gemini-CLI-All

# Verificar que se creó correctamente
ls -la Gemini-CLI-All/
# Deberías ver: bin/, include/, lib/, lib64, pyvenv.cfg, .gitignore
```

### Paso 3: Activar el Entorno Virtual

```bash
# Navegar al entorno virtual
cd Gemini-CLI-All

# Activar el entorno virtual
source bin/activate

# Verificar que está activo (debería mostrar la ruta del entorno virtual)
which python
# Salida esperada: /home/pequeniomanjaro/Programas/Gemini-CLI-All/bin/python
```

### Paso 4: Clonar el Repositorio

⚠️ **Nota sobre Error de OpenSSL:** Si encuentras errores relacionados con OpenSSL, ejecuta primero:

```bash
# Solución temporal para conflictos de OpenSSL
unset LD_LIBRARY_PATH
```

Luego clona el repositorio:

```bash
# Clonar el repositorio oficial
git clone https://github.com/google-gemini/gemini-cli.git

# Verificar que se clonó correctamente
ls -la gemini-cli/
# Deberías ver archivos como: README.md, package.json, etc.
```

### Paso 5: Instalar Gemini CLI

```bash
# Instalar globalmente usando npm
npm install -g @google/gemini-cli

# Verificar la instalación
which gemini
# Salida esperada: /home/pequeniomanjaro/.config/nvm/versions/node/v22.16.0/bin/gemini

# Probar que funciona
gemini --help
# Deberías ver la ayuda del comando
```

---

## 🔑 Configuración

### Configurar la Clave API

**Tu Clave API de Gemini:** `AIzaSyAghQk7L3QaE7ZRMSfyMf5YUqjlsfCX0_k`

#### Opción 1: Configuración Temporal (por sesión)

```bash
# Configurar la clave API para la sesión actual
export GEMINI_API_KEY="AIzaSyAghQk7L3QaE7ZRMSfyMf5YUqjlsfCX0_k"

# Verificar que se configuró
echo $GEMINI_API_KEY
```

#### Opción 2: Configuración Permanente

```bash
# Agregar al archivo .bashrc para que sea permanente
echo 'export GEMINI_API_KEY="AIzaSyAghQk7L3QaE7ZRMSfyMf5YUqjlsfCX0_k"' >> ~/.bashrc

# Recargar la configuración
source ~/.bashrc
```

#### Opción 3: Archivo de Configuración Local (RECOMENDADO)

```bash
# Crear archivo de configuración en el proyecto
cd /home/pequeniomanjaro/Programas/Gemini-CLI-All
echo 'GEMINI_API_KEY=AIzaSyAghQk7L3QaE7ZRMSfyMf5YUqjlsfCX0_k' > .env

# Verificar que se creó correctamente
cat .env

# Gemini CLI detecta automáticamente este archivo .env
# No necesitas hacer nada más, solo usar gemini normalmente
```

**✅ VENTAJA:** Gemini CLI detecta automáticamente el archivo `.env` y carga la API Key sin configuración adicional.

---

## 🎯 Uso Básico

### Comandos Fundamentales

```bash
# Activar entorno virtual (siempre hacer esto primero)
cd /home/pequeniomanjaro/Programas/Gemini-CLI-All
source bin/activate

# Configurar API Key (si no está en .bashrc)
export GEMINI_API_KEY="AIzaSyAghQk7L3QaE7ZRMSfyMf5YUqjlsfCX0_k"

# Ejecutar Gemini CLI
gemini
```

### Ejemplos de Uso

```bash
# Hacer una pregunta simple
gemini -p "¿Cuál es la capital de Francia?"

# Analizar archivos en el directorio actual
gemini -p "Analiza los archivos de este proyecto"

# Usar un modelo específico
gemini -m "gemini-2.5-pro" -p "Explica qué es la inteligencia artificial"

# Modo debug para ver más información
gemini -d -p "Ayúdame con este código"

# Incluir todos los archivos en el contexto
gemini -a -p "Revisa todo el proyecto y sugiere mejoras"
```

### Opciones Principales

- `-m, --model`: Especificar modelo (por defecto: "gemini-2.5-pro")
- `-p, --prompt`: Prompt o pregunta
- `-d, --debug`: Modo debug
- `-a, --all_files`: Incluir todos los archivos en contexto
- `-h, --help`: Mostrar ayuda
- `-v, --version`: Mostrar versión

### Modelos Disponibles

#### Gemini 2.5 Pro (Por defecto)
- **Modelo más avanzado y potente**
- **Límite de entrada:** 1,048,576 tokens (~1M tokens)
- **Límite de salida:** 65,536 tokens
- **Uso:** `gemini -p "pregunta"` o `gemini -m gemini-2.5-pro -p "pregunta"`

#### Gemini 2.5 Flash (Alternativo)
- **Modelo más rápido y eficiente**
- **Mismo límite de tokens**
- **Uso:** `gemini -m gemini-2.5-flash -p "pregunta"`

**⚠️ Nota:** Gemini CLI puede cambiar automáticamente entre modelos según disponibilidad del servidor. Esto es normal y beneficioso.

---

## ⚠️ Errores Comunes y Soluciones

### Error 1: Conflicto de OpenSSL al Clonar

**Error:**
```
/usr/lib/git-core/git-remote-https: version `OPENSSL_3.2.0' not found
```

**Solución:**
```bash
# Limpiar variables de entorno conflictivas
unset LD_LIBRARY_PATH

# Luego intentar clonar nuevamente
git clone https://github.com/google-gemini/gemini-cli.git
```

### Error 2: pip no encontrado

**Error:**
```
which: no pip3 in (/usr/bin:/bin/...)
```

**Solución:**
```bash
# Instalar pip usando pacman
sudo pacman -S python-pip --noconfirm

# Verificar instalación
pip --version
```

### Error 3: Node.js versión incompatible

**Error:**
```
Node.js version 16.x.x is not supported
```

**Solución:**
```bash
# Verificar versión actual
node --version

# Si es menor a v18, actualizar:
# Opción 1: Usando pacman
sudo pacman -S nodejs npm

# Opción 2: Usando nvm (si está instalado)
nvm install 22
nvm use 22
```

### Error 4: Permisos de npm

**Error:**
```
EACCES: permission denied, mkdir '/usr/local/lib/node_modules'
```

**Solución:**
```bash
# Configurar npm para usar directorio del usuario
mkdir ~/.npm-global
npm config set prefix '~/.npm-global'

# Agregar al PATH en .bashrc
echo 'export PATH=~/.npm-global/bin:$PATH' >> ~/.bashrc
source ~/.bashrc

# Reinstalar gemini-cli
npm install -g @google/gemini-cli
```

### Error 5: API Key no válida

**Error:**
```
API key not valid or expired
```

**Solución:**
```bash
# Verificar que la clave esté configurada
echo $GEMINI_API_KEY

# Si no aparece, configurarla:
export GEMINI_API_KEY="AIzaSyAghQk7L3QaE7ZRMSfyMf5YUqjlsfCX0_k"

# Verificar que la clave sea correcta en Google AI Studio:
# https://aistudio.google.com/apikey
```

### Error 6: Comando gemini no encontrado

**Error:**
```
bash: gemini: command not found
```

**Solución:**
```bash
# Verificar instalación
npm list -g @google/gemini-cli

# Si no está instalado, reinstalar:
npm install -g @google/gemini-cli

# Verificar PATH
echo $PATH

# Si es necesario, agregar directorio de npm al PATH:
echo 'export PATH="$(npm config get prefix)/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc
```

### Error 7: Conflicto con LD_LIBRARY_PATH (Específico de este sistema)

**Error:**
```
/usr/lib/git-core/git-remote-https: /home/pequeniomanjaro/.local/share/acli/plugin/rovodev/lib/libssl.so.3: version `OPENSSL_3.2.0' not found
```

**Causa:** Conflicto con bibliotecas de otros proyectos (acli/rovodev) que modifican LD_LIBRARY_PATH.

**Solución:**
```bash
# Limpiar variable de entorno antes de usar git
unset LD_LIBRARY_PATH

# O crear un alias para git sin conflictos
alias git-clean='unset LD_LIBRARY_PATH && git'

# Usar el alias para clonar
git-clean clone https://github.com/google-gemini/gemini-cli.git
```

### Error 8: Timeout en respuestas de Gemini

**Error:**
```
Request timeout or no response from Gemini API
```

**Solución:**
```bash
# Verificar conectividad
ping google.com

# Verificar que la API key sea válida
curl -H "Authorization: Bearer $GEMINI_API_KEY" \
     "https://generativelanguage.googleapis.com/v1/models"

# Si hay problemas de red, usar proxy o VPN si es necesario
# Reintentar con timeout mayor
timeout 30 gemini -p "test"
```

### Error 9: API Key no válida o expirada

**Error:**
```
API key not valid. Please pass a valid API key.
GEMINI_API_KEY environment variable not found.
```

**Causa:** La API key puede haber expirado, estar mal configurada, o no tener permisos para Gemini API.

**Solución:**
```bash
# Opción 1: Regenerar API key
# 1. Ir a https://aistudio.google.com/apikey
# 2. Crear nueva API key
# 3. Reemplazar en la configuración

# Opción 2: Usar autenticación OAuth (sin API key)
unset GEMINI_API_KEY
gemini  # Seguir el proceso de autenticación web

# Opción 3: Verificar permisos de la API key
# Asegurarse de que la API key tenga acceso a Gemini API en Google AI Studio
```

### Error 10: Conflicto persistente de bibliotecas

**Error:**
```
libssl.so.3: version `OPENSSL_3.2.0' not found
```

**Solución permanente:**
```bash
# Crear script wrapper que limpia el entorno
cat > ~/gemini-clean << 'EOF'
#!/bin/bash
unset LD_LIBRARY_PATH
export GEMINI_API_KEY="TU_API_KEY_AQUI"
cd /home/pequeniomanjaro/Programas/Gemini-CLI-All
source bin/activate
gemini "$@"
EOF

chmod +x ~/gemini-clean

# Usar el wrapper
~/gemini-clean -p "tu pregunta"
```

---

## 🔧 Mantenimiento

### Actualizar Gemini CLI

```bash
# Activar entorno virtual
cd /home/pequeniomanjaro/Programas/Gemini-CLI-All
source bin/activate

# Actualizar a la última versión
npm update -g @google/gemini-cli

# Verificar nueva versión
gemini --version
```

### Actualizar el Repositorio Local

```bash
# Navegar al repositorio clonado
cd /home/pequeniomanjaro/Programas/Gemini-CLI-All/gemini-cli

# Actualizar desde GitHub
git pull origin main

# Si hay cambios en package.json, reinstalar dependencias
npm install
```

### Limpiar Instalación

```bash
# Desinstalar gemini-cli
npm uninstall -g @google/gemini-cli

# Limpiar caché de npm
npm cache clean --force

# Eliminar entorno virtual completo (si es necesario)
rm -rf /home/pequeniomanjaro/Programas/Gemini-CLI-All
```

### Backup de Configuración

```bash
# Crear backup de configuraciones importantes
mkdir ~/gemini-cli-backup

# Copiar configuraciones
cp ~/.bashrc ~/gemini-cli-backup/bashrc-backup
cp ~/.npmrc ~/gemini-cli-backup/npmrc-backup 2>/dev/null || echo "No .npmrc found"

# Exportar lista de paquetes npm globales
npm list -g --depth=0 > ~/gemini-cli-backup/npm-global-packages.txt
```

---

## 📚 Comandos de Referencia Rápida

### Inicio Rápido Diario

```bash
# 1. Activar entorno virtual
cd /home/pequeniomanjaro/Programas/Gemini-CLI-All && source bin/activate

# 2. Configurar API Key (si no está en .bashrc)
export GEMINI_API_KEY="AIzaSyAghQk7L3QaE7ZRMSfyMf5YUqjlsfCX0_k"

# 3. Usar Gemini CLI
gemini -p "Tu pregunta aquí"
```

### Scripts de Automatización

#### Script de Inicio Automático

```bash
# Crear script de inicio en el directorio del proyecto
cd /home/pequeniomanjaro/Programas/Gemini-CLI-All

cat > start-gemini.sh << 'EOF'
#!/bin/bash
echo "🚀 Iniciando Gemini CLI..."
cd /home/pequeniomanjaro/Programas/Gemini-CLI-All
source bin/activate
export GEMINI_API_KEY="AIzaSyAghQk7L3QaE7ZRMSfyMf5YUqjlsfCX0_k"
echo "✅ Entorno virtual activado"
echo "✅ API Key configurada"
echo "🎯 Gemini CLI listo para usar!"
echo ""
echo "Comandos útiles:"
echo "  gemini -p 'tu pregunta'           # Hacer una pregunta"
echo "  gemini --help                     # Ver ayuda"
echo "  gemini -a -p 'analiza el proyecto' # Analizar todos los archivos"
echo ""
bash
EOF

# Hacer ejecutable
chmod +x start-gemini.sh

# Usar el script
./start-gemini.sh
```

#### Script de Verificación del Sistema

```bash
# Crear script de verificación
cd /home/pequeniomanjaro/Programas/Gemini-CLI-All

cat > check-gemini.sh << 'EOF'
#!/bin/bash
echo "🔍 Verificando instalación de Gemini CLI..."
echo "----------------------------------------"

echo "✅ Python: $(python3 --version)"
echo "✅ Node.js: $(node --version)"
echo "✅ npm: $(npm --version)"
echo "✅ Git: $(git --version)"

if command -v gemini &> /dev/null; then
    echo "✅ Gemini CLI: Instalado ($(gemini --version))"
else
    echo "❌ Gemini CLI: No encontrado"
fi

if [ -n "$GEMINI_API_KEY" ]; then
    echo "✅ API Key: Configurada"
else
    echo "❌ API Key: No configurada"
fi

echo "----------------------------------------"
echo "🎯 Estado: $([ -n "$GEMINI_API_KEY" ] && command -v gemini &> /dev/null && echo "Listo para usar" || echo "Requiere configuración")"
EOF

chmod +x check-gemini.sh

# Ejecutar verificación
./check-gemini.sh
```

### Verificación del Sistema

```bash
# Script de verificación completa
cat > ~/check-gemini.sh << 'EOF'
#!/bin/bash
echo "🔍 Verificando instalación de Gemini CLI..."
echo "----------------------------------------"

echo "✅ Python: $(python3 --version)"
echo "✅ Node.js: $(node --version)"
echo "✅ npm: $(npm --version)"
echo "✅ Git: $(git --version)"

if command -v gemini &> /dev/null; then
    echo "✅ Gemini CLI: Instalado"
    gemini --version
else
    echo "❌ Gemini CLI: No encontrado"
fi

if [ -n "$GEMINI_API_KEY" ]; then
    echo "✅ API Key: Configurada"
else
    echo "❌ API Key: No configurada"
fi

echo "----------------------------------------"
echo "🎯 Estado: $([ -n "$GEMINI_API_KEY" ] && command -v gemini &> /dev/null && echo "Listo para usar" || echo "Requiere configuración")"
EOF

chmod +x ~/check-gemini.sh
```

---

## 🔐 Seguridad y Mejores Prácticas

### Protección de la Clave API

1. **Nunca compartas tu clave API** en repositorios públicos
2. **Usa archivos .env** para proyectos específicos
3. **Configura .gitignore** para excluir archivos con claves:

```bash
# Agregar a .gitignore
echo ".env" >> .gitignore
echo "*.key" >> .gitignore
```

### Límites de Uso

- **Límite gratuito:** 60 requests/minuto, 1,000 requests/día
- **Para uso intensivo:** Considera obtener una clave de Google Cloud Platform

---

## 📞 Soporte y Recursos

### Enlaces Útiles

- **Repositorio oficial:** https://github.com/google-gemini/gemini-cli
- **Google AI Studio:** https://aistudio.google.com/apikey
- **Documentación:** https://ai.google.dev/gemini-api/docs
- **Issues y bugs:** https://github.com/google-gemini/gemini-cli/issues

### Comunidad

- **GitHub Discussions:** Para preguntas y discusiones
- **Stack Overflow:** Tag `gemini-api`

---

## ✅ Checklist de Instalación Exitosa

- [ ] Python 3.13+ instalado
- [ ] Node.js 18+ instalado
- [ ] Git instalado
- [ ] Entorno virtual creado en `/home/pequeniomanjaro/Programas/Gemini-CLI-All`
- [ ] Repositorio clonado exitosamente
- [ ] Gemini CLI instalado globalmente
- [ ] API Key configurada: `AIzaSyAghQk7L3QaE7ZRMSfyMf5YUqjlsfCX0_k`
- [ ] Comando `gemini --help` funciona
- [ ] Prueba básica exitosa

---

**🎉 ¡Instalación Completada!**

Tu instalación de Gemini CLI está lista para usar. Recuerda activar el entorno virtual y configurar la API Key cada vez que abras una nueva terminal, o usa el script de inicio automático para mayor comodidad.

---

## 📖 Metodología de Creación de Ejercicios R-Exams

Para asegurar la calidad y consistencia en la creación de ejercicios de R-Exams, es fundamental seguir la metodología definida en el proyecto. Esta se basa en un conjunto de reglas y ejemplos funcionales que garantizan la correcta estructuración y funcionamiento de los ejercicios.

### 📜 Reglas y Protocolos

El documento `rules_full_v1.md` contiene el conjunto completo de metodologías, protocolos y criterios de calidad que deben seguirse. Los puntos clave incluyen:

- **Sistema Condicional Automático:** Detección de contenido gráfico en imágenes para activar flujos de trabajo específicos.
- **Metodología TikZ Avanzada:** Para la replicación precisa de gráficos y diagramas.
- **Protocolo Anti-Errores:** Consulta obligatoria de ejemplos funcionales para prevenir errores comunes de implementación.
- **Estructura Obligatoria del Archivo .Rmd:** Definición precisa del encabezado YAML, metadatos ICFES, y la estructura de chunks.

### 📂 Ejemplos Funcionales

La consulta de ejemplos funcionales es un paso **obligatorio** antes de escribir cualquier código. Estos ejemplos proporcionan plantillas probadas y funcionales que deben ser la base para cualquier nuevo ejercicio.

**Ruta de los Ejemplos:** `/home/pequeniomanjaro/Documentos/proyecto-r-exams-icfes-matematicas-optimizado/A-Produccion/Ejemplos-Funcionales-Rmd/`

**Regla de Oro:** "Si no está en los ejemplos funcionales, no improvises."

---

## 📋 Resumen de la Instalación Exitosa

### ✅ Lo que se instaló:

1. **Entorno Virtual Python:** `/home/pequeniomanjaro/Programas/Gemini-CLI-All`
2. **Gemini CLI:** Versión 0.1.4 (instalado globalmente)
3. **Repositorio:** Clonado desde GitHub en `gemini-cli/`
4. **Scripts de automatización:** `start-gemini.sh` y `check-gemini.sh`

### ✅ Configuración aplicada:

- **API Key:** `AIzaSyAghQk7L3QaE7ZRMSfyMf5YUqjlsfCX0_k`
- **Modelo por defecto:** gemini-2.5-pro (ya configurado)
- **Límites diarios:** 1,000 requests/día, 60 requests/minuto, 32,000 tokens/minuto
- **Límites por request:** 1,048,576 tokens entrada, 65,536 tokens salida
- **Auto-switch:** Habilitado entre gemini-2.5-pro y gemini-2.5-flash

### ✅ Errores resueltos durante la instalación:

1. **pip no encontrado** → Instalado con `pacman -S python-pip`
2. **Conflicto OpenSSL** → Resuelto con `unset LD_LIBRARY_PATH`
3. **Dependencias Node.js** → Verificadas (v22.16.0 compatible)

### 🚀 Comandos para uso diario:

```bash
# Inicio rápido
cd /home/pequeniomanjaro/Programas/Gemini-CLI-All && ./start-gemini.sh

# Verificación del sistema
cd /home/pequeniomanjaro/Programas/Gemini-CLI-All && ./check-gemini.sh

# Uso directo (después de activar entorno)
gemini -p "Tu pregunta aquí"
```

### 📁 Estructura final del proyecto:

```
/home/pequeniomanjaro/Programas/Gemini-CLI-All/
├── bin/                    # Entorno virtual Python
├── lib/                    # Bibliotecas Python
├── gemini-cli/            # Repositorio clonado
├── start-gemini.sh        # Script de inicio
├── check-gemini.sh        # Script de verificación
└── pyvenv.cfg             # Configuración del entorno virtual
```

---

**Fecha de creación:** Diciembre 2024  
**Versión del tutorial:** 1.0  
**Sistema:** Manjaro XFCE  
**Estado:** ✅ Instalación verificada y funcional