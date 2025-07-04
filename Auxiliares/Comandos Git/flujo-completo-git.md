# 🎯 Flujo Completo: Sincronización Bidireccional Forzosa

## 📁 Una Sola Carpeta: `~/Documentos/proyecto-r-exams-icfes-matematicas-optimizado/`

---

## 🔄 RAMA MAIN

### 📥 Sincronizar NUBE → LOCAL (forzoso):
```bash
git checkout main
git fetch origin
git reset --hard origin/main
git clean -fd
```
**Resultado:** Tu `main` local queda IDÉNTICO a `origin/main`

### 📤 Sincronizar LOCAL → NUBE (forzoso):
```bash
git checkout main
# Hacer tus cambios...
git add .
git commit -m "Cambios en main"
git push --force-with-lease origin main
```
**Resultado:** `origin/main` queda IDÉNTICO a tu `main` local

---

## 🧪 RAMA EXPERIMENTOS-SEGUROS

### 📥 Sincronizar NUBE → LOCAL (forzoso):
```bash
git checkout experimentos-seguros
git fetch origin
git reset --hard origin/experimentos-seguros
git clean -fd
```
**Resultado:** Tu `experimentos-seguros` local queda IDÉNTICO a `origin/experimentos-seguros`

### 📤 Sincronizar LOCAL → NUBE (forzoso):
```bash
git checkout experimentos-seguros
# Hacer tus experimentos...
git add .
git commit -m "Experimentos arriesgados"
git push --force-with-lease origin experimentos-seguros
```
**Resultado:** `origin/experimentos-seguros` queda IDÉNTICO a tu `experimentos-seguros` local

---

## 🔄 FLUJO COMPLETO DE TRABAJO DIARIO

### Escenario 1: Empezar el día (sincronizar todo desde nube)
```bash
# Sincronizar main
git checkout main
git fetch origin && git reset --hard origin/main && git clean -fd

# Sincronizar experimentos
git checkout experimentos-seguros
git fetch origin && git reset --hard origin/experimentos-seguros && git clean -fd

# Ahora tienes ambas ramas actualizadas desde la nube
```

### Escenario 2: Trabajar en main
```bash
git checkout main
# Editar archivos...
git add .
git commit -m "Mejoras en main"
git push  # Push normal (o --force-with-lease si hay conflictos)
```

### Escenario 3: Trabajar en experimentos
```bash
git checkout experimentos-seguros
# Hacer experimentos arriesgados...
git add .
git commit -m "Experimento: nueva funcionalidad"
git push  # Push normal (o --force-with-lease si hay conflictos)
```

### Escenario 4: Terminar el día (subir todo a la nube)
```bash
# Subir cambios de main
git checkout main
git add . && git commit -m "Trabajo del día en main" && git push

# Subir cambios de experimentos
git checkout experimentos-seguros
git add . && git commit -m "Experimentos del día" && git push
```

---

## ⚠️ COMANDOS DE EMERGENCIA (Forzar Sincronización)

### 🚨 Forzar LOCAL → NUBE (sobrescribir nube completamente):
```bash
# Para main
git checkout main
git push --force origin main

# Para experimentos
git checkout experimentos-seguros
git push --force origin experimentos-seguros
```

### 🚨 Forzar NUBE → LOCAL (sobrescribir local completamente):
```bash
# Para main
git checkout main
git fetch origin && git reset --hard origin/main && git clean -fd

# Para experimentos
git checkout experimentos-seguros
git fetch origin && git reset --hard origin/experimentos-seguros && git clean -fd
```

---

## 📋 COMANDOS DE VERIFICACIÓN

### Ver estado de todas las ramas:
```bash
git fetch origin                    # Actualizar info de ramas remotas
git branch -a                       # Ver todas las ramas
git status                          # Ver estado de rama actual
```

### Comparar ramas:
```bash
git diff main experimentos-seguros  # Ver diferencias entre ramas
git log --oneline --graph --all     # Ver historial visual de todas las ramas
```

### Ver en qué rama estás:
```bash
git branch                          # El asterisco (*) marca la rama activa
```

---

## 🎯 REGLAS DE ORO

1. **Siempre `git checkout` antes de sincronizar**
2. **Cada rama se sincroniza con SU contraparte:**
   - `main` ↔ `origin/main`
   - `experimentos-seguros` ↔ `origin/experimentos-seguros`
3. **`--force-with-lease` es más seguro que `--force`**
4. **`git fetch origin` antes de cualquier reset**

---

## 💡 NOTAS IMPORTANTES

- **Una sola carpeta**: No necesitas clonar el repositorio múltiples veces
- **Cambio de contexto**: `git checkout` cambia toda la carpeta al estado de esa rama
- **Sincronización forzosa**: Los comandos con `--hard` y `--force` sobrescriben completamente
- **Seguridad**: `--force-with-lease` es más seguro que `--force` simple



## Desde otro computador

Otro computador que ya tiene rama principal

## **🔄 Pasos para Acceder a experimentos-seguros**

### **1. Actualizar información de ramas remotas:**
```bash
git fetch origin
```

### **2. Ver todas las ramas disponibles:**
```bash
git branch -a
```

### **3. Cambiar a experimentos-seguros:**
```bash
git checkout experimentos-seguros
```

### **4. Sincronizar con la nube:**
```bash
git reset --hard origin/experimentos-seguros
git clean -fd
```



---

## 🚀 OPTIMIZACIONES Y AUTOMATIZACIÓN

### 📦 Scripts Automatizados Disponibles

#### 1. [`git_commit_smart.sh`](./Optimizacion_Flujo_Git/git_commit_smart.sh) - Commit Inteligente
```bash
./git_commit_smart.sh
```
**Características:**

- Commit semántico con tipos (feat, fix, docs, refactor, test, chore)
- Validación de archivos .Rmd y detección de archivos grandes
- Mensajes estructurados con alcance y descripción detallada
- Push automático opcional con `--force-with-lease`
- Manejo inteligente de ramas

#### 2. [`sync_repo.sh`](./Optimizacion_Flujo_Git/sync_repo.sh) - Sincronización Completa
```bash
./sync_repo.sh
```
**Características:**

- Fetch automático y verificación de cambios remotos
- Manejo inteligente de conflictos con stash temporal
- Integración con script de commit inteligente
- Push seguro con verificación de rama
- Resumen completo del estado final

#### 3. [`setup_git_lfs.sh`](./Optimizacion_Flujo_Git/setup_git_lfs.sh) - Configuración Git LFS
```bash
./setup_git_lfs.sh
```
**Características:**

- Instalación automática de Git LFS según el sistema operativo
- Configuración de tracking para archivos grandes (PDF, imágenes, datos R)
- Migración opcional de archivos existentes
- Soluciona advertencias de GitHub sobre archivos >50MB

---

## 🎯 FLUJO DE TRABAJO OPTIMIZADO

### Workflow Diario Recomendado:

#### 🌅 Inicio del día:
```bash
./sync_repo.sh  # Sincronización completa automática
```

#### 💻 Durante el trabajo:
```bash
# Hacer cambios en archivos...
./git_commit_smart.sh  # Commit inteligente con push opcional
```

#### 🌙 Final del día:
```bash
./sync_repo.sh  # Verificar sincronización final
```

### Configuración Inicial (una sola vez):
```bash
# 1. Configurar Git LFS para archivos grandes
./setup_git_lfs.sh

# 2. Configurar aliases útiles
git config --global alias.commit-smart '!bash ./git_commit_smart.sh'
git config --global alias.sync-repo '!bash ./sync_repo.sh'
git config --global alias.push-safe 'push --force-with-lease origin experimentos-seguros'
```

---

## 📊 COMMITS SEMÁNTICOS

### Formato Estándar:
```
<tipo>(<alcance>): <descripción>

<descripción detallada opcional>
- Detalle 1
- Detalle 2
```

### Tipos de Commit:

- **feat**: Nueva funcionalidad (nuevos ejercicios, metodologías)
- **fix**: Corrección de errores (TikZ, compilación, datos)
- **docs**: Documentación (README, guías, metodologías)
- **refactor**: Refactoring (optimización de código)
- **test**: Pruebas (validación, testing)
- **chore**: Mantenimiento (configuración, limpieza)

### Alcances Recomendados:

- **ejercicios**: Nuevos ejercicios, modificaciones
- **tikz**: Gráficos, visualizaciones
- **metodologia**: Metodologías TikZ, corrección errores
- **docs**: Documentación, guías
- **config**: Configuración, scripts
- **general**: Cambios múltiples

### Ejemplos:
```bash
feat(ejercicios): agregar nuevos ejercicios de estadística

- Añadidos 5 ejercicios de variables cualitativas
- Implementada aleatorización avanzada (300+ versiones)
- Validación con metodología TikZ integrada

fix(tikz): corregir posicionamiento de elementos

- Solucionado orden incorrecto texto → tabla → pregunta
- Aplicada metodología de corrección de errores categoría B
- Tiempo de corrección: < 3 minutos
```

---

## 🔧 CONFIGURACIONES AVANZADAS

### Git LFS para Archivos Grandes:
```bash
# Tipos de archivos configurados automáticamente:
*.pdf *.png *.jpg *.jpeg *.gif *.bmp *.tiff *.svg
*.rds *.RData *.rda
*.zip *.tar.gz *.rar *.7z
*.docx *.pptx *.xlsx
*.mp4 *.avi *.mov
*.log test_*.pdf *_output.html
```

### Aliases Útiles:
```bash
git st          # git status
git co          # git checkout
git br          # git branch
git ci          # git commit
git unstage     # git reset HEAD --
git last        # git log -1 HEAD
git commit-smart # ./git_commit_smart.sh
git sync-repo   # ./sync_repo.sh
git push-safe   # git push --force-with-lease origin experimentos-seguros
```

---

## 📋 COMANDOS DE VERIFICACIÓN AVANZADOS

### Estado Completo del Repositorio:
```bash
# Información completa
git fetch origin && git status && git log --oneline --graph --all | head -10

# Archivos más modificados
git log --pretty=format: --name-only | sort | uniq -c | sort -rg | head -10

# Tamaño del repositorio
du -sh .git/

# Archivos en Git LFS
git lfs ls-files | head -10
```

### Estadísticas del Proyecto:
```bash
# Commits por tipo
git log --oneline | grep -E '^[a-f0-9]+ (feat|fix|docs|refactor|test|chore)' | cut -d' ' -f2 | cut -d'(' -f1 | sort | uniq -c

# Actividad por fecha
git log --pretty=format:"%ad" --date=short | sort | uniq -c | tail -10
```

---

## ⚠️ COMANDOS DE EMERGENCIA ACTUALIZADOS

### 🚨 Reset Completo con Limpieza:
```bash
# Para main
git checkout main
git fetch origin && git reset --hard origin/main && git clean -fd
git lfs pull  # Descargar archivos LFS

# Para experimentos-seguros
git checkout experimentos-seguros
git fetch origin && git reset --hard origin/experimentos-seguros && git clean -fd
git lfs pull  # Descargar archivos LFS
```

### 🚨 Reparar Repositorio Corrupto:
```bash
# Verificar integridad
git fsck --full

# Reparar referencias
git reflog expire --expire=now --all
git gc --prune=now --aggressive

# Reconfigurar Git LFS si es necesario
git lfs install --force
```

---

## 💡 NOTAS IMPORTANTES ACTUALIZADAS

### Archivos Grandes:

- **Git LFS configurado**: Archivos >50MB se manejan automáticamente
- **Sin advertencias de GitHub**: Los archivos grandes no afectan el rendimiento
- **Descarga selectiva**: Solo se descargan cuando se necesitan

### Scripts Automatizados:

- **Ubicación**: `~/Proyectos/proyecto-r-exams-icfes-matematicas-optimizado/Auxiliares/Comandos Git/Optimizacion_Flujo_Git/`
- **Permisos**: Ejecutables (`chmod +x *.sh`)
- **Integración**: Funcionan con aliases de Git

### Seguridad:

- **`--force-with-lease`**: Siempre preferido sobre `--force`
- **Verificación de rama**: Los scripts verifican la rama activa
- **Backup automático**: Stash temporal durante sincronización

### Rendimiento:

- **Commits más pequeños**: Fragmentación inteligente
- **Mensajes estructurados**: Mejor trazabilidad
- **Sincronización eficiente**: Menos conflictos

---

## 🎯 REGLAS DE ORO ACTUALIZADAS

1. **Usar scripts automatizados** para operaciones comunes
2. **Commits semánticos** con tipos y alcances claros
3. **Git LFS** para todos los archivos grandes
4. **`--force-with-lease`** siempre en lugar de `--force`
5. **Sincronización diaria** con `./sync_repo.sh`
6. **Verificación de rama** antes de operaciones críticas
7. **Mensajes descriptivos** con contexto del proyecto R-exams ICFES

---

## 📚 RECURSOS ADICIONALES

### Documentación:

- [Optimización completa](./Optimizacion_Flujo_Git/OPTIMIZACION_FLUJO_GIT.md)
- [Git LFS Documentation](https://git-lfs.github.io/)
- [Conventional Commits](https://www.conventionalcommits.org/)

### Scripts de Apoyo:

- [`git_commit_smart.sh`](./Optimizacion_Flujo_Git/git_commit_smart.sh) - Commit inteligente
- [`sync_repo.sh`](./Optimizacion_Flujo_Git/sync_repo.sh) - Sincronización completa  
- [`setup_git_lfs.sh`](./Optimizacion_Flujo_Git/setup_git_lfs.sh) - Configuración Git LFS

*Flujo optimizado y automatizado - Enero 2025*
