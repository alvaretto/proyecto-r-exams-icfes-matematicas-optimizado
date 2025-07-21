# Tutorial: Sistema de Sincronización Automática de Nombres de Archivos

## 📋 Descripción General

Este sistema permite mantener automáticamente sincronizados los nombres de archivos idénticos distribuidos en diferentes carpetas del proyecto. Cuando un archivo cambia de nombre, todos sus "hermanos" (archivos con el mismo nombre en otras ubicaciones) se renombran automáticamente.

## 🎯 Casos de Uso

### Ejemplo Práctico
```
Proyecto/
├── Lab-Lubuntu/01-ejercicio-matematicas.Rmd
├── Lab-Manjaro/01-ejercicio-matematicas.Rmd
└── Backup/01-ejercicio-matematicas.Rmd
```

Si renombras `Lab-Lubuntu/01-ejercicio-matematicas.Rmd` a `Lab-Lubuntu/02-ejercicio-avanzado.Rmd`, automáticamente:
- `Lab-Manjaro/01-ejercicio-matematicas.Rmd` → `Lab-Manjaro/02-ejercicio-avanzado.Rmd`
- `Backup/01-ejercicio-matematicas.Rmd` → `Backup/02-ejercicio-avanzado.Rmd`

## 🗂️ Archivos del Sistema

| Archivo | Descripción | Estado |
|---------|-------------|--------|
| `00-backup-sincro-tutorial.md` | **Este tutorial completo** | ✅ Creado |
| `01-sync-detector.sh` | Detecta archivos duplicados | ✅ Creado |
| `02-sync-monitor.sh` | Monitor en tiempo real | ✅ Creado |
| `03-sync-engine.sh` | Motor de sincronización | ✅ Creado |
| `04-sync-config.conf` | Configuración del sistema | ✅ Creado |
| `05-sync-database.txt` | Base de datos de grupos | ✅ Creado |
| `06-sync-service.sh` | Gestión como servicio systemd | ✅ Creado |
| `07-sync-utils.sh` | Utilidades de mantenimiento | ✅ Creado |
| `08-sync-manual.sh` | **Sincronización manual y selección de maestros** | ✅ Creado |
| `09-sync-relocate.sh` | **Detector de archivos reubicados** | ✅ Creado |

## 🔧 Componentes del Sistema

### 1. **01-sync-detector.sh** - Detector de Archivos Duplicados
- Escanea el proyecto buscando archivos con nombres idénticos
- Crea grupos de sincronización
- Genera base de datos de archivos vinculados

### 2. **02-sync-monitor.sh** - Monitor de Cambios
- Usa `inotify` para detectar cambios de nombres en tiempo real
- Ejecuta sincronización automática cuando detecta cambios
- Mantiene logs detallados de todas las operaciones

### 3. **03-sync-engine.sh** - Motor de Sincronización
- Ejecuta el renombrado sincronizado
- Maneja conflictos y errores
- Implementa sistema de locks para evitar bucles infinitos

### 4. **04-sync-config.conf** - Configuración
- Rutas a incluir/excluir del monitoreo
- Patrones de archivos a sincronizar
- Configuraciones de logging

### 5. **05-sync-database.txt** - Base de Datos
- Registro de grupos de archivos sincronizados
- Timestamps de última sincronización
- Estados de archivos

### 6. **06-sync-service.sh** - Gestión como Servicio
- Instala/desinstala el sistema como servicio systemd
- Gestión completa del servicio (start/stop/status)
- Logs centralizados con journalctl

### 7. **07-sync-utils.sh** - Utilidades de Mantenimiento
- Diagnóstico completo del sistema
- Limpieza de archivos temporales
- Backup y restauración del sistema
- Información detallada del estado

### 8. **08-sync-manual.sh** - Sincronización Manual (CRÍTICO)
- Comparación de fechas de modificación
- Identificación automática del archivo más reciente
- Sincronización controlada antes del modo automático
- Previene pérdida de datos por archivos desactualizados

### 9. **09-sync-relocate.sh** - Detector de Archivos Reubicados
- Detecta archivos que han cambiado de ubicación
- Busca automáticamente nuevas ubicaciones por nombre
- Actualiza la base de datos con las nuevas rutas
- Mantiene la sincronización después de mover archivos

## ⚠️ **PROBLEMA CRÍTICO: Archivos Desactualizados**

### El Dilema de la Sincronización Inicial

**PROBLEMA**: Si inicias el sistema automático sin preparación, podrías sincronizar desde un archivo obsoleto hacia archivos más recientes, **perdiendo trabajo importante**.

**SOLUCIÓN**: El script `08-sync-manual.sh` resuelve este problema permitiendo:

1. **Comparar fechas de modificación** entre archivos del mismo grupo
2. **Identificar automáticamente** el archivo más reciente
3. **Ver diferencias** entre archivos antes de sincronizar
4. **Seleccionar manualmente** qué archivo usar como maestro
5. **Sincronizar de forma controlada** antes de activar el automático

## 🚀 Guía de Inicio Rápido

### Paso 1: Verificar el Sistema
```bash
# Verificar que todos los archivos tienen permisos
ls -la *.sh

# Ejecutar diagnóstico del sistema
./07-sync-utils.sh diagnose
```

### Paso 2: Configuración Inicial
```bash
# Revisar y ajustar configuración si es necesario
nano 04-sync-config.conf

# Ejecutar detección inicial de archivos duplicados
./01-sync-detector.sh
```

### Paso 3: ⚠️ **CRÍTICO - Sincronización Manual Inicial**
```bash
# ANTES de activar el sistema automático, seleccionar archivos maestros
./08-sync-manual.sh interactive

# O comandos específicos:
./08-sync-manual.sh list                    # Ver todos los grupos
./08-sync-manual.sh compare 1               # Comparar grupo 1
./08-sync-manual.sh sync 1                  # Sincronizar grupo 1
```

### Paso 4: Iniciar Monitoreo Automático
```bash
# Solo DESPUÉS de la sincronización manual inicial
# Opción A: Ejecutar en background
./02-sync-monitor.sh start &

# Opción B: Instalar como servicio systemd (recomendado)
sudo ./06-sync-service.sh install
sudo ./06-sync-service.sh start
```

### Paso 5: Verificar Funcionamiento
```bash
# Ver estado del sistema
./07-sync-utils.sh info

# Ver logs en tiempo real
./06-sync-service.sh follow
```

## 🎯 Uso del Script de Sincronización Manual

### Menú Interactivo (Recomendado)
```bash
./08-sync-manual.sh interactive
```

El menú te permite:
1. **Listar todos los grupos** - Ver qué archivos están agrupados
2. **Comparar archivos** - Ver fechas, tamaños y cuál es más reciente
3. **Ver diferencias** - Comparar contenido entre archivos
4. **Sincronizar específico** - Elegir qué archivo usar como maestro
5. **Sincronizar todo** - Usar automáticamente el más reciente de cada grupo

### Comandos Específicos

#### Ver Grupos Disponibles
```bash
./08-sync-manual.sh list
```
Muestra todos los grupos detectados con sus archivos.

#### Comparar un Grupo Específico
```bash
./08-sync-manual.sh compare 1
```
Muestra información detallada del grupo 1:
- Fechas de modificación
- Tamaños de archivo
- Cuál es el más reciente
- Archivos faltantes

#### Ver Diferencias Entre Archivos
```bash
./08-sync-manual.sh diff 1
```
Compara el contenido de los archivos del grupo usando `diff`.

#### Sincronizar Grupo
```bash
# Usar el archivo más reciente como maestro
./08-sync-manual.sh sync 1

# Usar un archivo específico como maestro
./08-sync-manual.sh sync 1 /ruta/al/archivo/maestro.Rmd
```

#### Sincronizar Todos los Grupos
```bash
./08-sync-manual.sh sync-all
```
⚠️ **Cuidado**: Esto sincroniza TODOS los grupos usando el archivo más reciente de cada uno.

### Ejemplo de Uso Típico

```bash
# 1. Ver qué grupos existen
./08-sync-manual.sh list

# 2. Comparar un grupo específico
./08-sync-manual.sh compare 1

# Salida ejemplo:
# === GRUPO 1: ejercicio-matematicas.Rmd ===
# [1] .../Lab-Lubuntu/ejercicio-matematicas.Rmd
#     📅 Modificado: 2025-01-20 15:30:00
#     📏 Tamaño: 15KB
# [2] .../Lab-Manjaro/ejercicio-matematicas.Rmd
#     📅 Modificado: 2025-01-21 09:15:00  ← MÁS RECIENTE
#     📏 Tamaño: 18KB
# [3] .../Backup/ejercicio-matematicas.Rmd
#     📅 Modificado: 2025-01-19 14:20:00
#     📏 Tamaño: 12KB

# 3. Ver diferencias si es necesario
./08-sync-manual.sh diff 1

# 4. Sincronizar usando el más reciente
./08-sync-manual.sh sync 1
```

## 🔧 Manejo del Proceso

### Comandos Principales

#### Gestión del Detector
```bash
./01-sync-detector.sh                # Escanear archivos duplicados
./01-sync-detector.sh --status       # Ver estadísticas
./01-sync-detector.sh --rescan       # Re-escanear completamente
./01-sync-detector.sh --validate     # Validar base de datos
```

#### Gestión del Monitor
```bash
./02-sync-monitor.sh start           # Iniciar monitor
./02-sync-monitor.sh stop            # Detener monitor
./02-sync-monitor.sh status          # Ver estado
./02-sync-monitor.sh restart         # Reiniciar monitor
```

#### Gestión del Servicio (Recomendado)
```bash
sudo ./06-sync-service.sh install    # Instalar servicio
sudo ./06-sync-service.sh start      # Iniciar servicio
sudo ./06-sync-service.sh stop       # Detener servicio
sudo ./06-sync-service.sh status     # Ver estado
sudo ./06-sync-service.sh logs       # Ver logs
sudo ./06-sync-service.sh follow     # Seguir logs en tiempo real
```

#### Sincronización Manual (CRÍTICO)
```bash
./08-sync-manual.sh interactive      # Menú interactivo completo
./08-sync-manual.sh list             # Listar grupos disponibles
./08-sync-manual.sh compare 1        # Comparar archivos del grupo 1
./08-sync-manual.sh diff 1           # Ver diferencias entre archivos
./08-sync-manual.sh sync 1           # Sincronizar grupo 1 (usar más reciente)
./08-sync-manual.sh sync 1 /ruta     # Sincronizar desde archivo específico
./08-sync-manual.sh sync-all         # Sincronizar TODOS los grupos
```

#### Utilidades de Mantenimiento
```bash
./07-sync-utils.sh info              # Información del sistema
./07-sync-utils.sh diagnose          # Diagnóstico completo
./07-sync-utils.sh cleanup           # Limpiar archivos temporales
./07-sync-utils.sh backup            # Crear backup del sistema
```

#### Detector de Archivos Reubicados
```bash
./09-sync-relocate.sh interactive    # Menú interactivo para reubicaciones
./09-sync-relocate.sh detect         # Solo detectar archivos movidos
./09-sync-relocate.sh update         # Detectar y actualizar automáticamente
```

### Flujo de Trabajo Típico

1. **Instalación inicial:**
   ```bash
   ./07-sync-utils.sh diagnose
   ./01-sync-detector.sh
   # ⚠️ CRÍTICO: Sincronización manual ANTES del automático
   ./08-sync-manual.sh interactive
   sudo ./06-sync-service.sh install
   sudo ./06-sync-service.sh start
   ```

2. **Uso diario:**
   - El sistema funciona automáticamente
   - Renombra archivos normalmente
   - Los demás se sincronizan automáticamente

3. **Mantenimiento semanal:**
   ```bash
   ./07-sync-utils.sh cleanup
   ./01-sync-detector.sh --validate
   ```

4. **Monitoreo:**
   ```bash
   ./06-sync-service.sh status
   ./06-sync-service.sh logs 100
   ```

## 📊 Funcionamiento Interno

### Flujo de Trabajo
1. **Detección**: El sistema escanea periódicamente buscando archivos con nombres idénticos
2. **Agrupación**: Crea grupos de archivos que deben mantenerse sincronizados
3. **Monitoreo**: `inotify` detecta cuando un archivo cambia de nombre
4. **Sincronización**: Automáticamente renombra todos los archivos del mismo grupo
5. **Logging**: Registra todas las operaciones para auditoría

### Prevención de Conflictos
- **Sistema de Locks**: Evita renombrados simultáneos
- **Validación**: Verifica que el nuevo nombre no cause conflictos
- **Rollback**: Capacidad de deshacer cambios problemáticos
- **Timeouts**: Evita operaciones que se cuelguen

## 📊 Monitoreo y Logs

### Archivos de Log
- `sync-detector.log` - Detección de archivos duplicados
- `sync-monitor.log` - Monitor en tiempo real
- `sync-operations.log` - Operaciones de sincronización
- `sync-errors.log` - Errores y advertencias
- `sync-manual.log` - Operaciones manuales

### Comandos de Monitoreo
```bash
# Ver logs recientes
tail -f sync-monitor.log

# Ver errores
cat sync-errors.log

# Estadísticas del sistema
./01-sync-detector.sh --status

# Estado del servicio
sudo systemctl status sync-monitor

# Seguir logs del servicio en tiempo real
sudo journalctl -u sync-monitor -f
```

## 🛡️ Características de Seguridad

### Protecciones Implementadas
- ✅ **Sistema de locks** - Evita conflictos simultáneos
- ✅ **Backups automáticos** - Antes de cada sincronización
- ✅ **Validación de rutas** - Solo opera dentro del proyecto
- ✅ **Logs detallados** - Auditoría completa
- ✅ **Modo dry-run** - Probar sin ejecutar cambios
- ✅ **Sincronización manual inicial** - Evita pérdida de datos
- ✅ **Anti-bucles**: Previene renombrados infinitos
- ✅ **Validación de rutas**: Solo opera dentro del proyecto definido
- ✅ **Backup automático**: Crea respaldos antes de cambios críticos

### Exclusiones Automáticas
- Archivos del sistema (`.git/`, `node_modules/`, etc.)
- Archivos temporales (`.tmp`, `.swp`, etc.)
- Archivos de configuración críticos

### Modo de Prueba (Dry Run)
```bash
# Probar sincronización sin ejecutar
./03-sync-engine.sh --dry-run --sync-rename "ruta/antigua" "ruta/nueva"
```

## 📁 Manejo de Archivos Reubicados

### ⚠️ **Problema: Archivos Movidos de Ubicación**

Si mueves un archivo a otra carpeta, el sistema pierde la sincronización:

```bash
# Antes:
Lab-Lubuntu/archivo.Rmd     ← En grupo de sincronización
Lab-Manjaro/archivo.Rmd     ← En grupo de sincronización

# Después de mover:
mv Lab-Lubuntu/archivo.Rmd Nueva-Carpeta/archivo.Rmd

# Resultado:
Nueva-Carpeta/archivo.Rmd   ← YA NO sincroniza
Lab-Manjaro/archivo.Rmd     ← Sigue sincronizando solo con otros
```

### ✅ **Solución: Script de Reubicación**

#### Uso Interactivo (Recomendado)
```bash
./09-sync-relocate.sh interactive
```

El script:
1. **Detecta archivos faltantes** en la base de datos
2. **Busca automáticamente** archivos con el mismo nombre en nuevas ubicaciones
3. **Muestra las reubicaciones detectadas** para confirmación
4. **Actualiza la base de datos** con las nuevas rutas
5. **Verifica la integridad** después de la actualización

#### Comandos Específicos
```bash
# Solo detectar archivos movidos
./09-sync-relocate.sh detect

# Detectar y actualizar automáticamente
./09-sync-relocate.sh update
```

### 📋 **Flujo Típico Después de Mover Archivos**

```bash
# 1. Detectar qué archivos se movieron
./09-sync-relocate.sh detect

# 2. Actualizar base de datos interactivamente
./09-sync-relocate.sh interactive

# 3. Verificar que todo esté correcto
./01-sync-detector.sh --validate

# 4. Opcional: Re-escanear completamente
./01-sync-detector.sh --rescan
```

### 🔄 **Alternativas Rápidas**

#### Re-escaneo Completo (Más Simple)
```bash
# Recrear toda la base de datos
./01-sync-detector.sh --rescan
```

#### Limpieza y Reorganización
```bash
# Limpiar grupos rotos
./07-sync-utils.sh cleanup

# Sincronización manual para reorganizar
./08-sync-manual.sh interactive
```

## 🔍 Resolución de Problemas

### Problemas Comunes

**1. El monitor no detecta cambios**
```bash
# Verificar dependencias
./07-sync-utils.sh diagnose

# Reinstalar dependencias si es necesario
sudo apt-get install inotify-tools

# Verificar que inotify esté funcionando
inotifywait -m /ruta/del/proyecto

# Revisar límites del sistema
cat /proc/sys/fs/inotify/max_user_watches
```

**2. Archivos no se sincronizan**
```bash
# Verificar que estén en la base de datos
grep "nombre-archivo" 05-sync-database.txt

# Re-ejecutar detección
./01-sync-detector.sh --rescan

# Verificar configuración
source 04-sync-config.conf && echo "Configuración OK"
```

**3. Servicio no inicia**
```bash
# Ver logs del servicio
sudo journalctl -u sync-monitor -f

# Verificar permisos
ls -la *.sh

# Reinstalar servicio
sudo ./06-sync-service.sh uninstall
sudo ./06-sync-service.sh install
```

**4. Conflictos de nombres**
```bash
# Ver estado de grupos
./01-sync-detector.sh --status

# Limpiar sistema
./07-sync-utils.sh cleanup

# Sincronización manual para resolver
./08-sync-manual.sh interactive
```

### Comandos de Diagnóstico
```bash
# Diagnóstico completo
./07-sync-utils.sh diagnose

# Información del sistema
./07-sync-utils.sh info

# Verificar configuración
source 04-sync-config.conf && echo "Configuración OK"
```

## 🔄 Backup y Recuperación

### Crear Backups
```bash
# Backup automático del sistema
./07-sync-utils.sh backup

# Backup con nombre específico
./07-sync-utils.sh backup "mi-backup-importante"
```

### Restaurar desde Backup
```bash
# Listar backups disponibles
ls system-backups/

# Restaurar desde backup específico
./07-sync-utils.sh restore system-backups/backup-20250121.tar.gz
```

## 🔍 Resolución de Problemas

### Problemas Comunes

**1. El monitor no detecta cambios**
```bash
# Verificar que inotify esté funcionando
inotifywait -m /ruta/del/proyecto

# Revisar límites del sistema
cat /proc/sys/fs/inotify/max_user_watches
```

**2. Archivos no se sincronizan**
```bash
# Verificar que estén en la base de datos
grep "nombre-archivo" 05-sync-database.txt

# Re-ejecutar detección
./01-sync-detector.sh --force
```

**3. Conflictos de nombres**
```bash
# Ver conflictos pendientes
./03-sync-engine.sh --check-conflicts

# Resolver manualmente
./03-sync-engine.sh --resolve-conflict "grupo-id"
```

## 📋 Configuración Avanzada

### Personalizar Patrones de Archivos
Editar `04-sync-config.conf`:
```bash
# Agregar nuevos tipos de archivo
INCLUDE_PATTERNS+=(
    "*.tex"     # Archivos LaTeX
    "*.docx"    # Documentos Word
)

# Excluir directorios específicos
EXCLUDE_DIRS+=(
    "mi-directorio-especial"
)
```

### Configurar Notificaciones
```bash
# Habilitar notificaciones del sistema
ENABLE_NOTIFICATIONS=true

# Configurar email para errores críticos
ADMIN_EMAIL="tu-email@ejemplo.com"
```

## ✅ Verificación Final

### Lista de Verificación
- [ ] Todos los scripts tienen permisos de ejecución
- [ ] Diagnóstico del sistema sin errores
- [ ] Base de datos creada con archivos detectados
- [ ] **⚠️ CRÍTICO: Sincronización manual inicial completada**
- [ ] Verificado que archivos maestros son los correctos
- [ ] Probado el detector de archivos reubicados
- [ ] Monitor funcionando (servicio o background)
- [ ] Logs generándose correctamente
- [ ] Prueba de sincronización automática exitosa

### Comando de Verificación Completa
```bash
# Ejecutar verificación completa
./07-sync-utils.sh diagnose && \
./01-sync-detector.sh --status && \
./06-sync-service.sh status
```

## 🎯 Casos de Uso Específicos

### Para Archivos R Markdown
```bash
# El sistema detecta automáticamente:
Lab-Lubuntu/ejercicio-01.Rmd
Lab-Manjaro/ejercicio-01.Rmd
Backup/ejercicio-01.Rmd

# Al renombrar uno, todos se sincronizan
```

### Para Scripts de R
```bash
# Funciona con cualquier extensión configurada:
Scripts/analisis.R
Backup-Scripts/analisis.R
Lab-Test/analisis.R
```

## 🎉 ¡Sistema Listo!

Tu idea de sincronización automática está completamente implementada y lista para usar. El sistema:

1. ✅ **Detecta automáticamente** archivos con nombres idénticos
2. ✅ **Monitorea en tiempo real** cambios de nombres
3. ✅ **Sincroniza automáticamente** todos los archivos del grupo
4. ✅ **Mantiene logs detallados** de todas las operaciones
5. ✅ **Incluye protecciones** contra errores y conflictos
6. ✅ **Permite sincronización manual inicial** para evitar pérdida de datos

### Flujo Seguro de Inicialización

```bash
# 1. Detectar grupos de archivos
./01-sync-detector.sh

# 2. CRÍTICO: Revisar y sincronizar manualmente
./08-sync-manual.sh interactive

# 3. Solo entonces activar el sistema automático
sudo ./06-sync-service.sh install
sudo ./06-sync-service.sh start
```

**¡Ahora puedes renombrar un archivo y ver cómo todos sus "hermanos" se sincronizan automáticamente!** 🚀

---

**Nota**: Este sistema está diseñado específicamente para el proyecto RepositorioMatematicasICFES_R_Exams y puede adaptarse a otros proyectos similares.
