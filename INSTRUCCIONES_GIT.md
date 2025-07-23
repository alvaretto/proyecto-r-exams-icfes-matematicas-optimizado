# Instrucciones para Sincronización con GitHub

## Configuración Completada ✅

Tu repositorio ha sido configurado exitosamente para trabajar con GitHub usando tu token personal. Ya no necesitarás ingresar usuario y contraseña en cada operación.

### Configuración Aplicada:
- **Usuario**: alvaretto
- **Email**: alvaroangelm@gmail.com
- **Token**: Configurado en la URL del repositorio remoto
- **Repositorio**: https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado
- **Rama**: experimentos-seguros

## Scripts Disponibles

### 1. `actualizar_repositorio.sh` 📥
**Propósito**: Descargar y aplicar los últimos cambios del repositorio remoto

**Uso**:
```bash
./actualizar_repositorio.sh
```

**Qué hace**:
- Verifica que estés en la rama correcta (experimentos-seguros)
- Descarga los cambios del repositorio remoto
- Muestra un resumen de los nuevos commits
- Aplica los cambios a tu repositorio local
- Proporciona un reporte detallado de la actualización

### 2. `subir_cambios.sh` 📤
**Propósito**: Subir tus cambios locales al repositorio remoto

**Uso**:
```bash
./subir_cambios.sh
```

**Qué hace**:
- Detecta cambios en tu repositorio local
- Te permite revisar los cambios antes de subirlos
- Solicita un mensaje para el commit
- Actualiza desde el remoto para evitar conflictos
- Sube tus cambios al repositorio remoto

## Comandos Git Básicos (Alternativos)

Si prefieres usar comandos Git directamente:

### Actualizar repositorio:
```bash
git fetch origin
git merge origin/experimentos-seguros
```

### Subir cambios:
```bash
git add .
git commit -m "Tu mensaje de commit"
git push origin experimentos-seguros
```

### Ver estado:
```bash
git status
```

### Ver historial:
```bash
git log --oneline -10
```

## Flujo de Trabajo Recomendado

### Para Actualizar (Recibir cambios):
1. Ejecuta `./actualizar_repositorio.sh`
2. Revisa el resumen de cambios
3. Continúa con tu trabajo

### Para Subir Cambios:
1. Realiza tus modificaciones en los archivos
2. Ejecuta `./subir_cambios.sh`
3. Revisa los cambios si es necesario
4. Proporciona un mensaje descriptivo para el commit
5. Confirma la subida

## Resolución de Problemas

### Error de Conectividad:
- Verifica tu conexión a internet
- Comprueba que GitHub esté accesible: `ping github.com`

### Error de Token:
Si el token expira o hay problemas de autenticación:
```bash
git remote set-url origin https://NUEVO_TOKEN@github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado.git
```

### Conflictos de Merge:
Si hay conflictos al actualizar:
1. Git te mostrará los archivos en conflicto
2. Edita los archivos para resolver los conflictos
3. Ejecuta: `git add .` y luego `git commit`

### Verificar Configuración:
```bash
git remote -v
git config user.name
git config user.email
```

## Seguridad del Token

⚠️ **IMPORTANTE**: 
- Tu token está configurado en la URL del repositorio
- No compartas esta información con otros
- Si necesitas regenerar el token, hazlo desde GitHub y actualiza la configuración

## Estado Actual

✅ **Repositorio actualizado**: Tu repositorio local está sincronizado con la rama `experimentos-seguros`
✅ **Scripts listos**: Los scripts de automatización están configurados y listos para usar
✅ **Token configurado**: No necesitarás ingresar credenciales en futuras operaciones

## Próximos Pasos

1. **Para trabajar normalmente**: Usa los scripts `actualizar_repositorio.sh` y `subir_cambios.sh`
2. **Para verificar estado**: Ejecuta `git status` cuando tengas dudas
3. **Para ver cambios**: Usa `git log --oneline` para ver el historial

---

**Fecha de configuración**: $(date)
**Configurado por**: Augment Agent
**Repositorio**: RepositorioMatematicasICFES_R_Exams
