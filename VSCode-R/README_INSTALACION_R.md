# 🚀 Instalación Rápida de R + VSCode

## ⚡ Instalación Automática (Recomendada)

```bash
# Ejecutar script automático
./instalar_r_vscode.sh
```

## 📋 Instalación Manual

Si prefieres hacerlo paso a paso, sigue el tutorial completo:
👉 **[TUTORIAL_INSTALACION_R_VSCODE.md](TUTORIAL_INSTALACION_R_VSCODE.md)**

## 🔧 Archivos Incluidos

| Archivo | Descripción |
|---------|-------------|
| `TUTORIAL_INSTALACION_R_VSCODE.md` | Tutorial completo paso a paso |
| `instalar_r_vscode.sh` | Script de instalación automática |
| `README_INSTALACION_R.md` | Este archivo (instrucciones rápidas) |

## ✅ Verificación Rápida

Después de la instalación, verifica que todo funciona:

```bash
# Ejecutar script de verificación
Rscript verificar_instalacion.R

# Abrir VSCode
code .
```

## 🎯 Para Proyectos r-exams

Una vez instalado, puedes trabajar directamente con r-exams:

```r
# Cargar paquetes
load_common_packages()

# Crear un examen básico
library(exams)
exams2pdf("mi_ejercicio.Rmd")
```

## 🆘 Solución de Problemas

### Error común: "R Tools client: couldn't create connection"
✅ **Ya solucionado** en la configuración automática

### Paquetes no encontrados
```bash
R -e "install.packages('nombre_paquete', lib='~/R/library')"
```

### Python no detectado
```r
reticulate::use_python("/usr/bin/python3")
```

## 📞 Soporte

Si encuentras problemas:
1. Revisa el tutorial completo
2. Ejecuta el script de verificación
3. Verifica los logs de instalación

---

**Nota:** Optimizado para Manjaro/Arch Linux. Para otras distribuciones, adapta los comandos según corresponda.
