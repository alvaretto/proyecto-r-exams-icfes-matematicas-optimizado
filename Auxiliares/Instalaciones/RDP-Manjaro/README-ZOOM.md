# 📹 Documentación de Instalación de Zoom para Manjaro Linux

Bienvenido a la documentación completa para instalar Zoom en Manjaro Linux.

---

## 📚 Índice de Documentos

### 🚀 Inicio Rápido
1. **[ZOOM-INSTALACION-RAPIDA.md](ZOOM-INSTALACION-RAPIDA.md)**
   - Guía de instalación rápida
   - Comandos esenciales
   - Solución rápida de problemas
   - **Recomendado para:** Usuarios que quieren instalar Zoom rápidamente

### 🎯 Guía de Decisión
2. **[ZOOM-DECISION-GUIDE.md](ZOOM-DECISION-GUIDE.md)**
   - Comparación detallada: Flatpak vs AUR
   - Escenarios de uso
   - Matriz de decisión
   - **Recomendado para:** Usuarios que no saben qué método elegir

### 📖 Documentación Completa
3. **[RDP-Manjaro.md](RDP-Manjaro.md)** (Sección Zoom)
   - Análisis exhaustivo de opciones
   - Instalación paso a paso (ambos métodos)
   - Solución de problemas detallada
   - Configuración avanzada
   - **Recomendado para:** Usuarios que quieren entender todo el proceso

### 🤖 Script de Instalación
4. **[install-zoom.sh](install-zoom.sh)**
   - Script automatizado de instalación
   - Interfaz interactiva
   - Verificación automática de requisitos
   - Configuración post-instalación
   - **Recomendado para:** Instalación automatizada y sin errores

---

## 🎯 ¿Por Dónde Empezar?

### Si eres nuevo en Linux:
1. Lee **ZOOM-INSTALACION-RAPIDA.md**
2. Ejecuta **install-zoom.sh**
3. Selecciona opción 1 (Flatpak)

### Si tienes experiencia con Manjaro/Arch:
1. Lee **ZOOM-DECISION-GUIDE.md**
2. Decide entre Flatpak o AUR
3. Ejecuta **install-zoom.sh** o instala manualmente

### Si quieres entender todo el proceso:
1. Lee la sección de Zoom en **RDP-Manjaro.md**
2. Revisa **ZOOM-DECISION-GUIDE.md**
3. Instala usando el método que prefieras

---

## 📋 Resumen de Métodos de Instalación

### Método 1: Flatpak (Recomendado) 🟢

**Ventajas:**
- ✅ Más seguro (sandboxing)
- ✅ Más estable
- ✅ Fácil de instalar/desinstalar
- ✅ Actualizaciones automáticas

**Instalación:**
```bash
flatpak install flathub us.zoom.Zoom
```

**Ejecutar:**
```bash
flatpak run us.zoom.Zoom
```

---

### Método 2: AUR (Alternativa) 🟡

**Ventajas:**
- ✅ Mejor rendimiento
- ✅ Integración nativa
- ✅ Menos espacio en disco
- ✅ Acceso completo al hardware

**Instalación:**
```bash
yay -S zoom
```

**Ejecutar:**
```bash
zoom
```

---

## 🛠️ Herramientas Disponibles

### Script de Instalación Automatizada
```bash
chmod +x install-zoom.sh
./install-zoom.sh
```

El script te guiará paso a paso y:
- ✅ Verifica requisitos del sistema
- ✅ Instala dependencias necesarias
- ✅ Configura permisos
- ✅ Verifica la instalación
- ✅ Muestra instrucciones de uso

---

## 📊 Comparación Rápida

| Característica | Flatpak | AUR |
|----------------|---------|-----|
| **Seguridad** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ |
| **Rendimiento** | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| **Facilidad** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ |
| **Espacio** | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| **Estabilidad** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ |

---

## 🔧 Solución Rápida de Problemas

### Cámara/Micrófono no funciona (Flatpak)
```bash
flatpak override us.zoom.Zoom --device=all
```

### Compartir pantalla no funciona
```bash
sudo pacman -S xdg-desktop-portal-kde
systemctl --user restart xdg-desktop-portal
```

### Audio no funciona (AUR)
```bash
sudo pacman -S pulseaudio-alsa
```

### Pantalla borrosa en HiDPI
```bash
# Flatpak
flatpak override us.zoom.Zoom --env=QT_AUTO_SCREEN_SCALE_FACTOR=1

# AUR
export QT_AUTO_SCREEN_SCALE_FACTOR=1
```

---

## ✅ Checklist de Verificación

Después de instalar Zoom, verifica:

- [ ] Zoom se abre correctamente
- [ ] Cámara funciona (Configuración → Video)
- [ ] Micrófono funciona (Configuración → Audio)
- [ ] Altavoces funcionan (Configuración → Audio → Probar)
- [ ] Compartir pantalla funciona (en reunión de prueba)
- [ ] Notificaciones funcionan
- [ ] Integración con el sistema (iconos, menús)

---

## 📈 Información del Sistema

**Sistema verificado:**
- **OS:** Manjaro Linux (Kernel 6.16.8-1-MANJARO)
- **DE:** KDE Plasma
- **Flatpak:** ✅ Instalado
- **Yay:** ✅ Instalado
- **Versión de Zoom:** 6.6.0

**Dependencias verificadas:**
- fontconfig 2:2.17.1-1 ✅
- glib2 2.84.4-2 ✅
- libpulse 17.0+r43+g3e2bb8a1e-1 ✅
- libx11 1.8.12-1 ✅
- Y más...

---

## 🔄 Comandos Útiles

### Actualizar Zoom
```bash
# Flatpak
flatpak update us.zoom.Zoom

# AUR
yay -Syu zoom
```

### Desinstalar Zoom
```bash
# Flatpak
flatpak uninstall us.zoom.Zoom

# AUR
yay -Rns zoom
```

### Ver información de Zoom
```bash
# Flatpak
flatpak info us.zoom.Zoom

# AUR
pacman -Qi zoom
```

### Ver permisos de Zoom (Flatpak)
```bash
flatpak info --show-permissions us.zoom.Zoom
```

---

## 🔗 Enlaces Útiles

### Documentación Oficial
- [Sitio oficial de Zoom](https://zoom.us/)
- [Soporte de Zoom](https://support.zoom.us/)
- [Centro de ayuda de Zoom](https://support.zoom.us/hc/es)

### Recursos de la Comunidad
- [Flathub - Zoom](https://flathub.org/apps/us.zoom.Zoom)
- [AUR - Zoom](https://aur.archlinux.org/packages/zoom)
- [Wiki de Arch Linux](https://wiki.archlinux.org/)
- [Foros de Manjaro](https://forum.manjaro.org/)

### Documentación Técnica
- [Documentación de Flatpak](https://docs.flatpak.org/)
- [Guía de AUR](https://wiki.archlinux.org/title/Arch_User_Repository)
- [Permisos de Flatpak](https://docs.flatpak.org/en/latest/sandbox-permissions.html)

---

## 💡 Consejos Adicionales

### Crear alias para ejecución rápida (Flatpak)
```bash
echo 'alias zoom="flatpak run us.zoom.Zoom"' >> ~/.bashrc
source ~/.bashrc
```

### Configurar inicio automático
```bash
# Crear archivo .desktop personalizado
cp /var/lib/flatpak/exports/share/applications/us.zoom.Zoom.desktop ~/.config/autostart/
```

### Optimizar rendimiento de video
```bash
# Para Flatpak
flatpak override us.zoom.Zoom --socket=wayland
flatpak override us.zoom.Zoom --device=dri
```

---

## 🎓 Aprende Más

### Tutoriales Recomendados
1. [Cómo usar Zoom - Guía oficial](https://support.zoom.us/hc/es/articles/206618765)
2. [Mejores prácticas de seguridad en Zoom](https://support.zoom.us/hc/es/articles/201362723)
3. [Compartir pantalla en Zoom](https://support.zoom.us/hc/es/articles/201362153)

### Videos Útiles
- Búsqueda recomendada: "Zoom tutorial español"
- Búsqueda recomendada: "Zoom Linux tutorial"

---

## 📞 Soporte

### ¿Necesitas ayuda?

1. **Consulta la documentación:**
   - ZOOM-INSTALACION-RAPIDA.md
   - ZOOM-DECISION-GUIDE.md
   - RDP-Manjaro.md (sección Zoom)

2. **Revisa los problemas comunes:**
   - Todos los documentos incluyen sección de solución de problemas

3. **Comunidad:**
   - [Foros de Manjaro](https://forum.manjaro.org/)
   - [Reddit r/ManjaroLinux](https://www.reddit.com/r/ManjaroLinux/)
   - [Foros de Arch Linux](https://bbs.archlinux.org/)

4. **Soporte oficial de Zoom:**
   - [Centro de ayuda](https://support.zoom.us/hc/es)
   - [Contactar soporte](https://support.zoom.us/hc/es/articles/201362003)

---

## 🎉 ¡Listo para Empezar!

Ahora tienes toda la información necesaria para instalar y usar Zoom en Manjaro Linux.

### Pasos Recomendados:

1. **Lee** ZOOM-INSTALACION-RAPIDA.md (5 minutos)
2. **Ejecuta** ./install-zoom.sh (2 minutos)
3. **Verifica** que todo funcione (5 minutos)
4. **Disfruta** de Zoom en Manjaro Linux 🎉

---

## 📝 Notas Finales

- **Versión de la documentación:** 1.0
- **Fecha de creación:** 2025-10-12
- **Última actualización:** 2025-10-12
- **Autor:** Documentación creada para Manjaro Linux
- **Licencia:** Documentación libre para uso personal

---

## 🔄 Actualizaciones

Esta documentación se actualizará cuando:
- Haya nuevas versiones de Zoom
- Cambien los métodos de instalación recomendados
- Se descubran nuevos problemas o soluciones
- La comunidad aporte mejoras

---

**¿Preguntas? ¿Sugerencias?** Consulta la documentación completa o busca ayuda en los foros de la comunidad.

---

*¡Feliz videoconferencia con Zoom en Manjaro Linux!* 🎉📹

---

*Última actualización: 2025-10-12*

