#!/usr/bin/env python3
"""
🛠️ Generador de Código TikZ
===========================

Módulo especializado en generación de código TikZ de alta calidad
basado en análisis de imágenes matemáticas. Compatible con Qtikz/Ktikz.

Autor: Agente IA Especializado
Fecha: 2025-01-13
Versión: 1.0.0
"""

import logging
import json
from typing import Dict, List, Any, Optional
from pathlib import Path
import re

class GeneradorTikZ:
    """
    🛠️ Generador especializado de código TikZ.
    
    Convierte análisis de imágenes en código TikZ optimizado,
    usando templates profesionales y mejores prácticas.
    """
    
    def __init__(self, config: Dict[str, Any], directorio_base: Path):
        """
        Inicializar generador con configuración y templates.
        
        Args:
            config: Configuración del agente
            directorio_base: Directorio base del agente
        """
        self.config = config
        self.directorio_base = directorio_base
        self.logger = logging.getLogger("GeneradorTikZ")
        
        # Cargar templates
        self.templates_dir = directorio_base / config.get('templates_dir', 'templates')
        self.templates = self._cargar_templates()
        
        # Configuración de generación
        self.escala_default = config.get('escala_default', 1.2)
        self.precision_coordenadas = config.get('precision_coordenadas', 2)
        
        self.logger.info("🛠️ Generador TikZ inicializado")
    
    def generar(self, analisis: Dict[str, Any]) -> str:
        """
        🎯 Generar código TikZ basado en análisis de imagen.
        
        Args:
            analisis: Resultado del análisis de imagen
            
        Returns:
            Código TikZ generado
        """
        self.logger.info(f"🛠️ Generando TikZ para tipo: {analisis.get('tipo_detectado', 'general')}")
        
        try:
            tipo = analisis.get('tipo_detectado', 'general')
            
            # Seleccionar método de generación según tipo
            if tipo == 'funcion':
                codigo = self._generar_funcion(analisis)
            elif tipo == 'geometria':
                codigo = self._generar_geometria(analisis)
            elif tipo == 'diagrama':
                codigo = self._generar_diagrama(analisis)
            else:
                codigo = self._generar_general(analisis)
            
            # Aplicar optimizaciones
            if self.config.get('optimizacion_codigo', True):
                codigo = self.optimizar(codigo)
            
            self.logger.info("✅ Código TikZ generado exitosamente")
            return codigo
            
        except Exception as e:
            self.logger.error(f"❌ Error generando TikZ: {e}")
            return self._generar_codigo_error(str(e))
    
    def _generar_funcion(self, analisis: Dict[str, Any]) -> str:
        """Generar código TikZ para gráficas de funciones."""
        template = self.templates.get('funcion', self._template_funcion_default())
        
        # Extraer información del análisis
        ejes = analisis.get('ejes', {})
        curvas = analisis.get('curvas', [])
        puntos = analisis.get('puntos_importantes', [])
        cuadricula = analisis.get('cuadricula', {})
        
        # Construir código
        codigo_partes = []
        
        # Encabezado
        codigo_partes.append(f"\\begin{{tikzpicture}}[scale={self.escala_default}]")
        
        # Cuadrícula si está presente
        if cuadricula.get('presente', False):
            codigo_partes.append("% Cuadrícula de fondo")
            codigo_partes.append("\\draw[gray!20, thin] (0,0) grid[step=0.5] (5.5,3.8);")
        
        # Ejes coordenados
        if ejes.get('detectados', False):
            codigo_partes.append("% Ejes principales")
            codigo_partes.append("\\draw[thick, ->] (0,0) -- (5.5,0) node[below right] {$x$};")
            codigo_partes.append("\\draw[thick, ->] (0,0) -- (0,3.8) node[above left] {$y$};")
        
        # Curvas detectadas
        for i, curva in enumerate(curvas[:3]):  # Máximo 3 curvas principales
            puntos_curva = curva.get('puntos', [])
            if len(puntos_curva) > 2:
                codigo_partes.append(f"% Curva {i+1}")
                codigo_tikz_curva = self._convertir_puntos_a_tikz(puntos_curva)
                codigo_partes.append(f"\\draw[blue, very thick] {codigo_tikz_curva};")
        
        # Puntos importantes
        if puntos:
            codigo_partes.append("% Puntos importantes")
            for punto in puntos[:5]:  # Máximo 5 puntos
                x, y = self._normalizar_coordenadas(punto, analisis['dimensiones'])
                codigo_partes.append(f"\\fill[red] ({x},{y}) circle (2pt);")
        
        # Origen
        codigo_partes.append("% Origen")
        codigo_partes.append("\\fill[black] (0,0) circle (1.5pt);")
        
        # Cierre
        codigo_partes.append("\\end{tikzpicture}")
        
        return '\n'.join(codigo_partes)
    
    def _generar_geometria(self, analisis: Dict[str, Any]) -> str:
        """Generar código TikZ para figuras geométricas."""
        codigo_partes = []
        
        # Encabezado con librerías necesarias
        codigo_partes.append("\\usetikzlibrary{calc}")
        codigo_partes.append(f"\\begin{{tikzpicture}}[scale={self.escala_default}]")
        
        # Formas geométricas detectadas
        formas = analisis.get('formas', [])
        colores = ['blue', 'red', 'green', 'orange', 'purple']
        
        for i, forma in enumerate(formas[:5]):  # Máximo 5 formas
            tipo = forma.get('tipo', 'poligono')
            vertices = forma.get('vertices', [])
            color = colores[i % len(colores)]
            
            if vertices:
                codigo_partes.append(f"% {tipo.capitalize()} {i+1}")
                
                if tipo == 'triangulo':
                    codigo_partes.append(self._generar_triangulo(vertices, color))
                elif tipo == 'cuadrilatero':
                    codigo_partes.append(self._generar_cuadrilatero(vertices, color))
                elif tipo == 'circulo':
                    codigo_partes.append(self._generar_circulo(vertices, color))
                else:
                    codigo_partes.append(self._generar_poligono(vertices, color))
        
        # Medidas y etiquetas si están disponibles
        medidas = analisis.get('medidas', [])
        for medida in medidas[:3]:  # Máximo 3 medidas
            codigo_partes.append(self._generar_medida(medida))
        
        codigo_partes.append("\\end{tikzpicture}")
        
        return '\n'.join(codigo_partes)
    
    def _generar_diagrama(self, analisis: Dict[str, Any]) -> str:
        """Generar código TikZ para diagramas."""
        codigo_partes = []
        
        # Encabezado
        codigo_partes.append("\\usetikzlibrary{arrows.meta}")
        codigo_partes.append(f"\\begin{{tikzpicture}}[scale={self.escala_default}]")
        
        # Nodos
        nodos = analisis.get('nodos', [])
        for i, nodo in enumerate(nodos[:10]):  # Máximo 10 nodos
            x, y = self._normalizar_coordenadas(nodo, analisis['dimensiones'])
            codigo_partes.append(f"\\node[circle, draw] (n{i}) at ({x},{y}) {{{i+1}}};")
        
        # Conexiones
        conexiones = analisis.get('conexiones', [])
        for conexion in conexiones[:15]:  # Máximo 15 conexiones
            codigo_partes.append(self._generar_conexion(conexion))
        
        codigo_partes.append("\\end{tikzpicture}")
        
        return '\n'.join(codigo_partes)
    
    def _generar_general(self, analisis: Dict[str, Any]) -> str:
        """Generar código TikZ general."""
        codigo_partes = []
        
        codigo_partes.append(f"\\begin{{tikzpicture}}[scale={self.escala_default}]")
        codigo_partes.append("% Gráfica general basada en análisis de imagen")
        
        # Elementos básicos detectados
        elementos = analisis.get('elementos_basicos', {})
        
        # Agregar elementos según disponibilidad
        if elementos.get('lineas', 0) > 0:
            codigo_partes.append("% Líneas detectadas")
            codigo_partes.append("\\draw[thick] (0,0) -- (3,2);")
        
        if elementos.get('formas', 0) > 0:
            codigo_partes.append("% Formas detectadas")
            codigo_partes.append("\\draw[fill=blue!20] (1,1) rectangle (2,2);")
        
        # Texto si fue detectado
        texto_info = analisis.get('texto_detectado', {})
        if texto_info.get('regiones_detectadas', 0) > 0:
            codigo_partes.append("% Regiones de texto")
            codigo_partes.append("\\node at (2,3) {Texto};")
        
        codigo_partes.append("\\end{tikzpicture}")
        
        return '\n'.join(codigo_partes)
    
    def refinar(self, codigo_actual: str, errores: List[str], 
                analisis: Dict[str, Any]) -> str:
        """
        🔄 Refinar código TikZ basado en errores detectados.
        
        Args:
            codigo_actual: Código TikZ actual
            errores: Lista de errores detectados
            analisis: Análisis original de la imagen
            
        Returns:
            Código TikZ refinado
        """
        self.logger.info(f"🔄 Refinando código TikZ ({len(errores)} errores)")
        
        codigo_refinado = codigo_actual
        
        for error in errores:
            if 'syntax' in error.lower():
                codigo_refinado = self._corregir_sintaxis(codigo_refinado)
            elif 'coordinate' in error.lower():
                codigo_refinado = self._corregir_coordenadas(codigo_refinado)
            elif 'library' in error.lower():
                codigo_refinado = self._agregar_librerias(codigo_refinado)
            elif 'scale' in error.lower():
                codigo_refinado = self._ajustar_escala(codigo_refinado)
        
        return codigo_refinado
    
    def optimizar(self, codigo: str) -> str:
        """
        ⚡ Optimizar código TikZ para mejor rendimiento y legibilidad.
        
        Args:
            codigo: Código TikZ a optimizar
            
        Returns:
            Código TikZ optimizado
        """
        self.logger.info("⚡ Optimizando código TikZ")
        
        # Eliminar líneas vacías excesivas
        codigo = re.sub(r'\n\s*\n\s*\n', '\n\n', codigo)
        
        # Formatear indentación
        lineas = codigo.split('\n')
        lineas_formateadas = []
        nivel_indentacion = 0
        
        for linea in lineas:
            linea_limpia = linea.strip()
            
            if not linea_limpia:
                lineas_formateadas.append('')
                continue
            
            # Ajustar indentación
            if linea_limpia.startswith('\\end{'):
                nivel_indentacion = max(0, nivel_indentacion - 1)
            
            indentacion = '  ' * nivel_indentacion
            lineas_formateadas.append(indentacion + linea_limpia)
            
            if linea_limpia.startswith('\\begin{'):
                nivel_indentacion += 1
        
        # Agregar comentarios descriptivos
        codigo_optimizado = '\n'.join(lineas_formateadas)
        codigo_optimizado = self._agregar_comentarios_descriptivos(codigo_optimizado)
        
        return codigo_optimizado
    
    def _cargar_templates(self) -> Dict[str, str]:
        """Cargar templates TikZ desde archivos."""
        templates = {}
        
        if self.templates_dir.exists():
            for archivo in self.templates_dir.glob('*.tikz'):
                try:
                    with open(archivo, 'r', encoding='utf-8') as f:
                        nombre = archivo.stem
                        templates[nombre] = f.read()
                    self.logger.info(f"✅ Template cargado: {nombre}")
                except Exception as e:
                    self.logger.warning(f"⚠️ Error cargando template {archivo}: {e}")
        
        return templates
    
    def _template_funcion_default(self) -> str:
        """Template por defecto para funciones."""
        return """\\begin{tikzpicture}[scale=1.2]
% Cuadrícula de fondo
\\draw[gray!20, thin] (0,0) grid[step=0.5] (5.5,3.8);
% Ejes principales
\\draw[thick, ->] (0,0) -- (5.5,0) node[below right] {$x$};
\\draw[thick, ->] (0,0) -- (0,3.8) node[above left] {$y$};
% Función
\\draw[blue, very thick] plot coordinates {(0,1) (1,2) (2,1.5) (3,3) (4,2) (5,1)};
% Origen
\\fill[black] (0,0) circle (1.5pt);
\\end{tikzpicture}"""
    
    def _convertir_puntos_a_tikz(self, puntos: List[List[int]]) -> str:
        """Convertir lista de puntos a formato TikZ."""
        if len(puntos) < 2:
            return ""
        
        # Normalizar puntos y crear path
        puntos_normalizados = []
        for punto in puntos[::max(1, len(puntos)//20)]:  # Submuestrear si hay muchos puntos
            x, y = self._normalizar_coordenadas(punto, (800, 600))  # Dimensiones por defecto
            puntos_normalizados.append(f"({x},{y})")
        
        return "plot coordinates {" + " ".join(puntos_normalizados) + "}"
    
    def _normalizar_coordenadas(self, punto: List[int], dimensiones: tuple) -> tuple:
        """Normalizar coordenadas de imagen a coordenadas TikZ."""
        x, y = punto
        w, h = dimensiones
        
        # Normalizar a rango [0, 5.5] para x y [0, 3.8] para y
        x_norm = round((x / w) * 5.5, self.precision_coordenadas)
        y_norm = round(((h - y) / h) * 3.8, self.precision_coordenadas)  # Invertir Y
        
        return x_norm, y_norm
    
    def _generar_triangulo(self, vertices: List[List[int]], color: str) -> str:
        """Generar código TikZ para triángulo."""
        if len(vertices) < 3:
            return ""
        
        v1, v2, v3 = vertices[:3]
        x1, y1 = self._normalizar_coordenadas(v1, (800, 600))
        x2, y2 = self._normalizar_coordenadas(v2, (800, 600))
        x3, y3 = self._normalizar_coordenadas(v3, (800, 600))
        
        return f"\\draw[{color}, thick, fill={color}!20] ({x1},{y1}) -- ({x2},{y2}) -- ({x3},{y3}) -- cycle;"
    
    def _generar_cuadrilatero(self, vertices: List[List[int]], color: str) -> str:
        """Generar código TikZ para cuadrilátero."""
        if len(vertices) < 4:
            return ""
        
        coords = []
        for vertice in vertices[:4]:
            x, y = self._normalizar_coordenadas(vertice, (800, 600))
            coords.append(f"({x},{y})")
        
        return f"\\draw[{color}, thick, fill={color}!20] {' -- '.join(coords)} -- cycle;"
    
    def _generar_circulo(self, vertices: List[List[int]], color: str) -> str:
        """Generar código TikZ para círculo."""
        if len(vertices) < 3:
            return ""
        
        # Estimar centro y radio
        xs = [v[0] for v in vertices]
        ys = [v[1] for v in vertices]
        centro_x = sum(xs) / len(xs)
        centro_y = sum(ys) / len(ys)
        
        # Radio aproximado
        radio = max(max(xs) - min(xs), max(ys) - min(ys)) / 2
        
        cx, cy = self._normalizar_coordenadas([centro_x, centro_y], (800, 600))
        r_norm = radio / 800 * 5.5  # Normalizar radio
        
        return f"\\draw[{color}, thick, fill={color}!20] ({cx},{cy}) circle ({r_norm:.2f});"
    
    def _generar_poligono(self, vertices: List[List[int]], color: str) -> str:
        """Generar código TikZ para polígono general."""
        if len(vertices) < 3:
            return ""
        
        coords = []
        for vertice in vertices:
            x, y = self._normalizar_coordenadas(vertice, (800, 600))
            coords.append(f"({x},{y})")
        
        return f"\\draw[{color}, thick, fill={color}!20] {' -- '.join(coords)} -- cycle;"
    
    def _generar_medida(self, medida: Dict[str, Any]) -> str:
        """Generar código TikZ para medida."""
        # Implementación simplificada
        return "% Medida detectada"
    
    def _generar_conexion(self, conexion: Dict[str, Any]) -> str:
        """Generar código TikZ para conexión."""
        # Implementación simplificada
        return "% Conexión detectada"
    
    def _corregir_sintaxis(self, codigo: str) -> str:
        """Corregir errores de sintaxis comunes."""
        # Corregir punto y coma faltantes
        codigo = re.sub(r'(\\draw[^;]*[^;])\s*\n', r'\1;\n', codigo)
        
        # Corregir llaves desbalanceadas
        codigo = self._balancear_llaves(codigo)
        
        return codigo
    
    def _corregir_coordenadas(self, codigo: str) -> str:
        """Corregir coordenadas inválidas."""
        # Buscar coordenadas fuera de rango y normalizarlas
        def normalizar_coord(match):
            x, y = float(match.group(1)), float(match.group(2))
            x = max(0, min(x, 5.5))
            y = max(0, min(y, 3.8))
            return f"({x:.2f},{y:.2f})"
        
        codigo = re.sub(r'\((-?\d+\.?\d*),(-?\d+\.?\d*)\)', normalizar_coord, codigo)
        
        return codigo
    
    def _agregar_librerias(self, codigo: str) -> str:
        """Agregar librerías TikZ necesarias."""
        librerias_comunes = [
            "\\usetikzlibrary{calc}",
            "\\usetikzlibrary{arrows.meta}",
            "\\usetikzlibrary{positioning}"
        ]
        
        # Agregar librerías al inicio si no están presentes
        for libreria in librerias_comunes:
            if libreria not in codigo:
                codigo = libreria + '\n' + codigo
        
        return codigo
    
    def _ajustar_escala(self, codigo: str) -> str:
        """Ajustar escala del diagrama."""
        # Cambiar escala si es muy grande o muy pequeña
        codigo = re.sub(r'scale=\d+\.?\d*', f'scale={self.escala_default}', codigo)
        
        return codigo
    
    def _balancear_llaves(self, codigo: str) -> str:
        """Balancear llaves en el código."""
        # Implementación simplificada
        return codigo
    
    def _agregar_comentarios_descriptivos(self, codigo: str) -> str:
        """Agregar comentarios descriptivos al código."""
        lineas = codigo.split('\n')
        lineas_comentadas = []
        
        for linea in lineas:
            lineas_comentadas.append(linea)
            
            # Agregar comentarios para secciones importantes
            if '\\begin{tikzpicture}' in linea:
                lineas_comentadas.insert(-1, "% Gráfica generada automáticamente por Agente TikZ")
            elif '\\draw[' in linea and 'grid' in linea:
                lineas_comentadas.insert(-1, "% Cuadrícula de fondo")
            elif '\\draw[' in linea and '->' in linea:
                lineas_comentadas.insert(-1, "% Ejes coordenados")
        
        return '\n'.join(lineas_comentadas)
    
    def _generar_codigo_error(self, mensaje_error: str) -> str:
        """Generar código TikZ básico en caso de error."""
        return f"""% Error generando código TikZ: {mensaje_error}
\\begin{{tikzpicture}}[scale=1.2]
\\draw[thick, ->] (0,0) -- (3,0) node[below right] {{Error}};
\\draw[thick, ->] (0,0) -- (0,2) node[above left] {{Error}};
\\node at (1.5,1) {{Error en generación}};
\\end{{tikzpicture}}"""
