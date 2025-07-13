#!/usr/bin/env python3
"""
🔍 Validador de Código TikZ
===========================

Módulo especializado en validación y verificación de código TikZ generado,
incluyendo compilación, comparación visual y métricas de calidad.

Autor: Agente IA Especializado
Fecha: 2025-01-13
Versión: 1.0.0
"""

import os
import subprocess
import tempfile
import logging
import cv2
import numpy as np
from typing import Dict, List, Any, Optional, Tuple
from pathlib import Path
import shutil

class ValidadorCodigo:
    """
    🔍 Validador especializado de código TikZ.
    
    Verifica sintaxis, compila código, genera imágenes de salida
    y compara visualmente con imagen original.
    """
    
    def __init__(self, config: Dict[str, Any]):
        """
        Inicializar validador con configuración.
        
        Args:
            config: Configuración del agente
        """
        self.config = config
        self.logger = logging.getLogger("ValidadorCodigo")
        
        # Configuración de validación
        self.timeout_compilacion = config.get('timeout_compilacion', 30)
        self.umbral_similitud = config.get('umbral_similitud', 0.95)
        self.directorio_temp = Path(tempfile.gettempdir()) / "agente_tikz_validacion"
        self.directorio_temp.mkdir(exist_ok=True)
        
        # Verificar herramientas necesarias
        self._verificar_herramientas()
        
        self.logger.info("🔍 Validador de código inicializado")
    
    def validar(self, codigo_tikz: str, imagen_original: str = None) -> Dict[str, Any]:
        """
        🎯 Validar código TikZ completo.
        
        Args:
            codigo_tikz: Código TikZ a validar
            imagen_original: Ruta a imagen original para comparación
            
        Returns:
            Diccionario con resultados de validación
        """
        self.logger.info("🔍 Iniciando validación de código TikZ")
        
        resultado = {
            'exitoso': False,
            'errores': [],
            'advertencias': [],
            'metricas': {},
            'similitud': 0.0,
            'tiempo_compilacion': 0.0
        }
        
        try:
            # 1. Validación de sintaxis
            self.logger.info("📝 Validando sintaxis")
            sintaxis_ok, errores_sintaxis = self._validar_sintaxis(codigo_tikz)
            
            if not sintaxis_ok:
                resultado['errores'].extend(errores_sintaxis)
                return resultado
            
            # 2. Compilación con LaTeX
            self.logger.info("🔨 Compilando con LaTeX")
            compilacion_ok, tiempo_comp, errores_comp = self._compilar_latex(codigo_tikz)
            resultado['tiempo_compilacion'] = tiempo_comp
            
            if not compilacion_ok:
                resultado['errores'].extend(errores_comp)
                return resultado
            
            # 3. Comparación visual si hay imagen original
            if imagen_original and Path(imagen_original).exists():
                self.logger.info("👁️ Comparando visualmente")
                similitud = self._comparar_visualmente(imagen_original)
                resultado['similitud'] = similitud
                
                if similitud < self.umbral_similitud:
                    resultado['advertencias'].append(
                        f"Similitud visual baja: {similitud:.3f} < {self.umbral_similitud}"
                    )
            
            # 4. Métricas de calidad
            resultado['metricas'] = self._calcular_metricas_calidad(codigo_tikz)
            
            # 5. Verificaciones adicionales
            advertencias_adicionales = self._verificaciones_adicionales(codigo_tikz)
            resultado['advertencias'].extend(advertencias_adicionales)
            
            resultado['exitoso'] = True
            self.logger.info("✅ Validación completada exitosamente")
            
        except Exception as e:
            self.logger.error(f"❌ Error en validación: {e}")
            resultado['errores'].append(f"Error interno: {str(e)}")
        
        return resultado
    
    def _verificar_herramientas(self):
        """Verificar que las herramientas necesarias están disponibles."""
        herramientas = ['pdflatex', 'convert']  # ImageMagick convert
        
        for herramienta in herramientas:
            if not shutil.which(herramienta):
                self.logger.warning(f"⚠️ Herramienta no encontrada: {herramienta}")
            else:
                self.logger.info(f"✅ Herramienta disponible: {herramienta}")
    
    def _validar_sintaxis(self, codigo: str) -> Tuple[bool, List[str]]:
        """
        Validar sintaxis básica del código TikZ.
        
        Args:
            codigo: Código TikZ a validar
            
        Returns:
            Tupla (es_valido, lista_errores)
        """
        errores = []
        
        # Verificar estructura básica
        if '\\begin{tikzpicture}' not in codigo:
            errores.append("Falta \\begin{tikzpicture}")
        
        if '\\end{tikzpicture}' not in codigo:
            errores.append("Falta \\end{tikzpicture}")
        
        # Verificar balance de llaves
        if not self._verificar_balance_llaves(codigo):
            errores.append("Llaves desbalanceadas")
        
        # Verificar comandos básicos
        comandos_invalidos = self._detectar_comandos_invalidos(codigo)
        if comandos_invalidos:
            errores.extend([f"Comando inválido: {cmd}" for cmd in comandos_invalidos])
        
        # Verificar coordenadas
        coordenadas_invalidas = self._detectar_coordenadas_invalidas(codigo)
        if coordenadas_invalidas:
            errores.extend([f"Coordenada inválida: {coord}" for coord in coordenadas_invalidas])
        
        return len(errores) == 0, errores
    
    def _compilar_latex(self, codigo_tikz: str) -> Tuple[bool, float, List[str]]:
        """
        Compilar código TikZ con LaTeX.
        
        Args:
            codigo_tikz: Código TikZ a compilar
            
        Returns:
            Tupla (exitoso, tiempo_segundos, errores)
        """
        import time
        
        inicio = time.time()
        errores = []
        
        try:
            # Crear documento LaTeX completo
            documento_latex = self._crear_documento_latex(codigo_tikz)
            
            # Crear archivo temporal
            with tempfile.NamedTemporaryFile(mode='w', suffix='.tex', 
                                           dir=self.directorio_temp, delete=False) as f:
                f.write(documento_latex)
                archivo_tex = f.name
            
            # Compilar con pdflatex
            comando = [
                'pdflatex', 
                '-interaction=nonstopmode',
                '-output-directory', str(self.directorio_temp),
                archivo_tex
            ]
            
            resultado = subprocess.run(
                comando, 
                capture_output=True, 
                text=True, 
                timeout=self.timeout_compilacion,
                cwd=self.directorio_temp
            )
            
            tiempo_compilacion = time.time() - inicio
            
            if resultado.returncode != 0:
                errores.append("Error de compilación LaTeX")
                # Extraer errores específicos del log
                errores_latex = self._extraer_errores_latex(resultado.stderr)
                errores.extend(errores_latex)
                return False, tiempo_compilacion, errores
            
            # Verificar que se generó el PDF
            archivo_pdf = Path(archivo_tex).with_suffix('.pdf')
            if not archivo_pdf.exists():
                errores.append("No se generó archivo PDF")
                return False, tiempo_compilacion, errores
            
            # Convertir PDF a imagen para comparación
            self._convertir_pdf_a_imagen(archivo_pdf)
            
            return True, tiempo_compilacion, []
            
        except subprocess.TimeoutExpired:
            errores.append(f"Timeout de compilación ({self.timeout_compilacion}s)")
            return False, time.time() - inicio, errores
        
        except Exception as e:
            errores.append(f"Error inesperado en compilación: {str(e)}")
            return False, time.time() - inicio, errores
        
        finally:
            # Limpiar archivos temporales
            self._limpiar_archivos_temporales(archivo_tex)
    
    def _comparar_visualmente(self, imagen_original: str) -> float:
        """
        Comparar visualmente imagen generada con original.
        
        Args:
            imagen_original: Ruta a imagen original
            
        Returns:
            Métrica de similitud (0.0 a 1.0)
        """
        try:
            # Cargar imagen original
            img_original = cv2.imread(imagen_original)
            if img_original is None:
                self.logger.warning("No se pudo cargar imagen original")
                return 0.0
            
            # Buscar imagen generada más reciente
            imagen_generada = self._encontrar_imagen_generada()
            if not imagen_generada:
                self.logger.warning("No se encontró imagen generada")
                return 0.0
            
            img_generada = cv2.imread(str(imagen_generada))
            if img_generada is None:
                self.logger.warning("No se pudo cargar imagen generada")
                return 0.0
            
            # Redimensionar para comparación
            h, w = img_original.shape[:2]
            img_generada = cv2.resize(img_generada, (w, h))
            
            # Calcular similitud usando diferentes métricas
            similitud_ssim = self._calcular_ssim(img_original, img_generada)
            similitud_hist = self._calcular_similitud_histograma(img_original, img_generada)
            similitud_contornos = self._calcular_similitud_contornos(img_original, img_generada)
            
            # Promedio ponderado de métricas
            similitud_total = (
                similitud_ssim * 0.5 + 
                similitud_hist * 0.3 + 
                similitud_contornos * 0.2
            )
            
            self.logger.info(f"Similitudes: SSIM={similitud_ssim:.3f}, "
                           f"Hist={similitud_hist:.3f}, Contornos={similitud_contornos:.3f}")
            
            return similitud_total
            
        except Exception as e:
            self.logger.error(f"Error en comparación visual: {e}")
            return 0.0
    
    def _calcular_metricas_calidad(self, codigo: str) -> Dict[str, float]:
        """Calcular métricas de calidad del código."""
        lineas = codigo.split('\n')
        lineas_codigo = [l for l in lineas if l.strip() and not l.strip().startswith('%')]
        
        metricas = {
            'lineas_totales': len(lineas),
            'lineas_codigo': len(lineas_codigo),
            'lineas_comentarios': len(lineas) - len(lineas_codigo),
            'complejidad_ciclomatica': self._calcular_complejidad_ciclomatica(codigo),
            'densidad_comentarios': len(lineas) - len(lineas_codigo) / max(len(lineas), 1),
            'comandos_draw': codigo.count('\\draw'),
            'comandos_fill': codigo.count('\\fill'),
            'comandos_node': codigo.count('\\node'),
            'uso_librerias': len(self._extraer_librerias_usadas(codigo))
        }
        
        return metricas
    
    def _verificaciones_adicionales(self, codigo: str) -> List[str]:
        """Realizar verificaciones adicionales de calidad."""
        advertencias = []
        
        # Verificar uso de colores
        if 'color' not in codigo and 'blue' not in codigo and 'red' not in codigo:
            advertencias.append("No se detectan colores en el código")
        
        # Verificar escalabilidad
        if 'scale=' not in codigo:
            advertencias.append("No se especifica escala explícita")
        
        # Verificar comentarios
        lineas_comentario = len([l for l in codigo.split('\n') if l.strip().startswith('%')])
        if lineas_comentario < 3:
            advertencias.append("Pocos comentarios descriptivos")
        
        # Verificar complejidad
        if codigo.count('\\draw') > 20:
            advertencias.append("Código muy complejo (muchos comandos draw)")
        
        return advertencias
    
    # Métodos auxiliares
    def _verificar_balance_llaves(self, codigo: str) -> bool:
        """Verificar que las llaves estén balanceadas."""
        contador = 0
        for char in codigo:
            if char == '{':
                contador += 1
            elif char == '}':
                contador -= 1
            if contador < 0:
                return False
        return contador == 0
    
    def _detectar_comandos_invalidos(self, codigo: str) -> List[str]:
        """Detectar comandos TikZ inválidos."""
        import re
        
        # Lista de comandos válidos comunes
        comandos_validos = {
            'draw', 'fill', 'node', 'path', 'clip', 'shade', 'pattern',
            'begin', 'end', 'usetikzlibrary', 'tikzset', 'pgfplotsset'
        }
        
        # Buscar comandos que empiecen con backslash
        comandos_encontrados = re.findall(r'\\([a-zA-Z]+)', codigo)
        
        comandos_invalidos = []
        for comando in set(comandos_encontrados):
            if comando not in comandos_validos and not comando.startswith('tikz'):
                comandos_invalidos.append(comando)
        
        return comandos_invalidos[:5]  # Limitar a 5 para no saturar
    
    def _detectar_coordenadas_invalidas(self, codigo: str) -> List[str]:
        """Detectar coordenadas con formato inválido."""
        import re
        
        # Buscar patrones de coordenadas
        patron_coordenadas = r'\(([^)]+)\)'
        coordenadas = re.findall(patron_coordenadas, codigo)
        
        coordenadas_invalidas = []
        for coord in coordenadas:
            # Verificar formato básico (x,y)
            if ',' not in coord:
                continue
            
            partes = coord.split(',')
            if len(partes) != 2:
                coordenadas_invalidas.append(coord)
                continue
            
            # Verificar que sean números o expresiones válidas
            for parte in partes:
                parte = parte.strip()
                if not (parte.replace('.', '').replace('-', '').isdigit() or 
                       any(func in parte for func in ['sin', 'cos', 'sqrt'])):
                    coordenadas_invalidas.append(coord)
                    break
        
        return coordenadas_invalidas[:3]  # Limitar a 3
    
    def _crear_documento_latex(self, codigo_tikz: str) -> str:
        """Crear documento LaTeX completo para compilación."""
        return f"""\\documentclass{{standalone}}
\\usepackage{{tikz}}
\\usepackage{{pgfplots}}
\\usetikzlibrary{{calc,arrows.meta,positioning}}
\\pgfplotsset{{compat=1.18}}

\\begin{{document}}
{codigo_tikz}
\\end{{document}}"""
    
    def _extraer_errores_latex(self, stderr: str) -> List[str]:
        """Extraer errores específicos del log de LaTeX."""
        errores = []
        
        # Buscar líneas que contengan errores comunes
        lineas_error = [
            'Undefined control sequence',
            'Missing $ inserted',
            'Extra }',
            'Missing }',
            'Package tikz Error',
            'Package pgfplots Error'
        ]
        
        for linea in stderr.split('\n'):
            for error_tipo in lineas_error:
                if error_tipo in linea:
                    errores.append(linea.strip())
                    break
        
        return errores[:5]  # Limitar a 5 errores
    
    def _convertir_pdf_a_imagen(self, archivo_pdf: Path):
        """Convertir PDF generado a imagen PNG."""
        try:
            archivo_png = archivo_pdf.with_suffix('.png')
            
            comando = [
                'convert',
                '-density', '300',
                '-quality', '90',
                str(archivo_pdf),
                str(archivo_png)
            ]
            
            subprocess.run(comando, check=True, capture_output=True)
            
        except subprocess.CalledProcessError as e:
            self.logger.warning(f"Error convirtiendo PDF a PNG: {e}")
        except FileNotFoundError:
            self.logger.warning("ImageMagick convert no disponible")
    
    def _encontrar_imagen_generada(self) -> Optional[Path]:
        """Encontrar la imagen generada más reciente."""
        archivos_png = list(self.directorio_temp.glob('*.png'))
        
        if not archivos_png:
            return None
        
        # Retornar el más reciente
        return max(archivos_png, key=lambda p: p.stat().st_mtime)
    
    def _calcular_ssim(self, img1: np.ndarray, img2: np.ndarray) -> float:
        """Calcular índice de similitud estructural (SSIM)."""
        try:
            from skimage.metrics import structural_similarity as ssim
            
            # Convertir a escala de grises
            gray1 = cv2.cvtColor(img1, cv2.COLOR_BGR2GRAY)
            gray2 = cv2.cvtColor(img2, cv2.COLOR_BGR2GRAY)
            
            return ssim(gray1, gray2)
            
        except ImportError:
            # Fallback: usar correlación simple
            gray1 = cv2.cvtColor(img1, cv2.COLOR_BGR2GRAY)
            gray2 = cv2.cvtColor(img2, cv2.COLOR_BGR2GRAY)
            
            correlation = cv2.matchTemplate(gray1, gray2, cv2.TM_CCOEFF_NORMED)
            return float(np.max(correlation))
    
    def _calcular_similitud_histograma(self, img1: np.ndarray, img2: np.ndarray) -> float:
        """Calcular similitud basada en histogramas de color."""
        # Calcular histogramas
        hist1 = cv2.calcHist([img1], [0, 1, 2], None, [50, 50, 50], [0, 256, 0, 256, 0, 256])
        hist2 = cv2.calcHist([img2], [0, 1, 2], None, [50, 50, 50], [0, 256, 0, 256, 0, 256])
        
        # Normalizar
        cv2.normalize(hist1, hist1, alpha=0, beta=1, norm_type=cv2.NORM_MINMAX)
        cv2.normalize(hist2, hist2, alpha=0, beta=1, norm_type=cv2.NORM_MINMAX)
        
        # Calcular correlación
        return cv2.compareHist(hist1, hist2, cv2.HISTCMP_CORREL)
    
    def _calcular_similitud_contornos(self, img1: np.ndarray, img2: np.ndarray) -> float:
        """Calcular similitud basada en contornos."""
        # Convertir a escala de grises y detectar contornos
        gray1 = cv2.cvtColor(img1, cv2.COLOR_BGR2GRAY)
        gray2 = cv2.cvtColor(img2, cv2.COLOR_BGR2GRAY)
        
        edges1 = cv2.Canny(gray1, 50, 150)
        edges2 = cv2.Canny(gray2, 50, 150)
        
        # Calcular similitud de bordes
        intersection = cv2.bitwise_and(edges1, edges2)
        union = cv2.bitwise_or(edges1, edges2)
        
        if np.sum(union) == 0:
            return 1.0 if np.sum(intersection) == 0 else 0.0
        
        return np.sum(intersection) / np.sum(union)
    
    def _calcular_complejidad_ciclomatica(self, codigo: str) -> float:
        """Calcular complejidad ciclomática aproximada."""
        # Contar estructuras de control y comandos complejos
        complejidad = 1  # Base
        
        # Comandos que aumentan complejidad
        complejidad += codigo.count('\\foreach')
        complejidad += codigo.count('\\ifthenelse')
        complejidad += codigo.count('\\pgfmathparse')
        complejidad += codigo.count('controls')  # Curvas Bézier
        
        return min(complejidad, 10.0)  # Limitar a 10
    
    def _extraer_librerias_usadas(self, codigo: str) -> List[str]:
        """Extraer librerías TikZ usadas."""
        import re
        
        patron = r'\\usetikzlibrary\{([^}]+)\}'
        matches = re.findall(patron, codigo)
        
        librerias = []
        for match in matches:
            librerias.extend([lib.strip() for lib in match.split(',')])
        
        return list(set(librerias))
    
    def _limpiar_archivos_temporales(self, archivo_base: str):
        """Limpiar archivos temporales generados."""
        try:
            base_path = Path(archivo_base)
            
            # Extensiones a limpiar
            extensiones = ['.tex', '.pdf', '.aux', '.log', '.png', '.fls', '.fdb_latexmk']
            
            for ext in extensiones:
                archivo = base_path.with_suffix(ext)
                if archivo.exists():
                    archivo.unlink()
                    
        except Exception as e:
            self.logger.warning(f"Error limpiando archivos temporales: {e}")
