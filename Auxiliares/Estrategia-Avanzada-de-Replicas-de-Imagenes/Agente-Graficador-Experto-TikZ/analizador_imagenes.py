#!/usr/bin/env python3
"""
🔍 Analizador de Imágenes Matemáticas
====================================

Módulo especializado en análisis de imágenes que contienen gráficas,
diagramas y elementos matemáticos para extracción de características
relevantes para generación de código TikZ.

Autor: Agente IA Especializado
Fecha: 2025-01-13
Versión: 1.0.0
"""

import cv2
import numpy as np
import logging
from typing import Dict, List, Tuple, Any, Optional
from pathlib import Path
import json

class AnalizadorImagenes:
    """
    🔍 Analizador especializado en imágenes matemáticas.
    
    Detecta y extrae características de gráficas, diagramas geométricos,
    funciones matemáticas y elementos visuales relevantes.
    """
    
    def __init__(self, config: Dict[str, Any]):
        """
        Inicializar analizador con configuración.
        
        Args:
            config: Diccionario de configuración
        """
        self.config = config
        self.logger = logging.getLogger("AnalizadorImagenes")
        
        # Parámetros de análisis
        self.umbral_deteccion = config.get('umbral_deteccion', 0.7)
        self.resolucion_analisis = config.get('resolucion_analisis', (800, 600))
        
        self.logger.info("🔍 Analizador de imágenes inicializado")
    
    def analizar(self, ruta_imagen: str, descripcion: str = "", 
                tipo_esperado: str = "auto") -> Dict[str, Any]:
        """
        🎯 Análisis principal de imagen matemática.
        
        Args:
            ruta_imagen: Ruta a la imagen
            descripcion: Descripción opcional
            tipo_esperado: Tipo esperado ('auto', 'funcion', 'geometria', 'diagrama')
            
        Returns:
            Diccionario con análisis completo
        """
        self.logger.info(f"🔍 Analizando imagen: {Path(ruta_imagen).name}")
        
        try:
            # Cargar y preprocesar imagen
            imagen = self._cargar_imagen(ruta_imagen)
            if imagen is None:
                return {'exitoso': False, 'error': 'No se pudo cargar la imagen'}
            
            # Análisis base
            resultado = {
                'exitoso': True,
                'ruta_original': ruta_imagen,
                'descripcion': descripcion,
                'tipo_esperado': tipo_esperado,
                'dimensiones': imagen.shape[:2],
                'timestamp': self._timestamp()
            }
            
            # Detectar tipo de gráfica si es automático
            if tipo_esperado == "auto":
                tipo_detectado = self._detectar_tipo_grafica(imagen)
                resultado['tipo_detectado'] = tipo_detectado
            else:
                resultado['tipo_detectado'] = tipo_esperado
            
            # Análisis específico según tipo
            tipo = resultado['tipo_detectado']
            
            if tipo == 'funcion':
                resultado.update(self._analizar_funcion(imagen))
            elif tipo == 'geometria':
                resultado.update(self._analizar_geometria(imagen))
            elif tipo == 'diagrama':
                resultado.update(self._analizar_diagrama(imagen))
            else:
                resultado.update(self._analizar_general(imagen))
            
            # Análisis común para todos los tipos
            resultado.update(self._analizar_elementos_comunes(imagen))
            
            self.logger.info(f"✅ Análisis completado: tipo={tipo}")
            return resultado
            
        except Exception as e:
            self.logger.error(f"❌ Error en análisis: {e}")
            return {'exitoso': False, 'error': str(e)}
    
    def _cargar_imagen(self, ruta: str) -> Optional[np.ndarray]:
        """Cargar y preprocesar imagen."""
        try:
            imagen = cv2.imread(ruta)
            if imagen is None:
                return None
            
            # Redimensionar si es necesario
            h, w = imagen.shape[:2]
            max_w, max_h = self.resolucion_analisis
            
            if w > max_w or h > max_h:
                factor = min(max_w/w, max_h/h)
                nuevo_w, nuevo_h = int(w*factor), int(h*factor)
                imagen = cv2.resize(imagen, (nuevo_w, nuevo_h))
            
            return imagen
            
        except Exception as e:
            self.logger.error(f"Error cargando imagen: {e}")
            return None
    
    def _detectar_tipo_grafica(self, imagen: np.ndarray) -> str:
        """
        🎯 Detectar automáticamente el tipo de gráfica.
        
        Args:
            imagen: Imagen a analizar
            
        Returns:
            Tipo detectado ('funcion', 'geometria', 'diagrama', 'general')
        """
        # Convertir a escala de grises
        gris = cv2.cvtColor(imagen, cv2.COLOR_BGR2GRAY)
        
        # Detectar líneas
        edges = cv2.Canny(gris, 50, 150)
        lineas = cv2.HoughLinesP(edges, 1, np.pi/180, threshold=50, 
                                minLineLength=30, maxLineGap=10)
        
        num_lineas = len(lineas) if lineas is not None else 0
        
        # Detectar círculos
        circulos = cv2.HoughCircles(gris, cv2.HOUGH_GRADIENT, 1, 20,
                                   param1=50, param2=30, minRadius=10, maxRadius=100)
        
        num_circulos = len(circulos[0]) if circulos is not None else 0
        
        # Detectar contornos
        contornos, _ = cv2.findContours(edges, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
        num_contornos = len(contornos)
        
        # Heurísticas de clasificación
        if num_lineas > 20 and num_circulos < 3:
            # Muchas líneas, pocos círculos -> probablemente función
            return 'funcion'
        elif num_circulos > 2 or (num_lineas < 10 and num_contornos > 5):
            # Círculos o formas geométricas -> geometría
            return 'geometria'
        elif num_lineas > 5 and num_contornos > 3:
            # Líneas y formas -> diagrama
            return 'diagrama'
        else:
            return 'general'
    
    def _analizar_funcion(self, imagen: np.ndarray) -> Dict[str, Any]:
        """Análisis específico para gráficas de funciones."""
        resultado = {'tipo_analisis': 'funcion'}
        
        # Detectar ejes coordenados
        ejes = self._detectar_ejes(imagen)
        resultado['ejes'] = ejes
        
        # Detectar curvas
        curvas = self._detectar_curvas(imagen)
        resultado['curvas'] = curvas
        
        # Detectar puntos importantes
        puntos = self._detectar_puntos_importantes(imagen)
        resultado['puntos_importantes'] = puntos
        
        # Estimar dominio y rango
        if ejes.get('detectados', False):
            dominio, rango = self._estimar_dominio_rango(imagen, ejes)
            resultado['dominio_estimado'] = dominio
            resultado['rango_estimado'] = rango
        
        return resultado
    
    def _analizar_geometria(self, imagen: np.ndarray) -> Dict[str, Any]:
        """Análisis específico para figuras geométricas."""
        resultado = {'tipo_analisis': 'geometria'}
        
        # Detectar formas geométricas
        formas = self._detectar_formas_geometricas(imagen)
        resultado['formas'] = formas
        
        # Detectar ángulos
        angulos = self._detectar_angulos(imagen)
        resultado['angulos'] = angulos
        
        # Detectar medidas y etiquetas
        medidas = self._detectar_medidas(imagen)
        resultado['medidas'] = medidas
        
        return resultado
    
    def _analizar_diagrama(self, imagen: np.ndarray) -> Dict[str, Any]:
        """Análisis específico para diagramas."""
        resultado = {'tipo_analisis': 'diagrama'}
        
        # Detectar elementos conectados
        conexiones = self._detectar_conexiones(imagen)
        resultado['conexiones'] = conexiones
        
        # Detectar nodos/vértices
        nodos = self._detectar_nodos(imagen)
        resultado['nodos'] = nodos
        
        # Detectar flujo direccional
        direcciones = self._detectar_direcciones(imagen)
        resultado['direcciones'] = direcciones
        
        return resultado
    
    def _analizar_general(self, imagen: np.ndarray) -> Dict[str, Any]:
        """Análisis general para imágenes no clasificadas."""
        resultado = {'tipo_analisis': 'general'}
        
        # Análisis básico de elementos
        elementos = self._detectar_elementos_basicos(imagen)
        resultado['elementos_basicos'] = elementos
        
        return resultado
    
    def _analizar_elementos_comunes(self, imagen: np.ndarray) -> Dict[str, Any]:
        """Análisis de elementos comunes a todos los tipos."""
        resultado = {}
        
        # Detectar texto y etiquetas
        texto = self._detectar_texto(imagen)
        resultado['texto_detectado'] = texto
        
        # Análisis de colores
        colores = self._analizar_colores(imagen)
        resultado['colores_principales'] = colores
        
        # Detectar cuadrícula
        cuadricula = self._detectar_cuadricula(imagen)
        resultado['cuadricula'] = cuadricula
        
        # Calcular complejidad visual
        complejidad = self._calcular_complejidad_visual(imagen)
        resultado['complejidad_visual'] = complejidad
        
        return resultado
    
    def _detectar_ejes(self, imagen: np.ndarray) -> Dict[str, Any]:
        """Detectar ejes coordenados en la imagen."""
        gris = cv2.cvtColor(imagen, cv2.COLOR_BGR2GRAY)
        edges = cv2.Canny(gris, 50, 150)
        
        # Detectar líneas horizontales y verticales
        lineas = cv2.HoughLinesP(edges, 1, np.pi/180, threshold=50,
                                minLineLength=100, maxLineGap=10)
        
        ejes_h = []
        ejes_v = []
        
        if lineas is not None:
            for linea in lineas:
                x1, y1, x2, y2 = linea[0]
                
                # Clasificar como horizontal o vertical
                if abs(y2 - y1) < 10:  # Línea horizontal
                    ejes_h.append(((x1, y1), (x2, y2)))
                elif abs(x2 - x1) < 10:  # Línea vertical
                    ejes_v.append(((x1, y1), (x2, y2)))
        
        return {
            'detectados': len(ejes_h) > 0 and len(ejes_v) > 0,
            'horizontales': ejes_h,
            'verticales': ejes_v,
            'origen_estimado': self._estimar_origen(ejes_h, ejes_v)
        }
    
    def _detectar_curvas(self, imagen: np.ndarray) -> List[Dict[str, Any]]:
        """Detectar curvas en la imagen."""
        gris = cv2.cvtColor(imagen, cv2.COLOR_BGR2GRAY)
        edges = cv2.Canny(gris, 30, 100)
        
        # Encontrar contornos que podrían ser curvas
        contornos, _ = cv2.findContours(edges, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
        
        curvas = []
        for contorno in contornos:
            if len(contorno) > 20:  # Filtrar contornos muy pequeños
                # Aproximar curva
                epsilon = 0.02 * cv2.arcLength(contorno, True)
                approx = cv2.approxPolyDP(contorno, epsilon, True)
                
                curvas.append({
                    'puntos': contorno.reshape(-1, 2).tolist(),
                    'aproximacion': approx.reshape(-1, 2).tolist(),
                    'longitud': cv2.arcLength(contorno, False),
                    'area': cv2.contourArea(contorno)
                })
        
        return curvas
    
    def _detectar_puntos_importantes(self, imagen: np.ndarray) -> List[Tuple[int, int]]:
        """Detectar puntos importantes (intersecciones, máximos, mínimos)."""
        gris = cv2.cvtColor(imagen, cv2.COLOR_BGR2GRAY)
        
        # Detectar esquinas/puntos de interés
        corners = cv2.goodFeaturesToTrack(gris, maxCorners=20, qualityLevel=0.01,
                                         minDistance=10, blockSize=3)
        
        puntos = []
        if corners is not None:
            for corner in corners:
                x, y = corner.ravel()
                puntos.append((int(x), int(y)))
        
        return puntos
    
    def _detectar_formas_geometricas(self, imagen: np.ndarray) -> List[Dict[str, Any]]:
        """Detectar formas geométricas básicas."""
        gris = cv2.cvtColor(imagen, cv2.COLOR_BGR2GRAY)
        edges = cv2.Canny(gris, 50, 150)
        
        contornos, _ = cv2.findContours(edges, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
        
        formas = []
        for contorno in contornos:
            if cv2.contourArea(contorno) > 100:  # Filtrar formas muy pequeñas
                # Aproximar forma
                epsilon = 0.02 * cv2.arcLength(contorno, True)
                approx = cv2.approxPolyDP(contorno, epsilon, True)
                
                # Clasificar forma
                num_vertices = len(approx)
                if num_vertices == 3:
                    tipo = 'triangulo'
                elif num_vertices == 4:
                    tipo = 'cuadrilatero'
                elif num_vertices > 8:
                    tipo = 'circulo'
                else:
                    tipo = 'poligono'
                
                formas.append({
                    'tipo': tipo,
                    'vertices': approx.reshape(-1, 2).tolist(),
                    'area': cv2.contourArea(contorno),
                    'perimetro': cv2.arcLength(contorno, True)
                })
        
        return formas
    
    def _detectar_texto(self, imagen: np.ndarray) -> Dict[str, Any]:
        """Detectar regiones de texto en la imagen."""
        gris = cv2.cvtColor(imagen, cv2.COLOR_BGR2GRAY)
        
        # Detectar regiones de texto usando MSER
        mser = cv2.MSER_create()
        regions, _ = mser.detectRegions(gris)
        
        regiones_texto = []
        for region in regions:
            if len(region) > 10:  # Filtrar regiones muy pequeñas
                x, y, w, h = cv2.boundingRect(region.reshape(-1, 1, 2))
                regiones_texto.append({
                    'bbox': (x, y, w, h),
                    'area': w * h
                })
        
        return {
            'regiones_detectadas': len(regiones_texto),
            'regiones': regiones_texto[:10]  # Limitar a 10 regiones principales
        }
    
    def _analizar_colores(self, imagen: np.ndarray) -> List[Tuple[int, int, int]]:
        """Analizar colores principales en la imagen."""
        # Redimensionar para análisis más rápido
        small = cv2.resize(imagen, (150, 150))
        data = small.reshape((-1, 3))
        data = np.float32(data)
        
        # K-means para encontrar colores dominantes
        criteria = (cv2.TERM_CRITERIA_EPS + cv2.TERM_CRITERIA_MAX_ITER, 20, 1.0)
        k = 5
        _, labels, centers = cv2.kmeans(data, k, None, criteria, 10, cv2.KMEANS_RANDOM_CENTERS)
        
        # Convertir a lista de colores BGR
        colores = [tuple(map(int, center)) for center in centers]
        return colores
    
    def _detectar_cuadricula(self, imagen: np.ndarray) -> Dict[str, Any]:
        """Detectar presencia de cuadrícula en la imagen."""
        gris = cv2.cvtColor(imagen, cv2.COLOR_BGR2GRAY)
        
        # Detectar líneas finas (posible cuadrícula)
        edges = cv2.Canny(gris, 20, 50)
        lineas = cv2.HoughLinesP(edges, 1, np.pi/180, threshold=30,
                                minLineLength=20, maxLineGap=5)
        
        if lineas is None:
            return {'presente': False}
        
        # Contar líneas horizontales y verticales regulares
        h_lines = []
        v_lines = []
        
        for linea in lineas:
            x1, y1, x2, y2 = linea[0]
            if abs(y2 - y1) < 3:  # Horizontal
                h_lines.append(y1)
            elif abs(x2 - x1) < 3:  # Vertical
                v_lines.append(x1)
        
        # Verificar regularidad
        h_regular = self._verificar_regularidad(h_lines)
        v_regular = self._verificar_regularidad(v_lines)
        
        return {
            'presente': h_regular and v_regular and len(h_lines) > 3 and len(v_lines) > 3,
            'lineas_horizontales': len(h_lines),
            'lineas_verticales': len(v_lines),
            'regularidad_h': h_regular,
            'regularidad_v': v_regular
        }
    
    def _verificar_regularidad(self, valores: List[int]) -> bool:
        """Verificar si una lista de valores tiene espaciado regular."""
        if len(valores) < 3:
            return False
        
        valores = sorted(valores)
        diferencias = [valores[i+1] - valores[i] for i in range(len(valores)-1)]
        
        if not diferencias:
            return False
        
        promedio = sum(diferencias) / len(diferencias)
        varianza = sum((d - promedio)**2 for d in diferencias) / len(diferencias)
        
        # Considerar regular si la varianza es pequeña
        return varianza < (promedio * 0.3)**2
    
    def _calcular_complejidad_visual(self, imagen: np.ndarray) -> float:
        """Calcular métrica de complejidad visual."""
        gris = cv2.cvtColor(imagen, cv2.COLOR_BGR2GRAY)
        
        # Calcular gradientes
        grad_x = cv2.Sobel(gris, cv2.CV_64F, 1, 0, ksize=3)
        grad_y = cv2.Sobel(gris, cv2.CV_64F, 0, 1, ksize=3)
        
        # Magnitud del gradiente
        magnitud = np.sqrt(grad_x**2 + grad_y**2)
        
        # Complejidad como promedio de magnitudes normalizadas
        complejidad = np.mean(magnitud) / 255.0
        
        return min(complejidad, 1.0)
    
    def _timestamp(self) -> str:
        """Generar timestamp actual."""
        from datetime import datetime
        return datetime.now().isoformat()
    
    # Métodos auxiliares adicionales (implementación simplificada)
    def _estimar_origen(self, h_lines, v_lines):
        """Estimar punto de origen de ejes coordenados."""
        if not h_lines or not v_lines:
            return None
        
        # Buscar intersección más probable
        for h_line in h_lines:
            for v_line in v_lines:
                # Simplificado: usar primera intersección encontrada
                return (v_line[0][0], h_line[0][1])
        return None
    
    def _estimar_dominio_rango(self, imagen, ejes):
        """Estimar dominio y rango de función."""
        # Implementación simplificada
        h, w = imagen.shape[:2]
        return {'min': 0, 'max': w}, {'min': 0, 'max': h}
    
    def _detectar_angulos(self, imagen):
        """Detectar ángulos en figuras geométricas."""
        return []  # Implementación simplificada
    
    def _detectar_medidas(self, imagen):
        """Detectar medidas y dimensiones."""
        return []  # Implementación simplificada
    
    def _detectar_conexiones(self, imagen):
        """Detectar conexiones en diagramas."""
        return []  # Implementación simplificada
    
    def _detectar_nodos(self, imagen):
        """Detectar nodos en diagramas."""
        return []  # Implementación simplificada
    
    def _detectar_direcciones(self, imagen):
        """Detectar direcciones de flujo."""
        return []  # Implementación simplificada
    
    def _detectar_elementos_basicos(self, imagen):
        """Detectar elementos básicos."""
        return {'lineas': 0, 'formas': 0, 'texto': 0}  # Implementación simplificada
