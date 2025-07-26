#!/usr/bin/env python3
"""
Sistema de Análisis Visual con IA para Revisor-Rmd 2.0
=====================================================

Analiza gráficas generadas por código R/Python y detecta errores matemáticos
que solo son visibles en las representaciones visuales.
"""

import os
import sys
import cv2
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.image as mpimg
from PIL import Image, ImageEnhance
import pytesseract
import json
import re
from typing import Dict, List, Tuple, Any
import logging
import subprocess
import tempfile

# Configurar logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class VisualAnalyzer:
    """Analizador visual principal para gráficas matemáticas"""
    
    def __init__(self, config: Dict[str, Any] = None):
        self.config = config or {}
        self.temp_dir = self.config.get('temp_dir', '/tmp/revisor-visual')
        self.ensure_temp_dir()
        
    def ensure_temp_dir(self):
        """Crear directorio temporal si no existe"""
        os.makedirs(self.temp_dir, exist_ok=True)
        
    def analyze_rmd_file(self, rmd_path: str) -> Dict[str, Any]:
        """
        Analizar archivo .Rmd completo con análisis visual
        
        Args:
            rmd_path: Ruta al archivo .Rmd
            
        Returns:
            Diccionario con resultados del análisis
        """
        logger.info(f"Iniciando análisis visual de: {rmd_path}")
        
        results = {
            'file_path': rmd_path,
            'graphics_generated': [],
            'visual_errors': [],
            'mathematical_inconsistencies': [],
            'data_extraction': {},
            'summary': {}
        }
        
        try:
            # Paso 1: Ejecutar código y generar gráficas
            graphics_paths = self.execute_and_capture_graphics(rmd_path)
            results['graphics_generated'] = graphics_paths
            
            # Paso 2: Analizar cada gráfica
            for graphic_path in graphics_paths:
                graphic_analysis = self.analyze_single_graphic(graphic_path)
                results['visual_errors'].extend(graphic_analysis.get('errors', []))
                results['mathematical_inconsistencies'].extend(
                    graphic_analysis.get('math_errors', [])
                )
                results['data_extraction'][graphic_path] = graphic_analysis.get('extracted_data', {})
            
            # Paso 3: Validar coherencia global
            coherence_analysis = self.validate_global_coherence(results)
            results['mathematical_inconsistencies'].extend(coherence_analysis)
            
            # Paso 4: Generar resumen
            results['summary'] = self.generate_summary(results)
            
        except Exception as e:
            logger.error(f"Error en análisis visual: {e}")
            results['visual_errors'].append({
                'type': 'analysis_error',
                'message': f"Error interno: {str(e)}",
                'severity': 'error'
            })
            
        return results
    
    def execute_and_capture_graphics(self, rmd_path: str) -> List[str]:
        """
        Ejecutar código R/Python del archivo .Rmd y capturar gráficas generadas
        """
        logger.info("Ejecutando código y capturando gráficas...")
        
        # Crear directorio para gráficas de este archivo
        file_name = os.path.splitext(os.path.basename(rmd_path))[0]
        graphics_dir = os.path.join(self.temp_dir, f"graphics_{file_name}")
        os.makedirs(graphics_dir, exist_ok=True)
        
        # Leer contenido del archivo .Rmd
        with open(rmd_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Extraer chunks de código
        r_chunks = self.extract_r_chunks(content)
        
        graphics_paths = []
        
        # Ejecutar chunks R que generan gráficas
        for i, chunk in enumerate(r_chunks):
            if self.chunk_generates_graphics(chunk):
                try:
                    graphic_path = self.execute_r_chunk(chunk, graphics_dir, f"r_graphic_{i}")
                    if graphic_path and os.path.exists(graphic_path):
                        graphics_paths.append(graphic_path)
                        logger.info(f"Gráfica generada: {graphic_path}")
                except Exception as e:
                    logger.warning(f"Error ejecutando chunk R {i}: {e}")
        
        logger.info(f"Total gráficas generadas: {len(graphics_paths)}")
        return graphics_paths
    
    def extract_r_chunks(self, content: str) -> List[str]:
        """Extraer chunks de código R del contenido .Rmd"""
        pattern = r'```\{r[^}]*\}(.*?)```'
        matches = re.findall(pattern, content, re.DOTALL)
        return [match.strip() for match in matches if match.strip()]
    
    def chunk_generates_graphics(self, chunk_code: str) -> bool:
        """Determinar si un chunk genera gráficas"""
        graphics_indicators = [
            'include_tikz',
            'ggplot',
            'plot(',
            'barplot',
            'hist(',
            'pie(',
            'boxplot',
            'scatter',
            'matplotlib',
            'plt.'
        ]
        
        return any(indicator in chunk_code for indicator in graphics_indicators)
    
    def execute_r_chunk(self, chunk_code: str, output_dir: str, filename: str) -> str:
        """
        Ejecutar chunk de código R y capturar gráfica
        """
        # Crear script R temporal que capture gráficas
        r_script = f"""
# Configurar dispositivo gráfico
png_file <- "{output_dir}/{filename}.png"
png(png_file, width=800, height=600, res=150)

# Configurar librerías necesarias
suppressPackageStartupMessages({{
    library(exams)
    library(datasets)
    library(readxl)
    library(data.table)
    library(reticulate)
    library(digest)
    library(testthat)
}})

# Configurar variables globales comunes
typ <- "png"
options(scipen=999)

# Código del chunk
tryCatch({{
    {chunk_code}
}}, error = function(e) {{
    cat("Error en chunk:", e$message, "\\n")
}})

# Cerrar dispositivo
dev.off()

# Verificar si se generó la imagen
if (file.exists(png_file)) {{
    cat("SUCCESS:", png_file, "\\n")
}} else {{
    cat("NO_OUTPUT\\n")
}}
"""
        
        script_path = os.path.join(self.temp_dir, f"{filename}.R")
        with open(script_path, 'w') as f:
            f.write(r_script)
        
        # Ejecutar script R
        try:
            result = subprocess.run(['Rscript', script_path], 
                                  capture_output=True, text=True, timeout=60)
            
            graphic_path = os.path.join(output_dir, f"{filename}.png")
            
            # Verificar si se generó la gráfica
            if "SUCCESS:" in result.stdout and os.path.exists(graphic_path):
                return graphic_path
            else:
                logger.debug(f"No se generó gráfica para chunk. Output: {result.stdout}")
                
        except subprocess.TimeoutExpired:
            logger.warning(f"Timeout ejecutando chunk R")
        except Exception as e:
            logger.warning(f"Error ejecutando R: {e}")
        
        return None
    
    def analyze_single_graphic(self, graphic_path: str) -> Dict[str, Any]:
        """
        Analizar una gráfica individual
        """
        logger.info(f"Analizando gráfica: {os.path.basename(graphic_path)}")
        
        analysis = {
            'graphic_path': graphic_path,
            'extracted_data': {},
            'errors': [],
            'math_errors': [],
            'visual_properties': {}
        }
        
        try:
            # Cargar imagen
            image = cv2.imread(graphic_path)
            if image is None:
                analysis['errors'].append({
                    'type': 'image_load_error',
                    'message': f"No se pudo cargar la imagen: {graphic_path}",
                    'severity': 'error'
                })
                return analysis
            
            # Análisis visual básico
            analysis['visual_properties'] = self.extract_visual_properties(image)
            
            # Extracción de texto (OCR)
            extracted_text = self.extract_text_from_image(graphic_path)
            analysis['extracted_data']['text'] = extracted_text
            
            # Extracción de datos numéricos
            numeric_data = self.extract_numeric_data(extracted_text)
            analysis['extracted_data']['numbers'] = numeric_data
            
            # Análisis de coherencia matemática
            math_analysis = self.analyze_mathematical_coherence(numeric_data)
            analysis['math_errors'].extend(math_analysis)
            
            # Análisis específico por tipo de gráfica
            chart_type = self.detect_chart_type(image)
            analysis['visual_properties']['chart_type'] = chart_type
            
        except Exception as e:
            logger.error(f"Error analizando gráfica {graphic_path}: {e}")
            analysis['errors'].append({
                'type': 'analysis_error',
                'message': f"Error en análisis: {str(e)}",
                'severity': 'error'
            })
        
        return analysis
    
    def extract_visual_properties(self, image: np.ndarray) -> Dict[str, Any]:
        """Extraer propiedades visuales básicas de la imagen"""
        height, width = image.shape[:2]
        
        return {
            'dimensions': {'width': width, 'height': height},
            'aspect_ratio': width / height,
            'color_channels': image.shape[2] if len(image.shape) > 2 else 1
        }
    
    def extract_text_from_image(self, image_path: str) -> List[str]:
        """Extraer texto de la imagen usando OCR"""
        try:
            # Mejorar imagen para OCR
            image = Image.open(image_path)
            enhancer = ImageEnhance.Contrast(image)
            image = enhancer.enhance(2.0)
            
            # Aplicar OCR
            text = pytesseract.image_to_string(image, lang='spa+eng')
            lines = [line.strip() for line in text.split('\n') if line.strip()]
            
            return lines
        except Exception as e:
            logger.warning(f"Error en OCR: {e}")
            return []
    
    def extract_numeric_data(self, text_lines: List[str]) -> List[float]:
        """Extraer datos numéricos del texto extraído"""
        numbers = []
        
        for line in text_lines:
            # Buscar números (enteros y decimales, incluyendo porcentajes)
            number_pattern = r'-?\d+\.?\d*%?'
            matches = re.findall(number_pattern, line)
            
            for match in matches:
                try:
                    # Remover símbolo de porcentaje si existe
                    clean_number = match.replace('%', '')
                    numbers.append(float(clean_number))
                except ValueError:
                    continue
        
        return numbers
    
    def analyze_mathematical_coherence(self, numbers: List[float]) -> List[Dict[str, Any]]:
        """Analizar coherencia matemática de los números extraídos"""
        errors = []
        
        if not numbers:
            return errors
        
        # Verificar si parecen ser porcentajes
        potential_percentages = [n for n in numbers if 0 <= n <= 100]
        
        if len(potential_percentages) >= 3:
            total = sum(potential_percentages)
            if abs(total - 100) > 2:  # Tolerancia de 2%
                errors.append({
                    'type': 'percentage_sum_error',
                    'message': f"CRÍTICO: Los porcentajes no suman 100%: {total:.1f}%",
                    'severity': 'error',
                    'values': potential_percentages
                })
        
        return errors
    
    def detect_chart_type(self, image: np.ndarray) -> str:
        """Detectar tipo de gráfica usando análisis de formas"""
        # Convertir a escala de grises
        gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
        
        # Detectar contornos
        contours, _ = cv2.findContours(gray, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
        
        # Análisis básico de formas
        rectangles = 0
        circles = 0
        
        for contour in contours:
            area = cv2.contourArea(contour)
            if area < 100:  # Ignorar contornos muy pequeños
                continue
            
            # Aproximar contorno
            epsilon = 0.02 * cv2.arcLength(contour, True)
            approx = cv2.approxPolyDP(contour, epsilon, True)
            
            if len(approx) == 4:
                rectangles += 1
            elif len(approx) > 8:
                circles += 1
        
        # Clasificación simple
        if rectangles > circles:
            return 'bar_chart'
        elif circles > 0:
            return 'pie_chart'
        else:
            return 'unknown'
    
    def validate_global_coherence(self, results: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Validar coherencia global entre todas las gráficas"""
        errors = []
        
        # Si hay múltiples gráficas, verificar consistencia
        if len(results['graphics_generated']) > 1:
            # TODO: Implementar validaciones más sofisticadas
            pass
        
        return errors
    
    def generate_summary(self, results: Dict[str, Any]) -> Dict[str, Any]:
        """Generar resumen del análisis"""
        return {
            'total_graphics': len(results['graphics_generated']),
            'total_errors': len(results['visual_errors']),
            'total_math_errors': len(results['mathematical_inconsistencies']),
            'status': 'error' if results['visual_errors'] or results['mathematical_inconsistencies'] else 'success'
        }


def main():
    """Función principal para testing"""
    if len(sys.argv) != 2:
        print("Uso: python visual_analyzer.py <archivo.Rmd>")
        sys.exit(1)
    
    rmd_file = sys.argv[1]
    
    analyzer = VisualAnalyzer()
    results = analyzer.analyze_rmd_file(rmd_file)
    
    # Mostrar resultados
    print(json.dumps(results, indent=2, ensure_ascii=False))


if __name__ == "__main__":
    main()
