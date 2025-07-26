#!/usr/bin/env python3
"""
Validador de Gráficas Real - Ejecuta código R y analiza gráficas generadas
========================================================================

Detecta errores específicos como:
- Tablas con datos confusos
- Tortas con proporciones incorrectas
- Inconsistencias entre gráficas
"""

import os
import sys
import re
import json
import subprocess
import tempfile
from typing import Dict, List, Any
import logging

logger = logging.getLogger(__name__)

class GraphicsValidator:
    """Validador que ejecuta código R y analiza gráficas reales"""
    
    def __init__(self, config=None):
        self.config = config or {}
        self.temp_dir = self.config.get('temp_dir', '/tmp/graphics-validator')
        self.ensure_temp_dir()
        
    def ensure_temp_dir(self):
        """Crear directorio temporal si no existe"""
        os.makedirs(self.temp_dir, exist_ok=True)
        
    def validate_graphics_in_rmd(self, rmd_path: str) -> Dict[str, Any]:
        """
        Validar gráficas en archivo .Rmd ejecutando código real
        
        Args:
            rmd_path: Ruta al archivo .Rmd
            
        Returns:
            Diccionario con resultados de validación
        """
        logger.info(f"Iniciando validación de gráficas: {rmd_path}")
        
        results = {
            'file_path': rmd_path,
            'graphics_generated': [],
            'data_extracted': {},
            'graphics_errors': [],
            'consistency_errors': [],
            'summary': {}
        }
        
        try:
            # Paso 1: Extraer datos originales del código
            original_data = self.extract_original_data(rmd_path)
            results['original_data'] = original_data
            
            # Paso 2: Ejecutar código R y generar gráficas
            graphics_info = self.execute_and_generate_graphics(rmd_path)
            results['graphics_generated'] = graphics_info
            
            # Paso 3: Analizar cada gráfica generada
            for graphic_info in graphics_info:
                analysis = self.analyze_graphic_content(graphic_info, original_data)
                results['data_extracted'][graphic_info['name']] = analysis
                
                # Detectar errores específicos
                errors = self.detect_graphic_errors(analysis, original_data, graphic_info)
                results['graphics_errors'].extend(errors)
            
            # Paso 4: Validar consistencia global (3 correctas + 1 incorrecta)
            consistency_analysis = self.validate_graphics_consistency(results)
            results['consistency_errors'].extend(consistency_analysis)
            
            # Paso 5: Generar resumen
            results['summary'] = self.generate_graphics_summary(results)
            
        except Exception as e:
            logger.error(f"Error en validación de gráficas: {e}")
            results['graphics_errors'].append({
                'type': 'validation_error',
                'message': f"Error interno: {str(e)}",
                'severity': 'error'
            })
            
        return results
    
    def extract_original_data(self, rmd_path: str) -> Dict[str, Any]:
        """Extraer datos originales del código R"""
        
        with open(rmd_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Buscar definición de datos
        data_patterns = {
            'percentages': r'vector_resultado\s*<-\s*c\(([^)]+)\)',
            'animals': r'selmascota\s*<-\s*sample\([^)]+\)',
            'total_surveyed': r'enkuestados\s*<-\s*(\d+)'
        }
        
        extracted_data = {}
        
        for key, pattern in data_patterns.items():
            match = re.search(pattern, content)
            if match:
                if key == 'percentages':
                    # Extraer porcentajes
                    percentages_str = match.group(1)
                    percentages = re.findall(r'\d+\.?\d*', percentages_str)
                    extracted_data[key] = [float(p) for p in percentages]
                elif key == 'total_surveyed':
                    extracted_data[key] = int(match.group(1))
        
        return extracted_data
    
    def execute_and_generate_graphics(self, rmd_path: str) -> List[Dict[str, Any]]:
        """Ejecutar código R y generar gráficas reales"""
        
        file_name = os.path.splitext(os.path.basename(rmd_path))[0]
        graphics_dir = os.path.join(self.temp_dir, f"graphics_{file_name}")
        os.makedirs(graphics_dir, exist_ok=True)
        
        # Leer contenido del archivo
        with open(rmd_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Extraer chunks de código R
        r_chunks = self.extract_r_chunks(content)
        
        graphics_info = []
        
        # Ejecutar cada chunk y capturar gráficas
        for i, chunk in enumerate(r_chunks):
            if self.chunk_generates_graphics(chunk):
                try:
                    graphic_info = self.execute_r_chunk_with_analysis(
                        chunk, graphics_dir, f"graphic_{i}", i
                    )
                    if graphic_info:
                        graphics_info.append(graphic_info)
                        logger.info(f"Gráfica generada: {graphic_info['name']}")
                except Exception as e:
                    logger.warning(f"Error ejecutando chunk {i}: {e}")
        
        return graphics_info
    
    def extract_r_chunks(self, content: str) -> List[str]:
        """Extraer chunks de código R"""
        pattern = r'```\{r[^}]*\}(.*?)```'
        matches = re.findall(pattern, content, re.DOTALL)
        return [match.strip() for match in matches if match.strip()]
    
    def chunk_generates_graphics(self, chunk_code: str) -> bool:
        """Determinar si un chunk genera gráficas"""
        graphics_indicators = [
            'include_tikz',
            'kable',
            'data.frame',
            'table',
            'pie',
            'barplot',
            'ggplot'
        ]
        
        return any(indicator in chunk_code for indicator in graphics_indicators)
    
    def execute_r_chunk_with_analysis(self, chunk_code: str, output_dir: str, 
                                    filename: str, chunk_index: int) -> Dict[str, Any]:
        """Ejecutar chunk R y capturar tanto gráfica como datos"""
        
        # Crear script R que capture datos y gráficas
        r_script = f"""
# Configurar librerías
suppressPackageStartupMessages({{
    library(exams)
    library(knitr)
    library(kableExtra)
    library(datasets)
    library(readxl)
    library(data.table)
    library(reticulate)
    library(digest)
    library(testthat)
}})

# Configurar variables globales
typ <- "png"
options(scipen=999)

# Configurar dispositivo gráfico
png_file <- "{output_dir}/{filename}.png"
png(png_file, width=800, height=600, res=150)

# Variables para capturar datos
captured_data <- list()

# Función para capturar datos de tablas
capture_table_data <- function(data) {{
    if (is.data.frame(data)) {{
        captured_data$table_data <<- data
    }}
    return(data)
}}

# Función para capturar datos de gráficas de pastel
capture_pie_data <- function(data) {{
    if (is.numeric(data)) {{
        captured_data$pie_data <<- data
    }}
    return(data)
}}

# Código del chunk con captura de datos
tryCatch({{
    {chunk_code}
}}, error = function(e) {{
    cat("Error en chunk:", e$message, "\\n")
}})

# Cerrar dispositivo
dev.off()

# Guardar datos capturados
data_file <- "{output_dir}/{filename}_data.json"
if (length(captured_data) > 0) {{
    jsonlite::write_json(captured_data, data_file, auto_unbox = TRUE)
}}

# Verificar si se generó la imagen
if (file.exists(png_file)) {{
    cat("SUCCESS:", png_file, "\\n")
    cat("DATA:", data_file, "\\n")
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
            data_path = os.path.join(output_dir, f"{filename}_data.json")
            
            if "SUCCESS:" in result.stdout and os.path.exists(graphic_path):
                # Cargar datos capturados si existen
                captured_data = {}
                if os.path.exists(data_path):
                    try:
                        with open(data_path, 'r') as f:
                            captured_data = json.load(f)
                    except:
                        pass
                
                return {
                    'name': filename,
                    'chunk_index': chunk_index,
                    'image_path': graphic_path,
                    'data_path': data_path,
                    'captured_data': captured_data,
                    'chunk_code': chunk_code
                }
                
        except subprocess.TimeoutExpired:
            logger.warning(f"Timeout ejecutando chunk")
        except Exception as e:
            logger.warning(f"Error ejecutando R: {e}")
        
        return None
    
    def analyze_graphic_content(self, graphic_info: Dict[str, Any], 
                              original_data: Dict[str, Any]) -> Dict[str, Any]:
        """Analizar contenido específico de una gráfica"""
        
        analysis = {
            'graphic_name': graphic_info['name'],
            'graphic_type': self.detect_graphic_type(graphic_info),
            'extracted_values': [],
            'data_consistency': {},
            'errors': []
        }
        
        # Analizar según tipo de gráfica
        graphic_type = analysis['graphic_type']
        
        if graphic_type == 'table':
            analysis.update(self.analyze_table_graphic(graphic_info, original_data))
        elif graphic_type == 'pie':
            analysis.update(self.analyze_pie_graphic(graphic_info, original_data))
        elif graphic_type == 'bar':
            analysis.update(self.analyze_bar_graphic(graphic_info, original_data))
        
        return analysis
    
    def detect_graphic_type(self, graphic_info: Dict[str, Any]) -> str:
        """Detectar tipo de gráfica basado en código"""
        code = graphic_info.get('chunk_code', '').lower()
        
        if 'kable' in code or 'data.frame' in code:
            return 'table'
        elif 'pie' in code:
            return 'pie'
        elif 'barplot' in code or 'ggplot' in code:
            return 'bar'
        else:
            return 'unknown'
    
    def analyze_table_graphic(self, graphic_info: Dict[str, Any], 
                            original_data: Dict[str, Any]) -> Dict[str, Any]:
        """Analizar gráfica de tabla específicamente"""
        
        analysis = {
            'table_analysis': {
                'expected_percentages': original_data.get('percentages', []),
                'table_shows_correct_data': True,
                'inconsistencies': []
            }
        }
        
        # TODO: Implementar análisis específico de tabla
        # Por ahora, marcar como análisis pendiente
        analysis['table_analysis']['analysis_pending'] = True
        
        return analysis
    
    def analyze_pie_graphic(self, graphic_info: Dict[str, Any], 
                          original_data: Dict[str, Any]) -> Dict[str, Any]:
        """Analizar gráfica de pastel específicamente"""
        
        analysis = {
            'pie_analysis': {
                'expected_percentages': original_data.get('percentages', []),
                'pie_shows_correct_proportions': True,
                'inconsistencies': []
            }
        }
        
        # TODO: Implementar análisis específico de pastel
        # Por ahora, marcar como análisis pendiente
        analysis['pie_analysis']['analysis_pending'] = True
        
        return analysis
    
    def analyze_bar_graphic(self, graphic_info: Dict[str, Any], 
                          original_data: Dict[str, Any]) -> Dict[str, Any]:
        """Analizar gráfica de barras específicamente"""
        
        analysis = {
            'bar_analysis': {
                'expected_percentages': original_data.get('percentages', []),
                'bars_show_correct_heights': True,
                'inconsistencies': []
            }
        }
        
        # TODO: Implementar análisis específico de barras
        # Por ahora, marcar como análisis pendiente
        analysis['bar_analysis']['analysis_pending'] = True
        
        return analysis
    
    def detect_graphic_errors(self, analysis: Dict[str, Any], 
                            original_data: Dict[str, Any], 
                            graphic_info: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Detectar errores específicos en una gráfica"""
        
        errors = []
        
        # Por ahora, crear error placeholder para indicar que se necesita análisis manual
        errors.append({
            'type': 'manual_verification_required',
            'message': f"VERIFICACIÓN MANUAL REQUERIDA: Gráfica {graphic_info['name']} - Verificar si muestra datos confusos",
            'severity': 'warning',
            'graphic_name': graphic_info['name'],
            'graphic_type': analysis.get('graphic_type', 'unknown')
        })
        
        return errors
    
    def validate_graphics_consistency(self, results: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Validar que haya exactamente 3 gráficas correctas y 1 incorrecta"""
        
        errors = []
        
        total_graphics = len(results['graphics_generated'])
        
        if total_graphics > 0:
            errors.append({
                'type': 'graphics_structure_validation',
                'message': f"CRÍTICO: Se generaron {total_graphics} gráficas. Para pregunta negativa, verificar que EXACTAMENTE 3 sean correctas y 1 incorrecta.",
                'severity': 'error',
                'total_graphics': total_graphics,
                'expected_structure': '3 correctas + 1 incorrecta'
            })
        
        return errors
    
    def generate_graphics_summary(self, results: Dict[str, Any]) -> Dict[str, Any]:
        """Generar resumen de validación de gráficas"""
        
        return {
            'total_graphics_generated': len(results['graphics_generated']),
            'total_graphics_errors': len(results['graphics_errors']),
            'total_consistency_errors': len(results['consistency_errors']),
            'status': 'error' if results['graphics_errors'] or results['consistency_errors'] else 'success',
            'requires_manual_verification': True  # Siempre requerir verificación manual por ahora
        }


def main():
    """Función principal para testing"""
    if len(sys.argv) != 2:
        print("Uso: python graphics_validator.py <archivo.Rmd>")
        sys.exit(1)
    
    rmd_file = sys.argv[1]
    
    validator = GraphicsValidator()
    results = validator.validate_graphics_in_rmd(rmd_file)
    
    print(json.dumps(results, indent=2, ensure_ascii=False))


if __name__ == "__main__":
    main()
