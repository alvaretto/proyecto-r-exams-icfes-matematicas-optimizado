#!/usr/bin/env python3
"""
🎨 Agente Graficador Experto TikZ - Motor Principal
==================================================

Agente especializado en análisis de imágenes matemáticas y generación 
de código TikZ de alta calidad, compatible con Qtikz/Ktikz.

Autor: Agente IA Especializado
Fecha: 2025-01-13
Versión: 1.0.0
"""

import os
import sys
import json
import logging
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Tuple, Any

# Importar módulos del agente
from analizador_imagenes import AnalizadorImagenes
from generador_tikz import GeneradorTikZ
from validador_codigo import ValidadorCodigo

class ResultadoProcesamiento:
    """Clase para encapsular resultados del procesamiento."""
    
    def __init__(self):
        self.imagen_original: str = ""
        self.analisis: Dict[str, Any] = {}
        self.codigo_tikz: str = ""
        self.errores: List[str] = []
        self.advertencias: List[str] = []
        self.metricas: Dict[str, float] = {}
        self.tiempo_procesamiento: float = 0.0
        self.iteraciones: int = 0
        self.validacion_exitosa: bool = False

class AgenteTikZ:
    """
    🎯 Agente Graficador Experto TikZ
    
    Motor principal que coordina el análisis de imágenes,
    generación de código TikZ y validación iterativa.
    """
    
    def __init__(self, directorio_base: str = None):
        """
        Inicializar el agente con configuración base.
        
        Args:
            directorio_base: Directorio raíz del agente
        """
        self.directorio_base = Path(directorio_base) if directorio_base else Path(__file__).parent
        self.configurar_logging()
        self.cargar_configuracion()
        self.inicializar_modulos()
        
        self.logger.info("🎨 Agente TikZ inicializado correctamente")
    
    def configurar_logging(self):
        """Configurar sistema de logging."""
        log_dir = self.directorio_base / "logs"
        log_dir.mkdir(exist_ok=True)
        
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        log_file = log_dir / f"agente_tikz_{timestamp}.log"
        
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
            handlers=[
                logging.FileHandler(log_file),
                logging.StreamHandler(sys.stdout)
            ]
        )
        self.logger = logging.getLogger("AgenteTikZ")
    
    def cargar_configuracion(self):
        """Cargar configuración del agente."""
        config_file = self.directorio_base / "config.json"
        
        # Configuración por defecto
        self.config = {
            "max_iteraciones": 5,
            "umbral_similitud": 0.95,
            "timeout_compilacion": 30,
            "templates_dir": "templates",
            "ejemplos_dir": "ejemplos",
            "validacion_automatica": True,
            "optimizacion_codigo": True
        }
        
        # Cargar configuración personalizada si existe
        if config_file.exists():
            try:
                with open(config_file, 'r', encoding='utf-8') as f:
                    config_custom = json.load(f)
                    self.config.update(config_custom)
                self.logger.info(f"✅ Configuración cargada desde {config_file}")
            except Exception as e:
                self.logger.warning(f"⚠️ Error cargando configuración: {e}")
    
    def inicializar_modulos(self):
        """Inicializar módulos especializados."""
        try:
            self.analizador = AnalizadorImagenes(self.config)
            self.generador = GeneradorTikZ(self.config, self.directorio_base)
            self.validador = ValidadorCodigo(self.config)
            self.logger.info("✅ Módulos especializados inicializados")
        except Exception as e:
            self.logger.error(f"❌ Error inicializando módulos: {e}")
            raise
    
    def procesar_imagen(self, ruta_imagen: str, 
                       descripcion: str = "", 
                       tipo_grafica: str = "auto") -> ResultadoProcesamiento:
        """
        🔍 Procesar una imagen y generar código TikZ.
        
        Args:
            ruta_imagen: Ruta a la imagen a procesar
            descripcion: Descripción opcional de la imagen
            tipo_grafica: Tipo de gráfica esperado ('auto', 'funcion', 'geometria', 'diagrama')
            
        Returns:
            ResultadoProcesamiento con todos los resultados
        """
        inicio = datetime.now()
        resultado = ResultadoProcesamiento()
        resultado.imagen_original = ruta_imagen
        
        self.logger.info(f"🎯 Iniciando procesamiento de: {ruta_imagen}")
        
        try:
            # 1. Análisis de la imagen
            self.logger.info("🔍 Fase 1: Análisis de imagen")
            resultado.analisis = self.analizador.analizar(ruta_imagen, descripcion, tipo_grafica)
            
            if not resultado.analisis.get('exitoso', False):
                resultado.errores.append("Error en análisis de imagen")
                return resultado
            
            # 2. Generación inicial de código TikZ
            self.logger.info("🛠️ Fase 2: Generación de código TikZ")
            resultado.codigo_tikz = self.generador.generar(resultado.analisis)
            
            if not resultado.codigo_tikz:
                resultado.errores.append("Error generando código TikZ")
                return resultado
            
            # 3. Validación y refinamiento iterativo
            self.logger.info("🔄 Fase 3: Validación iterativa")
            resultado = self._refinar_iterativamente(resultado)
            
            # 4. Métricas finales
            resultado.tiempo_procesamiento = (datetime.now() - inicio).total_seconds()
            resultado.metricas = self._calcular_metricas(resultado)
            
            self.logger.info(f"✅ Procesamiento completado en {resultado.tiempo_procesamiento:.2f}s")
            
        except Exception as e:
            self.logger.error(f"❌ Error en procesamiento: {e}")
            resultado.errores.append(str(e))
        
        return resultado
    
    def _refinar_iterativamente(self, resultado: ResultadoProcesamiento) -> ResultadoProcesamiento:
        """
        🔄 Refinar código TikZ iterativamente hasta satisfacer criterios.
        
        Args:
            resultado: Resultado parcial a refinar
            
        Returns:
            Resultado refinado
        """
        max_iter = self.config.get('max_iteraciones', 5)
        
        for iteracion in range(1, max_iter + 1):
            self.logger.info(f"🔄 Iteración {iteracion}/{max_iter}")
            resultado.iteraciones = iteracion
            
            # Validar código actual
            validacion = self.validador.validar(resultado.codigo_tikz, resultado.imagen_original)
            
            if validacion.get('exitoso', False):
                similitud = validacion.get('similitud', 0.0)
                umbral = self.config.get('umbral_similitud', 0.95)
                
                if similitud >= umbral:
                    resultado.validacion_exitosa = True
                    self.logger.info(f"✅ Validación exitosa (similitud: {similitud:.3f})")
                    break
                else:
                    self.logger.info(f"🔄 Similitud insuficiente: {similitud:.3f} < {umbral}")
            
            # Refinar código basado en errores detectados
            errores_detectados = validacion.get('errores', [])
            if errores_detectados:
                resultado.codigo_tikz = self.generador.refinar(
                    resultado.codigo_tikz, 
                    errores_detectados,
                    resultado.analisis
                )
            else:
                # Si no hay errores específicos, optimizar código
                resultado.codigo_tikz = self.generador.optimizar(resultado.codigo_tikz)
        
        if not resultado.validacion_exitosa:
            resultado.advertencias.append(f"No se alcanzó validación exitosa en {max_iter} iteraciones")
        
        return resultado
    
    def _calcular_metricas(self, resultado: ResultadoProcesamiento) -> Dict[str, float]:
        """Calcular métricas de calidad del resultado."""
        metricas = {
            'tiempo_procesamiento': resultado.tiempo_procesamiento,
            'iteraciones_usadas': resultado.iteraciones,
            'lineas_codigo': len(resultado.codigo_tikz.split('\n')),
            'complejidad_estimada': self._estimar_complejidad(resultado.codigo_tikz),
            'validacion_exitosa': 1.0 if resultado.validacion_exitosa else 0.0
        }
        
        return metricas
    
    def _estimar_complejidad(self, codigo: str) -> float:
        """Estimar complejidad del código TikZ generado."""
        # Métricas simples de complejidad
        lineas = len(codigo.split('\n'))
        comandos_draw = codigo.count('\\draw')
        comandos_fill = codigo.count('\\fill')
        comandos_node = codigo.count('\\node')
        
        complejidad = (lineas * 0.1 + comandos_draw * 0.3 + 
                      comandos_fill * 0.2 + comandos_node * 0.4)
        
        return min(complejidad / 10.0, 10.0)  # Normalizar a escala 0-10
    
    def procesar_lote(self, directorio_entrada: str, directorio_salida: str) -> List[ResultadoProcesamiento]:
        """
        📁 Procesar múltiples imágenes en lote.
        
        Args:
            directorio_entrada: Directorio con imágenes
            directorio_salida: Directorio para resultados
            
        Returns:
            Lista de resultados de procesamiento
        """
        entrada = Path(directorio_entrada)
        salida = Path(directorio_salida)
        salida.mkdir(exist_ok=True)
        
        extensiones_validas = {'.png', '.jpg', '.jpeg', '.bmp', '.tiff'}
        imagenes = [f for f in entrada.iterdir() 
                   if f.suffix.lower() in extensiones_validas]
        
        resultados = []
        
        self.logger.info(f"📁 Procesando lote: {len(imagenes)} imágenes")
        
        for i, imagen in enumerate(imagenes, 1):
            self.logger.info(f"📷 Procesando {i}/{len(imagenes)}: {imagen.name}")
            
            resultado = self.procesar_imagen(str(imagen))
            resultados.append(resultado)
            
            # Guardar resultado
            if resultado.codigo_tikz:
                archivo_salida = salida / f"{imagen.stem}.tikz"
                with open(archivo_salida, 'w', encoding='utf-8') as f:
                    f.write(resultado.codigo_tikz)
                
                # Guardar metadatos
                metadata_file = salida / f"{imagen.stem}_metadata.json"
                metadata = {
                    'imagen_original': str(imagen),
                    'tiempo_procesamiento': resultado.tiempo_procesamiento,
                    'iteraciones': resultado.iteraciones,
                    'validacion_exitosa': resultado.validacion_exitosa,
                    'errores': resultado.errores,
                    'advertencias': resultado.advertencias,
                    'metricas': resultado.metricas
                }
                
                with open(metadata_file, 'w', encoding='utf-8') as f:
                    json.dump(metadata, f, indent=2, ensure_ascii=False)
        
        self.logger.info(f"✅ Lote completado: {len(resultados)} resultados")
        return resultados
    
    def generar_reporte(self, resultados: List[ResultadoProcesamiento], 
                       archivo_reporte: str = None) -> str:
        """
        📊 Generar reporte de procesamiento.
        
        Args:
            resultados: Lista de resultados a reportar
            archivo_reporte: Archivo donde guardar el reporte
            
        Returns:
            Contenido del reporte
        """
        if not archivo_reporte:
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            archivo_reporte = f"reporte_agente_tikz_{timestamp}.md"
        
        # Calcular estadísticas
        total = len(resultados)
        exitosos = sum(1 for r in resultados if r.validacion_exitosa)
        tiempo_total = sum(r.tiempo_procesamiento for r in resultados)
        tiempo_promedio = tiempo_total / total if total > 0 else 0
        
        # Generar contenido del reporte
        reporte = f"""# 📊 Reporte Agente Graficador TikZ

## 📈 Estadísticas Generales

- **Total de imágenes procesadas:** {total}
- **Procesamiento exitoso:** {exitosos} ({exitosos/total*100:.1f}%)
- **Tiempo total:** {tiempo_total:.2f} segundos
- **Tiempo promedio:** {tiempo_promedio:.2f} segundos por imagen

## 🎯 Resultados Detallados

"""
        
        for i, resultado in enumerate(resultados, 1):
            estado = "✅ Exitoso" if resultado.validacion_exitosa else "❌ Fallido"
            reporte += f"""### Imagen {i}: {Path(resultado.imagen_original).name}

- **Estado:** {estado}
- **Tiempo:** {resultado.tiempo_procesamiento:.2f}s
- **Iteraciones:** {resultado.iteraciones}
- **Líneas de código:** {resultado.metricas.get('lineas_codigo', 'N/A')}
- **Errores:** {len(resultado.errores)}
- **Advertencias:** {len(resultado.advertencias)}

"""
        
        # Guardar reporte si se especifica archivo
        if archivo_reporte:
            with open(archivo_reporte, 'w', encoding='utf-8') as f:
                f.write(reporte)
            self.logger.info(f"📊 Reporte guardado en: {archivo_reporte}")
        
        return reporte

if __name__ == "__main__":
    # Ejemplo de uso
    agente = AgenteTikZ()
    print("🎨 Agente Graficador Experto TikZ v1.0.0")
    print("Listo para procesar imágenes matemáticas!")
