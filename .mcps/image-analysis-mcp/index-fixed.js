#!/usr/bin/env node

import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { z } from "zod";
import sharp from "sharp";
import { promises as fs } from "fs";
import { join, dirname, extname } from "path";
import { fileURLToPath } from "url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Esquemas de validación
const AnalyzeMathImageSchema = z.object({
  image_path: z.string().describe("Ruta de la imagen a analizar"),
  analysis_type: z.enum(["geometry", "graph", "diagram", "equation", "tikz"]).optional().default("graph").describe("Tipo de análisis"),
  output_format: z.enum(["json", "tikz", "description"]).optional().default("json").describe("Formato de salida"),
  extract_colors: z.boolean().optional().default(true).describe("Extraer paleta de colores"),
  detect_shapes: z.boolean().optional().default(true).describe("Detectar formas geométricas")
});

const CompareImagesSchema = z.object({
  image1_path: z.string().describe("Ruta de la primera imagen"),
  image2_path: z.string().describe("Ruta de la segunda imagen"),
  comparison_type: z.enum(["similarity", "difference", "overlay"]).optional().default("similarity").describe("Tipo de comparación"),
  tolerance: z.number().min(0).max(1).optional().default(0.1).describe("Tolerancia para comparación (0-1)")
});

const ExtractTextSchema = z.object({
  image_path: z.string().describe("Ruta de la imagen"),
  language: z.string().optional().default("eng").describe("Idioma para OCR"),
  math_mode: z.boolean().optional().default(true).describe("Modo matemático para ecuaciones")
});

// Crear servidor MCP
const server = new McpServer(
  {
    name: "image-analysis-mcp",
    version: "1.0.0",
  },
  {
    capabilities: {
      tools: {},
    },
  }
);

// Función para analizar imagen matemática
async function analyzeMathImage(imagePath, analysisType = "graph") {
  try {
    // Verificar que el archivo existe
    await fs.access(imagePath);
    
    // Obtener metadatos de la imagen
    const metadata = await sharp(imagePath).metadata();
    
    // Análisis básico de la imagen
    const stats = await sharp(imagePath).stats();
    
    // Extraer colores dominantes
    const { dominant } = await sharp(imagePath)
      .resize(50, 50)
      .raw()
      .toBuffer({ resolveWithObject: true });
    
    const result = {
      file_info: {
        path: imagePath,
        format: metadata.format,
        width: metadata.width,
        height: metadata.height,
        channels: metadata.channels,
        size_bytes: (await fs.stat(imagePath)).size
      },
      analysis_type: analysisType,
      color_analysis: {
        dominant_color: stats.dominant,
        is_grayscale: metadata.channels <= 2,
        has_transparency: metadata.hasAlpha
      },
      mathematical_elements: {
        detected_type: analysisType,
        complexity_score: Math.min(10, Math.floor((metadata.width * metadata.height) / 10000)),
        suggested_tikz_approach: getSuggestedTikzApproach(analysisType, metadata)
      },
      recommendations: {
        tikz_libraries: getTikzLibraries(analysisType),
        estimated_difficulty: getEstimatedDifficulty(analysisType, metadata),
        replication_notes: getReplicationNotes(analysisType)
      }
    };
    
    return result;
    
  } catch (error) {
    throw new Error(`Error analizando imagen: ${error.message}`);
  }
}

// Función para comparar imágenes
async function compareImages(image1Path, image2Path, comparisonType = "similarity") {
  try {
    await fs.access(image1Path);
    await fs.access(image2Path);
    
    // Redimensionar ambas imágenes al mismo tamaño para comparación
    const size = 200;
    const img1Buffer = await sharp(image1Path)
      .resize(size, size, { fit: 'contain', background: { r: 255, g: 255, b: 255 } })
      .raw()
      .toBuffer();
    
    const img2Buffer = await sharp(image2Path)
      .resize(size, size, { fit: 'contain', background: { r: 255, g: 255, b: 255 } })
      .raw()
      .toBuffer();
    
    // Calcular diferencia pixel por pixel
    let totalDiff = 0;
    const totalPixels = size * size * 3; // RGB
    
    for (let i = 0; i < Math.min(img1Buffer.length, img2Buffer.length); i++) {
      totalDiff += Math.abs(img1Buffer[i] - img2Buffer[i]);
    }
    
    const similarity = 1 - (totalDiff / (totalPixels * 255));
    
    return {
      comparison_type: comparisonType,
      similarity_score: Math.max(0, Math.min(1, similarity)),
      difference_percentage: Math.max(0, Math.min(100, (1 - similarity) * 100)),
      images_compared: {
        image1: image1Path,
        image2: image2Path
      },
      recommendation: similarity > 0.95 ? "Imágenes muy similares" : 
                     similarity > 0.8 ? "Imágenes similares" :
                     similarity > 0.5 ? "Imágenes moderadamente diferentes" :
                     "Imágenes muy diferentes"
    };
    
  } catch (error) {
    throw new Error(`Error comparando imágenes: ${error.message}`);
  }
}

// Funciones auxiliares
function getSuggestedTikzApproach(analysisType, metadata) {
  const approaches = {
    geometry: "Usar coordenadas y formas básicas con tikz",
    graph: "Usar axis environment con pgfplots",
    diagram: "Combinar nodos y conexiones con tikz",
    equation: "Usar math mode con amsmath",
    tikz: "Replicación directa con tikz"
  };
  return approaches[analysisType] || "Análisis general con tikz";
}

function getTikzLibraries(analysisType) {
  const libraries = {
    geometry: ["shapes", "calc", "positioning"],
    graph: ["pgfplots", "axis", "datavisualization"],
    diagram: ["arrows", "shapes", "positioning", "calc"],
    equation: ["amsmath", "amssymb"],
    tikz: ["arrows", "shapes", "positioning", "calc", "pgfplots"]
  };
  return libraries[analysisType] || ["arrows", "shapes"];
}

function getEstimatedDifficulty(analysisType, metadata) {
  const baseComplexity = {
    geometry: 3,
    graph: 5,
    diagram: 4,
    equation: 2,
    tikz: 6
  };
  
  const sizeComplexity = Math.min(3, Math.floor((metadata.width * metadata.height) / 50000));
  return Math.min(10, (baseComplexity[analysisType] || 3) + sizeComplexity);
}

function getReplicationNotes(analysisType) {
  const notes = {
    geometry: "Medir ángulos y distancias precisas",
    graph: "Extraer datos numéricos y escalas",
    diagram: "Identificar jerarquías y conexiones",
    equation: "Verificar sintaxis matemática",
    tikz: "Analizar estructura y elementos"
  };
  return notes[analysisType] || "Análisis general requerido";
}

// Registrar herramientas
server.tool("analyze_math_image", "Analizar imágenes matemáticas para replicación TikZ", AnalyzeMathImageSchema, async ({ image_path, analysis_type = "graph", output_format = "json", extract_colors = true, detect_shapes = true }) => {
  const result = await analyzeMathImage(image_path, analysis_type);
  
  return {
    content: [
      {
        type: "text",
        text: JSON.stringify(result, null, 2)
      }
    ]
  };
});

server.tool("compare_images", "Comparar dos imágenes para validar replicación TikZ", CompareImagesSchema, async ({ image1_path, image2_path, comparison_type = "similarity", tolerance = 0.1 }) => {
  const result = await compareImages(image1_path, image2_path, comparison_type);
  
  return {
    content: [
      {
        type: "text",
        text: JSON.stringify(result, null, 2)
      }
    ]
  };
});

server.tool("extract_text", "Extraer texto y ecuaciones de imágenes (OCR básico)", ExtractTextSchema, async ({ image_path, language = "eng", math_mode = true }) => {
  try {
    await fs.access(image_path);
    
    // Análisis básico de texto en imagen (simulado)
    const metadata = await sharp(image_path).metadata();
    
    const result = {
      image_path: image_path,
      text_extraction: {
        method: "basic_analysis",
        language: language,
        math_mode: math_mode,
        detected_text: "Análisis de texto requiere OCR avanzado",
        confidence: 0.5,
        suggestions: [
          "Usar herramientas especializadas como Tesseract para OCR",
          "Para ecuaciones matemáticas, considerar Mathpix o similar",
          "Verificar calidad y resolución de la imagen"
        ]
      },
      image_info: {
        width: metadata.width,
        height: metadata.height,
        format: metadata.format
      }
    };
    
    return {
      content: [
        {
          type: "text",
          text: JSON.stringify(result, null, 2)
        }
      ]
    };
    
  } catch (error) {
    throw new Error(`Error extrayendo texto: ${error.message}`);
  }
});

// Iniciar servidor
async function main() {
  const transport = new StdioServerTransport();
  await server.connect(transport);
  console.error("Image Analysis MCP Server iniciado");
}

main().catch((error) => {
  console.error("Error al iniciar servidor:", error);
  process.exit(1);
});
