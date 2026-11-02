## pdf_pandoc_rstudio.R — Paso 8, ronda 9 (md5 f98f9dda…): regla #20 pide validar el PDF también con el pandoc que empaqueta RStudio.
## Renderiza las 5 semillas de render_cloze.R (7, 3, 2, 1, 4) forzando ESE pandoc (find_pandoc con cache = FALSE; comprobado con
## pandoc_exec()) y les pasa las mismas comprobaciones de PDF que a los 100 del pandoc del PATH. Uso: Rscript --no-init-file pdf_pandoc_rstudio.R
SOLO_DEFINICIONES <- TRUE
source("verificar_formatos.R", encoding = "UTF-8")
PD <- "/usr/lib/rstudio/resources/app/bin/quarto/bin/tools/x86_64"
rmarkdown::find_pandoc(dir = PD, cache = FALSE)
cat("pandoc forzado:", rmarkdown::pandoc_exec(), "· versión", as.character(rmarkdown::pandoc_version()), "\n")
stopifnot("no se forzó el pandoc de RStudio" = identical(normalizePath(rmarkdown::pandoc_exec()), normalizePath(file.path(PD, "pandoc"))))
SEM5 <- c(7L, 3L, 2L, 1L, 4L)
dir.create("pdf_rstudio", showWarnings = FALSE)
exams2pdf(F, n = 5, dir = "pdf_rstudio", name = "cloze", seed = SEM5, texdir = "tex_rstudio")
REG <- REG[0, ]
for (k in seq_along(SEM5)) verificar_pdf(SEM5[k], file.path("pdf_rstudio", sprintf("cloze%d.pdf", k)), fmt = "PDF-RStudio")
cat("PDF con el pandoc de RStudio: comprobaciones", nrow(REG), "· fallidas", sum(!REG$ok), "\n")
if (any(!REG$ok)) print(REG[!REG$ok, c("id", "sem", "det")])
tt <- paste(unlist(lapply(list.files("tex_rstudio", pattern = "\\.tex$", full.names = TRUE), readLines, warn = FALSE)), collapse = "\n")
cat(sprintf("tex (RStudio): \\LTcaptype{none}: %d · guarda c@none: %s · longtable: %d\n", lengths(regmatches(tt, gregexpr("LTcaptype{none}", tt, fixed = TRUE))),
            grepl("@ifundefined{c@none}", tt, fixed = TRUE), lengths(regmatches(tt, gregexpr("begin{longtable}", tt, fixed = TRUE)))))
