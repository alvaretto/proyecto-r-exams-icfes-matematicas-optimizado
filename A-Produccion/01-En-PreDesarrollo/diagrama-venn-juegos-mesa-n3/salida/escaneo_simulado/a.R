suppressMessages({library(exams); library(png)})
set.seed(11)
ex <- exams2nops(rep("item.Rmd", 3), n = 1, name = "h", dir = ".", language = "es", title = "Prueba", institution = "I. E.", logo = NULL,
                 date = "2026-11-06", blank = 0, duplex = TRUE, twocolumn = TRUE, samepage = TRUE, header = "\\raggedbottom", encoding = "UTF-8", quiet = TRUE)
system2("pdfjam", c("--papersize", shQuote("{216mm,330mm}"), "--noautoscale", "true", "--quiet", "h1.pdf", "-o", "h1_oficio.pdf"))
system2("pdftoppm", c("-r", "300", "-f", "1", "-l", "1", "-gray", "-png", "h1.pdf", "hoja"))
system2("pdftoppm", c("-r", "300", "-f", "1", "-l", "1", "-gray", "-png", "h1_oficio.pdf", "oficio"))
f <- exams:::trim_nops_scan; b <- body(f); b[[length(b)]] <- quote(c(rt = rt, rb = rb, cl = cl, cr = cr)); body(f) <- b
m <- f(list.files(".", "^hoja.*png$")[1]); print(m)
x <- png::readPNG(list.files(".", "^hoja.*png$")[1]); print(dim(x))
writeLines(as.character(m), "marco.txt")
