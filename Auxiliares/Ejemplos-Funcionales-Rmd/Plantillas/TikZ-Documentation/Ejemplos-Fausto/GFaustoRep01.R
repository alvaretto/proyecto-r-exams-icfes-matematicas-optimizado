## exams ----------------------------------------------------------------------------
setwd("~/Documentos/NuevoRStudio/Rmarkdowns/Esqueleto/exercises/schoice/CuerposGeometricos/Fausto")
##load package
library(exams)
library(tinytex)
library(tikzDevice)
##library(magick)
## exam with a simple vector of exercises in R/Markdown (.Rmd) format
## -> alternatively try a list of vectors of more exercises
examen01<-c("FaustoRepositorio01.Rmd")
## note that the currency exercise is in UTF-8 encoding

semilla<-sample(100:1e8, 1)
semilla
set.seed(semilla) 
##set.seed(11001) 

## Generate Moodle exam with three replications per question

exams2pdf(examen01,name="FaustoRepositorio01_",encoding="UTF-8",n=1,template=("nuevotaller"),dir="salida",edir="ejercicios")
#set.seed(semilla) 
exams2pdf(examen01,name="Respuestas_FaustoRepositorio01_",encoding="UTF-8",n=1,template=c("soluciontallerespc"),dir="salida",edir="ejercicios")
#set.seed(semilla) 
exams2html("FaustoRepositorio01.Rmd",svg=TRUE)
##set.seed(semilla) 
##exams2pandoc(examen01, n = 1, dir = "salida", type = "docx")
##set.seed(semilla)
##exams2pandoc(examen01, n = 1, dir = "salida", type = "odt")

##exams2moodle(examen01,n=1,name="DVennDef",encoding="UTF-8",dir="salida",edir="ejercicios",mchoice = list(shuffle = TRUE, answernumbering = "none", eval = list(partial = TRUE, rule = "none")))

##exams2moodle(examen01,n=2,svg=TRUE,name="DVennDef",encoding="UTF-8",dir="salida",edir="ejercicios",mchoice = list(shuffle = TRUE, answernumbering = "none", eval = list(partial = TRUE, rule = "none")))


## hint: to quickly check (prior to Moodle export) whether each exercise can be
## converted to HTML, exams2html() can be used
## exams2html("exercises/deriv.Rmd")

## ----------------------------------------------------------------------------------