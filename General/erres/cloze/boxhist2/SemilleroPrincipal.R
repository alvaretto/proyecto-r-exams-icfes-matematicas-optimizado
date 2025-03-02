library(exams)

qqq <- rep("General/erres/cloze/boxhist2/boxhist2_esp.Rmd", 2) # Usar la ruta completa al archivo
copias <- 1

nombre_sin_extension <- sub("\\.Rmd$", "", qqq)
nombre_arch <- paste0(nombre_sin_extension, "_")

examen01 <- qqq

semilla <- sample(100:1e8, 1)
semilla
#set.seed(semilla)
##set.seed(11001)

# Para Moodle 'copias'
# Para los demás, 'rep'

################################################################################
# set.seed(semilla)
exams2html(examen01,
           svg = TRUE,
           results = "asis") # Evitar cualquier intento de abrir un navegador
################################################################################

set.seed(semilla)
exams2pdf(examen01,
          name = nombre_arch,
          encoding = "UTF-8",
          n = copias,
          template = "solpcielo",
          dir = "salida",
          edir = "General/erres/cloze/boxhist2/") # Usar la ruta correcta al directorio de ejercicios
################################################################################

set.seed(semilla)
exams2moodle(examen01,
             n = copias,
             svg = TRUE,
             name = nombre_arch,
             encoding = "UTF-8",
             dir = "salida",
             edir = "General/erres/cloze/boxhist2/", # Usar la ruta correcta al directorio de ejercicios
             mchoice = list(shuffle = FALSE,
                            answernumbering = "ABCD",
                            eval = list(partial = TRUE,
                                        rule = "none")),
             tdir = "temp_dir") # Establecer un directorio temporal válido
################################################################################

exams2pandoc(examen01, 
             n = copias, 
             nsamp = NULL, 
             dir = "salida",
             name = nombre_arch,
             type = "docx", 
             template = "plain.tex",
             header = list(Date = Sys.Date()), 
             inputs = NULL, 
             options = NULL,
             quiet = TRUE, 
             resolution = 100, 
             width = 4, 
             height = 4, 
             svg = TRUE,
             encoding = "UTF-8", 
             edir = "General/erres/cloze/boxhist2/", # Usar la ruta correcta al directorio de ejercicios
             tdir = "temp_dir", # Establecer un directorio temporal válido
             sdir = NULL,
             verbose = FALSE, 
             points = NULL, 
             exshuffle = NULL)
