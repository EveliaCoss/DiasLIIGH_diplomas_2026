# Generador de diplomas
# Codigo original de : Dr. Israel Aguilar
# Modificado por la Dra. Evelia Coss
# Fecha: 03 de febrero, 2026

####
# ----- Cargar paquetes ----------
####
pacman::p_load( "tidyverse", "dplyr",
                "rmarkdown", "stringr", "here", "stringi", "googlesheets4")

set_here() # Acomodar la ruta en la misma carpeta donde nos localizamos
# En mi caso: /Users/ecoss/Documents/DiasLIIGH_diplomas_2026

####
# ----- Diplomas de Comite organizador, ponentes y evaluadores ----------
####

# ---- 1. Cargar datos -------
# Cargar funcion certificate
source("functions/certificate.R")
# Guardar liga en una variable
url <- "https://docs.google.com/spreadsheets/d/1thK7VWzXahu7VEAjKHN_NUJKi8oTOO5rOlcqs4VmIWM/edit?usp=sharing" 

# Leer la hoja "Organizadores" desde Google Sheets
# data.df contiene las columnas: Nombre, Email, Participacion, filePDF
data_organizadores <- read_sheet(url, sheet = "Organizadores") %>%
  # Normalizar caracteres en la columna Nombre (acentos, ñ, etc. → ASCII)
  # mutate(Nombre = stri_trans_general(str = Nombre, id = "Latin-ASCII")) %>%
  mutate(Nombre = stringi::stri_trans_nfc(Nombre)) %>%
  # Ordenar el data frame por la columna Email
  arrange(Email) %>%
  # Crear una nueva columna filePDF con la ruta y nombre de archivo PDF
  # Formato: "PDF/<número_de_fila>_<Email>.pdf"
  # mutate(filePDF = str_c(row_number(), "_",
  #                       str_replace_all(Email, fixed(" "), "_"), ".pdf")) %>%
  # Crear una nueva columna filePDF con el nombre de la persona 
  # Ejemplo: "Evelia Lorena Coss Navarrete" → "Evelia_Lorena_Coss_Navarrete.pdf" 
  mutate(filePDF = str_c(str_replace_all(Nombre, " ", "_"), ".pdf")) %>%  
  # Si Nombre está vacío (" "), reemplazarlo por el Email
  mutate(Nombre = ifelse(test = Nombre == " ",
                         yes = Email,
                         no = Nombre)) %>%
  # Eliminar la columna Email (ya no se necesita en el resultado final)
  select(-Email)

# Ponencias
data_poster <- read_sheet(url, sheet = "Poster_oral") %>%
  # Normalizar caracteres en la columna Nombre (acentos, ñ, etc. → ASCII)
  # mutate(Nombre = stri_trans_general(str = Nombre, id = "Latin-ASCII")) %>%
  mutate(Nombre = stringi::stri_trans_nfc(Nombre)) %>%
  # Ordenar el data frame por la columna Email
  arrange(Email) %>%
  # Crear una nueva columna filePDF con la ruta y nombre de archivo PDF
  # Formato: "PDF/<número_de_fila>_<Email>.pdf"
  # mutate(filePDF = str_c(row_number(), "_",
  #                       str_replace_all(Email, fixed(" "), "_"), ".pdf")) %>%
  # Crear una nueva columna filePDF con el nombre de la persona 
  # Ejemplo: "Evelia Lorena Coss Navarrete" → "Evelia_Lorena_Coss_Navarrete.pdf" 
  mutate(filePDF = str_c(str_replace_all(Nombre, " ", "_"), ".pdf")) %>%  
  # Si Nombre está vacío (" "), reemplazarlo por el Email
  mutate(Nombre = ifelse(test = Nombre == " ",
                         yes = Email,
                         no = Nombre)) %>%
  # Eliminar la columna Email (ya no se necesita en el resultado final)
  select(-Email)

# Unir
data.df <- rbind(data_organizadores, data_poster)

# ---- 2. Eliminar carpetas pre-existentes y generar una nueva -------

# Obtener todas las divisiones únicas
divisiones <- unique(data.df$Participacion)

# Crear/limpiar subcarpetas dentro de "participacion"
for (div in divisiones) {
  # Convertir espacios en guiones bajos para nombres válidos
  carpeta <- here("participacion", str_replace_all(div, " ", "_"))
  
  # Si la carpeta existe, eliminarla con todo su contenido
  unlink(carpeta, recursive = TRUE)
  
  # Crear la carpeta nuevamente
  dir.create(path = carpeta, showWarnings = FALSE, recursive = TRUE)
}

# Generar certificados en la carpeta correspondiente
for (i in seq_len(nrow(data.df))) {
  division <- str_replace_all(data.df$Participacion[i], " ", "_")
  outPath <- here("participacion", division, data.df$filePDF[i])
  
  certificate(template = here("participacion/template_participacion.txt"),
              attendeeName = data.df$Nombre[i],
              participation = data.df$Participacion[i],
              outPDF = outPath,
              knitDir = here())
}

# ---- 3. Bucle para renderizar diplomas -------

# Iterar sobre todas las filas del data frame data.df
for (i in seq_len(nrow(data.df))) {
  # Usar la función certificate() para generar un PDF por cada fila
  # Se pasa como parámetros:
  # - La plantilla Rmd con los marcadores de posición
  # - El nombre del asistente (columna Nombre)
  # - La descripción de participación (columna Participacion)
  # - La ruta de salida del PDF (columna filePDF)
  # - El directorio de trabajo para knitDir
  with(data.df,
       certificate(template = here("participacion/template_participacion.txt"),
                   attendeeName = Nombre[i], 
                   participation = Participacion[i],
                   outPDF = filePDF[i],
                   knitDir = here())
  )
}


####
# ---- Asistentes -----
####

# ---- 1. Cargar datos -------
data_asistentes <- read_sheet(url, sheet = "Asistentes") %>%
  mutate(
    # Unir nombre(s) + apellidos en una sola columna Nombre
    Nombre = str_c(Nombres, Apellido_paterno, Apellido_materno, sep = " "),
    # Normalizar acentos y ñ
    Nombre = stringi::stri_trans_nfc(Nombre),
    # Crear nombre de archivo PDF con guiones bajos
    filePDF = str_c(str_replace_all(Nombre, " ", "_"), ".pdf"))  %>%  # Orden comenzando la tabla con Nombre
  dplyr::select(Nombre, everything()) %>%  # opcional: poner Nombre al inicio
  dplyr::select(-Nombres, -Apellido_paterno, -Apellido_materno)

# ---- 2. Eliminar carpetas pre-existentes y generar una nueva -------

carpeta <- here("participacion", "Asistentes")
unlink(carpeta, recursive = TRUE)
dir.create(carpeta, showWarnings = FALSE, recursive = TRUE)

# Eliminar NA
data_asistentes <- data_asistentes[!is.na(data_asistentes$filePDF), ]

for (i in seq_len(nrow(data_asistentes))) {
  # Usar la función certificate() para generar un PDF por cada fila
  # Se pasa como parámetros:
  # - La plantilla Rmd con los marcadores de posición
  # - El nombre del asistente (columna Nombre)
  # - La descripción de participación (columna Participacion)
  # - La ruta de salida del PDF (columna filePDF)
  # - El directorio de trabajo para knitDir
  with(data_asistentes,
       certificate(template = here("participacion/template_participacion.txt"),
                   attendeeName = Nombre[i], 
                   participation = Participacion[i],
                   outPDF = paste0(here("participacion", "Asistentes"),"/", filePDF[i]),
                   knitDir = here() # Archivos temporales
  ))
}


###
# --- Extras ----
###

# ---- 1. Cargar datos -------
data_extras <- read_sheet(url, sheet = "extras") %>%
  mutate(
    # Normalizar acentos y ñ
    Nombre = stringi::stri_trans_nfc(Nombre),
    # Crear nombre de archivo PDF con guiones bajos
    filePDF = str_c(str_replace_all(Nombre, " ", "_"), ".pdf"))  %>%  # Orden comenzando la tabla con Nombre
  dplyr::select(Nombre, everything())

# ---- 2. Eliminar carpetas pre-existentes y generar una nueva -------

carpeta <- here("participacion", "extras")
unlink(carpeta, recursive = TRUE)
dir.create(carpeta, showWarnings = FALSE, recursive = TRUE)

for (i in seq_len(nrow(data_extras))) {
  # Usar la función certificate() para generar un PDF por cada fila
  # Se pasa como parámetros:
  # - La plantilla Rmd con los marcadores de posición
  # - El nombre del asistente (columna Nombre)
  # - La descripción de participación (columna Participacion)
  # - La ruta de salida del PDF (columna filePDF)
  # - El directorio de trabajo para knitDir
  with(data_extras,
       certificate(template = here("participacion/template_participacion.txt"),
                   attendeeName = Nombre[i], 
                   participation = Participacion[i],
                   outPDF = paste0(here("participacion", "extras"),"/", filePDF[i]),
                   knitDir = here() # Archivos temporales
       ))
}

# Eliminar archivos de log (.log) generados durante la compilación
list.files(path = ".", pattern = "\\.log") %>%
  file.remove()

# Eliminar archivos temporales de R Markdown (.Rmd) generados durante la compilación
list.files(path = ".", pattern = "\\.Rmd") %>%
  file.remove()

# PDF
list.files(path = ".", pattern = "\\.pdf") %>%
  file.remove()
