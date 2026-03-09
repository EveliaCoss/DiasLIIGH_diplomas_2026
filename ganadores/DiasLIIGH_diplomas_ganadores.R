# Generador de diplomas
# Codigo original de : Dr. Israel Aguilar
# Modificado por la Dra. Evelia Coss
# Fecha: 03 de febrero, 2026


# ----- Cargar paquetes ----------
pacman::p_load( "tidyverse", "dplyr",
                "rmarkdown", "stringr", "here", "stringi", "googlesheets4")

set_here() # Acomodar la ruta en la misma carpeta donde nos localizamos
# En mi caso: /Users/ecoss/Documents/DiasLIIGH_diplomas_2026

# ---- 1. Cargar datos -------
# Cargar funcion certificate
source("functions/certificate.R")
# Guardar liga en una variable
# Crear dataframe con las combinaciones
data.df <- expand.grid(
  Participacion = c("Licenciatura, dentro de la categoría", "Posgrado, dentro de la categoría"),
  Lugar = c("1er lugar", "2do lugar", "3er lugar"),
  Tipo = c("Presentación oral, durante los", "Presentación de cartel, durante los"),
  stringsAsFactors = FALSE
)

# Agregar columna de Nombre vacía
data.df$Nombre <- ""

# Crear nombres de archivo PDF para cada diploma
# editar caracteres
data.df$filePDF <- paste0("Diploma_", 
                          gsub(", dentro de la categoría", "", data.df$Participacion), "_",
                          gsub(" ", "_", data.df$Lugar), "_",
                          gsub(", durante los", "", data.df$Tipo), ".pdf")
# Cambiar espacios por guion bajo
data.df$filePDF <- gsub(" ", "_", data.df$filePDF) 

# unir ambas variables en una sola
data.df$ParticipacionTipo <- gsub("\n", " ", paste(data.df$Participacion, data.df$Tipo))

# ---- 2. Eliminar carpetas pre-existentes y generar una nueva -------

carpeta <- here("ganadores", "Diplomas")
unlink(carpeta, recursive = TRUE)
dir.create(carpeta, showWarnings = FALSE, recursive = TRUE)

# ---- 3. Bucle para renderizar diplomas -------
for (i in seq_len(nrow(data.df))) {
  # Generar diplomas sin nombre del asistente
  with(data.df,
       certificate(template = here("ganadores/template_ganadores.txt"),
                   attendeeName = "",   # <-- vacío
                   participation = ParticipacionTipo[i], # <-- aquí usas la columna unida
                   place = Lugar[i],    # si tu data.df tiene columna Lugar
                   # typeParticipation = Tipo[i], # si tu data.df tiene columna Tipo
                   outPDF = paste0(here("ganadores", "Diplomas"),"/", filePDF[i]),
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