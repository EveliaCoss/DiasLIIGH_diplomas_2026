#' Generate personalized PDF certificates from an R Markdown template
#'
#' This function takes an R Markdown template containing placeholders
#' (e.g., `<<ATTENDEE_NAME>>`, `<<PARTICIPATION>>`, `<<PLACE>>`,
#' `<<TYPE_PARTICIPATION>>`, `<<CATEGORY>>`), replaces them with
#' attendee information, and renders a personalized PDF certificate.
#'
#' @param template Character string. Path to the R Markdown template file
#'   containing placeholders for attendee information.
#' @param attendeeName Character string. Name of the attendee or winner.
#' @param participation Character string. Description of the attendee's
#'   participation (e.g., "Comité organizador", "Ponente oral").
#'   Optional, defaults to `NULL`.
#' @param place Character string. Award placement (e.g., "Primer lugar").
#'   Optional, defaults to `NULL`.
#' @param typeParticipation Character string. Type of participation
#'   (e.g., "Presentación oral", "Presentación de cartel").
#'   Optional, defaults to `NULL`.
#' @param category Character string. Category of the award
#'   (e.g., "Licenciatura", "Posgrado").
#'   Optional, defaults to `NULL`.
#' @param outPDF Character string. Path (relative or absolute) to the output
#'   PDF file to be generated.
#' @param knitDir Character string. Directory where the temporary `.Rmd` file
#'   will be created for rendering. Must be a valid writable path.
#'
#' @details
#' The function works by:
#' \enumerate{
#'   \item Reading the R Markdown template file.
#'   \item Replacing placeholders with attendee information.
#'   \item Writing a temporary `.Rmd` file in the specified directory.
#'   \item Rendering the `.Rmd` file into a PDF using \code{rmarkdown::render}.
#'   \item Deleting the temporary `.Rmd` file after rendering.
#' }
#'
#' The output PDF will be saved at the location specified by \code{outPDF}.
#' The function prints progress messages to the console.
#'
#' @return Invisibly returns the path to the generated PDF file.
#'
#' @examples
#' \dontrun{
#' # Diploma de participación
#' certificate(
#'   template = "templates/participacion/template_participacion.txt",
#'   attendeeName = "Jane Doe",
#'   participation = "Ponente de charla oral",
#'   outPDF = "participacion/ponentes_oral/jane_doe.pdf",
#'   knitDir = tempdir()
#' )
#'
#' # Diploma de ganadores
#' certificate(
#'   template = "templates/ganadores/template_ganadores.txt",
#'   attendeeName = "John Smith",
#'   place = "Primer lugar",
#'   typeParticipation = "Presentación de cartel",
#'   category = "Posgrado",
#'   outPDF = "ganadores/cartel_posgrado/john_smith.pdf",
#'   knitDir = tempdir()
#' )
#' }
#'
#' @export
certificate <- function(template, attendeeName, participation = NULL,
                        place = NULL, typeParticipation = NULL, category = NULL,
                        outPDF, knitDir){
  
  cat("\n Starting:", outPDF, "\n")
  
  # Leer el archivo de plantilla
  templateCert <- read_file(template)
  
  # Reemplazar los placeholders con la información correspondiente
  tmpRmd <- templateCert %>%
    str_replace("<<ATTENDEE_NAME>>", attendeeName) %>%
    str_replace("<<PARTICIPATION>>", participation %||% "") %>%
    str_replace("<<PLACE>>", place %||% "") %>%
    str_replace("<<TYPE_PARTICIPATION>>", typeParticipation %||% "") %>%
    str_replace("<<CATEGORY>>", category %||% "")
  
  # Crear archivo temporal .Rmd
  RmdFile <- tempfile(tmpdir = knitDir, fileext = ".Rmd")
  write_file(tmpRmd, RmdFile)
  
  # Renderizar el certificado en PDF
  # rmarkdown::render(RmdFile, output_file = here(outPDF), quiet = TRUE)
  rmarkdown::render(RmdFile, output_file = outPDF, quiet = TRUE)
  
  
  # Eliminar archivo temporal
  file.remove(RmdFile)
  
  cat("\n Finished:", outPDF, "\n")
  
  invisible(outPDF)
}
